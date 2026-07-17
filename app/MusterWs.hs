{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | WebSocket (+ HTTP) face on a muster channel.
--
-- Serves deck.html, /api/state (board + agents), and WS bus duplex.
--
-- @
--   cabal run muster-ws -- --name desk
--   open http://127.0.0.1:9162/
-- @
module Main where

import Circuit.Comm
  ( Channel,
    ChannelConfig (..),
    channelAttach,
    channelClose,
    channelRecv,
    channelSend,
    frameMessage,
  )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, waitEither)
import Control.Concurrent.STM
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (forever, void, when)
import Data.ByteString.Lazy qualified as LBS
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Network.HTTP.Types (status200, status404)
import Network.Wai
  ( Application,
    Response,
    pathInfo,
    responseFile,
    responseLBS,
  )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
  ( Connection,
    ServerApp,
    acceptRequest,
    defaultConnectionOptions,
    receiveData,
    sendTextData,
  )
import Options.Applicative
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (CPid (..))
import Prelude

data Opts = Opts
  { optName :: String,
    optChannel :: String,
    optBusRoot :: FilePath,
    optHost :: String,
    optPort :: Int,
    optHistory :: Int,
    optHtml :: FilePath,
    optBoard :: FilePath
  }

optsP :: Parser Opts
optsP =
  Opts
    <$> strOption (long "name" <> short 'n' <> value "desk" <> showDefault <> help "Bus identity")
    <*> strOption (long "channel" <> short 'c' <> value "general" <> showDefault)
    <*> strOption (long "bus-root" <> value "" <> help "Default: $HOME/mg/logs/muster")
    <*> strOption (long "host" <> value "127.0.0.1" <> showDefault)
    <*> option auto (long "port" <> short 'p' <> value 9162 <> showDefault)
    <*> option auto (long "history" <> value 80 <> showDefault <> help "Log lines on WS connect")
    <*> strOption (long "html" <> value "app/deck.html" <> showDefault)
    <*> strOption (long "board" <> value "" <> help "Default: $HOME/mg/loom/board.md")

main :: IO ()
main = do
  o <- execParser $ info (optsP <**> helper) (progDesc "HTTP+WS muster desk")
  root <- resolveRoot (optBusRoot o)
  boardPath <- resolveBoard (optBoard o)
  let dir = root </> optChannel o
      fifo = dir </> "bus.fifo"
      logf = dir </> "log.md"
      agentsDir = root </> "agents"
  ok <- doesFileExist fifo
  when (not ok) $
    error $
      "bus.fifo missing at " <> fifo <> " — run: muster bus start -c " <> optChannel o

  let name = T.pack (optName o)
      cfg =
        ChannelConfig
          { chStdinPath = fifo,
            chStdoutPath = logf,
            chStderrPath = dir </> "err.md",
            chName = name,
            chWorkingDir = dir
          }

  putStrLn $ "muster-ws: attaching as " <> optName o <> " on " <> optChannel o
  putStrLn $ "  log   " <> logf
  putStrLn $ "  board " <> boardPath
  putStrLn $ "  http  http://" <> optHost o <> ":" <> show (optPort o) <> "/"
  hFlush stdout

  bracket (channelAttach cfg) channelClose $ \ch -> do
    busFan <- newBroadcastTChanIO
    void $ async $ busPump ch busFan
    let wsApp = clientApp ch name busFan logf (optHistory o)
        httpApp = staticApp (optHtml o) boardPath agentsDir logf
        app = websocketsOr defaultConnectionOptions wsApp httpApp
    run (optPort o) app

resolveRoot :: FilePath -> IO FilePath
resolveRoot explicit
  | not (null explicit) = pure explicit
  | otherwise = do
      menv <- lookupEnv "MUSTER_ROOT"
      case menv of
        Just d | not (null d) -> pure d
        _ -> (</> "mg/logs/muster") <$> getEnv "HOME"

resolveBoard :: FilePath -> IO FilePath
resolveBoard explicit
  | not (null explicit) = pure explicit
  | otherwise = (</> "mg/loom/board.md") <$> getEnv "HOME"

busPump :: Channel -> TChan Text -> IO ()
busPump ch fan = forever $ do
  msgs <- channelRecv ch
  mapM_
    ( \(sender, body) ->
        atomically $ writeTChan fan (frameMessage sender body)
    )
    msgs
  when (null msgs) $ threadDelay 50_000

staticApp :: FilePath -> FilePath -> FilePath -> FilePath -> Application
staticApp htmlPath boardPath agentsDir logf req respond =
  case pathInfo req of
    [] -> serveHtml htmlPath respond
    ["api", "state"] -> do
      body <- buildStateJson boardPath agentsDir logf
      respond $
        responseLBS
          status200
          [ ("Content-Type", "application/json; charset=utf-8"),
            ("Cache-Control", "no-store")
          ]
          (LBS.fromStrict (encodeUtf8 body))
    ["api", "board"] -> do
      t <- readFileUtf8 boardPath
      respond $
        responseLBS
          status200
          [("Content-Type", "text/plain; charset=utf-8"), ("Cache-Control", "no-store")]
          (LBS.fromStrict (encodeUtf8 t))
    _ ->
      respond $
        responseLBS status404 [("Content-Type", "text/plain")] "not found"

serveHtml :: FilePath -> (Response -> IO a) -> IO a
serveHtml htmlPath respond = do
  exists <- doesFileExist htmlPath
  if exists
    then
      respond $
        responseFile
          status200
          [("Content-Type", "text/html; charset=utf-8")]
          htmlPath
          Nothing
    else
      respond $
        responseLBS status404 [("Content-Type", "text/plain")] "deck.html not found"

buildStateJson :: FilePath -> FilePath -> FilePath -> IO Text
buildStateJson boardPath agentsDir logf = do
  board <- readFileUtf8 boardPath
  agents <- listAgents agentsDir
  lastBy <- lastLinesBySender logf 400
  let agentRows =
        [ agentJson a (Map.lookup (agName a) lastBy)
          | a <- agents
        ]
      projects = projectLines board
  pure $
    T.concat
      [ "{\"boardPath\":",
        jsonStr (T.pack boardPath),
        ",\"board\":",
        jsonStr board,
        ",\"projects\":[",
        T.intercalate "," (map jsonStr projects),
        "],\"agents\":[",
        T.intercalate "," agentRows,
        "]}"
      ]

data AgentInfo = AgentInfo
  { agName :: Text,
    agStatus :: Text,
    agPid :: Maybe Int,
    agModel :: Text
  }

listAgents :: FilePath -> IO [AgentInfo]
listAgents dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      ents <- listDirectory dir
      let names = filter (\n -> n /= "_coord" && n /= "." && n /= "..") ents
      mapM (readAgent dir) names

readAgent :: FilePath -> FilePath -> IO AgentInfo
readAgent dir name = do
  let base = dir </> name
      pidPath = base </> "agent.pid"
      cfgPath = base </> "config"
  mpid <- readPid pidPath
  st <- case mpid of
    Nothing -> pure "down"
    Just p -> do
      alive <- pidAlive p
      pure $ if alive then "alive" else "stale"
  model <- readModel cfgPath
  pure $
    AgentInfo
      { agName = T.pack name,
        agStatus = st,
        agPid = mpid,
        agModel = model
      }

readPid :: FilePath -> IO (Maybe Int)
readPid path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      t <- T.strip <$> TIO.readFile path
      pure $
        case reads (T.unpack (T.takeWhile (/= '\n') t)) of
          [(n, _)] -> Just n
          _ -> Nothing

pidAlive :: Int -> IO Bool
pidAlive p = do
  r <- try @IOException $ signalProcess nullSignal (CPid (fromIntegral p))
  pure $ case r of
    Left _ -> False
    Right _ -> True

readModel :: FilePath -> IO Text
readModel path = do
  exists <- doesFileExist path
  if not exists
    then pure ""
    else do
      t <- TIO.readFile path
      let ms =
            [ T.strip (T.drop 1 rest)
              | l <- T.lines t,
                let (k, rest) = T.breakOn "=" l,
                T.strip k == "model",
                T.isPrefixOf "=" rest
            ]
      pure $ case ms of
        (m : _) -> m
        [] -> ""

lastLinesBySender :: FilePath -> Int -> IO (Map.Map Text Text)
lastLinesBySender path n = do
  exists <- doesFileExist path
  if not exists
    then pure Map.empty
    else do
      raw <- TIO.readFile path
      let ls = reverse $ take n $ reverse $ filter (not . T.null) $ T.lines raw
          step m line =
            case parseSender line of
              Nothing -> m
              Just s -> Map.insert s line m
      pure $ foldl' step Map.empty ls

parseSender :: Text -> Maybe Text
parseSender t =
  case T.stripPrefix "[" t of
    Nothing -> Nothing
    Just rest ->
      let (s, _) = T.breakOn "]" rest
       in if T.null s then Nothing else Just s

projectLines :: Text -> [Text]
projectLines board =
  [ T.strip l
    | l <- T.lines board,
      let s = T.strip l,
      T.isPrefixOf "🟣" s
        || T.isPrefixOf "🟢" s
        || T.isPrefixOf "🟡" s
        || T.isPrefixOf "🔵" s
  ]

agentJson :: AgentInfo -> Maybe Text -> Text
agentJson a mlast =
  T.concat
    [ "{\"name\":",
      jsonStr (agName a),
      ",\"status\":",
      jsonStr (agStatus a),
      ",\"pid\":",
      maybe "null" (T.pack . show) (agPid a),
      ",\"model\":",
      jsonStr (agModel a),
      ",\"last\":",
      jsonStr (maybe "" id mlast),
      "}"
    ]

jsonStr :: Text -> Text
jsonStr t =
  T.concat ["\"", T.concatMap esc t, "\""]
  where
    esc c
      | c == '"' = "\\\""
      | c == '\\' = "\\\\"
      | c == '\n' = "\\n"
      | c == '\r' = "\\r"
      | c == '\t' = "\\t"
      | otherwise = T.singleton c

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = do
  exists <- doesFileExist path
  if not exists then pure "" else TIO.readFile path

clientApp :: Channel -> Text -> TChan Text -> FilePath -> Int -> ServerApp
clientApp ch name fan logf hist pending = do
  conn <- acceptRequest pending
  clientSession ch name fan logf hist conn

clientSession :: Channel -> Text -> TChan Text -> FilePath -> Int -> Connection -> IO ()
clientSession ch _name fan logf hist conn = do
  histLines <- readHistory logf hist
  mapM_ (sendTextData conn) histLines
  inQ <- newTQueueIO
  outQ <- newTQueueIO
  myFan <- atomically $ dupTChan fan
  tFan <- async $ forever $ do
    t <- atomically $ readTChan myFan
    atomically $ writeTQueue outQ t
  tPost <- async $ forever $ do
    t <- atomically $ readTQueue inQ
    let body = T.strip t
    when (not (T.null body)) $
      channelSend ch body
  tOut <- async $ forever $ do
    t <- atomically $ readTQueue outQ
    sendTextData conn t
  tIn <- async $ forever $ do
    msg <- receiveData conn
    atomically $ writeTQueue inQ (msg :: Text)
  void $
    finally
      (waitEitherCancel tIn tOut)
      (mapM_ cancel [tFan, tPost, tIn, tOut])

waitEitherCancel :: Async a -> Async b -> IO ()
waitEitherCancel a b = do
  void $ waitEither a b
  cancel a
  cancel b

readHistory :: FilePath -> Int -> IO [Text]
readHistory path n = do
  exists <- doesFileExist path
  if not exists || n <= 0
    then pure []
    else do
      raw <- TIO.readFile path
      let ls = filter (not . T.null) $ T.lines raw
      pure $ drop (max 0 (length ls - n)) ls
