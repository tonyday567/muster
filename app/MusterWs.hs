{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | WebSocket (+ HTTP) face on a muster channel.
--
-- Serves deck.html, /api/state (board + agents), /channels, and WS bus duplex.
--
-- @
--   cabal run muster-ws -- --name deck
--   open http://127.0.0.1:9162/
-- @
module Main where

import Muster.Channel
  ( Channel,
    ChannelConfig (..),
    channelAttach,
    channelRecvRaw,
    channelSend,
    defaultChannelConfig,
  )
import Muster.Cli.Opts
  ( boardOpt,
    busRootOpt,
    channelOpt,
    devOpt,
    devPathOpt,
    historyOpt,
    hostOpt,
    nameOpt,
    portOpt,
    resolveBusRoot,
  )
import Muster.Framing qualified as Framing
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, waitEither)
import Control.Concurrent.STM
import Control.Exception (IOException, finally, try)
import Control.Monad (filterM, forever, void, when)
import Data.ByteString.Lazy qualified as LBS
import Data.FileEmbed (embedFile)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as TIO
import Network.HTTP.Types (parseQuery, status200, status400, status404)
import Network.Wai
  ( Application,
    Request,
    Response,
    pathInfo,
    rawQueryString,
    responseLBS,
  )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
  ( Connection,
    PendingConnection (..),
    RequestHead (..),
    ServerApp,
    acceptRequest,
    defaultConnectionOptions,
    receiveData,
    sendTextData,
  )
import Options.Applicative
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, stdout)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Process (createProcess, proc)
import System.Posix.Types (CPid (..))
import Prelude

-- | Deck HTML embedded at compile time so the binary is self-contained.
deckHtml :: LBS.ByteString
deckHtml = LBS.fromStrict $(embedFile "app/deck.html")

data Opts = Opts
  { optName :: String,
    optChannel :: String,
    optBusRoot :: FilePath,
    optHost :: String,
    optPort :: Int,
    optHistory :: Int,
    optBoard :: FilePath,
    optDev :: Bool,
    optDevPath :: FilePath
  }

-- | Same channel / name / bus-root vocabulary as @muster@ CLI ('Muster.Cli.Opts').
optsP :: Parser Opts
optsP =
  Opts
    <$> nameOpt "deck"
    <*> channelOpt
    <*> busRootOpt
    <*> hostOpt
    <*> portOpt 9162
    <*> historyOpt
    <*> boardOpt
    <*> devOpt
    <*> devPathOpt

-- ---------------------------------------------------------------------------
-- Channel cache — so we can switch channels without restart
-- ---------------------------------------------------------------------------

data ChannelEntry = ChannelEntry
  { ceChannel :: Channel,
    ceFan :: TChan Text,
    cePump :: Async ()
  }

-- | A cache of active channels, keyed by channel name.
type ChannelCache = IORef (Map.Map String ChannelEntry)

-- | Get or create a channel by name.  Returns the Channel handle and its
-- broadcast TChan for fan-out.
getOrCreateChannel :: FilePath -> String -> Text -> ChannelCache -> IO (Channel, TChan Text)
getOrCreateChannel root chanName identity cache = do
  m <- readIORef cache
  case Map.lookup chanName m of
    Just entry -> pure (ceChannel entry, ceFan entry)
    Nothing -> do
      let cfg =
            (defaultChannelConfig identity)
              { chChannel = chanName,
                chBusRoot = root
              }
      ch <- channelAttach cfg
      fan <- newBroadcastTChanIO
      pump <- async $ forever $ do
        lines' <- channelRecvRaw ch
        mapM_ (atomically . writeTChan fan) lines'
        when (null lines') $ threadDelay 50_000
      let entry = ChannelEntry ch fan pump
      atomicModifyIORef' cache $ \m' -> (Map.insert chanName entry m', ())
      pure (ch, fan)

main :: IO ()
main = do
  o <- execParser $ info (optsP <**> helper) (progDesc "HTTP+WS muster deck")
  root <- resolveBusRoot (optBusRoot o)
  boardPath <- resolveBoard (optBoard o)
  let dir = root </> optChannel o
      fifo = dir </> "bus.fifo"
      logf = dir </> "log.md"
      agentsDir = root </> "agents"
  ok <- doesFileExist fifo
  when (not ok) $
    error $
      "bus.fifo missing at " <> fifo <> " — run: muster bus start (channel " <> optChannel o <> ")"

  let name = T.pack (optName o)

  putStrLn $ "muster-ws: attaching as " <> optName o <> " on " <> optChannel o
  putStrLn $ "  bus-root " <> root
  putStrLn $ "  log      " <> logf
  putStrLn $ "  board    " <> boardPath
  putStrLn $ "  http     http://" <> optHost o <> ":" <> show (optPort o) <> "/"
  hFlush stdout

  -- Pre-create the default channel so it's ready immediately.
  channelCache <- newIORef Map.empty

  -- Pre-attach to the default channel.
  _ <- getOrCreateChannel root (optChannel o) name channelCache

  let wsApp = clientApp root name (optChannel o) channelCache (optHistory o)
      httpApp =
        staticApp
          root
          boardPath
          agentsDir
          (T.pack (optChannel o))
          name
          (optDev o)
          (optDevPath o)
      app = websocketsOr defaultConnectionOptions wsApp httpApp
  run (optPort o) app

resolveBoard :: FilePath -> IO FilePath
resolveBoard explicit
  | not (null explicit) = pure explicit
  | otherwise = (</> "mg/loom/board.md") <$> getEnv "HOME"

-- | Extract the channel name from the WebSocket request's query string.
-- Returns the default channel if none specified.
extractChannelFromWs :: PendingConnection -> String -> String
extractChannelFromWs pending defaultChannel =
  let path = requestPath (pendingRequest pending)
      -- requestPath includes the query string. Parse '?channel=NAME'.
      pathText = decodeUtf8 path
      chan = case T.breakOn "?channel=" pathText of
        (_, rest) | T.null rest -> defaultChannel
        (_, rest) ->
          let val = T.takeWhile (/= '&') (T.drop 9 rest)  -- drop "?channel="
          in if T.null val then defaultChannel else T.unpack val
  in chan

staticApp :: FilePath -> FilePath -> FilePath -> Text -> Text -> Bool -> FilePath -> Application
staticApp root boardPath agentsDir defaultChannel identity dev devPath req respond =
  case pathInfo req of
    [] -> serveHtml dev devPath req defaultChannel identity root respond
    ["api", "state"] -> do
      -- Read channel from query param for state endpoint too
      let qs = parseQuery (rawQueryString req)
          chan = case lookup "channel" qs of
            Just (Just c) -> decodeUtf8 c
            _ -> defaultChannel
          chanDir = root </> T.unpack chan
          chanLogf = chanDir </> "log.md"
      body <- buildStateJson boardPath agentsDir chanLogf chan identity
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
    ["api", "open"] -> do
      let loomDir = takeDirectory boardPath
          qs = parseQuery (rawQueryString req)
          mpath = case lookup "path" qs of
            Just (Just p) -> Just (loomDir </> T.unpack (decodeUtf8 p))
            _ -> Nothing
      case mpath of
        Just fullPath -> do
          void $ try @IOException $ createProcess (proc "open" [fullPath])
          respond $ responseLBS status200 [("Content-Type", "text/plain")] "ok"
        Nothing ->
          respond $ responseLBS status400 [("Content-Type", "text/plain")] "missing path"
    ["channels"] -> do
      dirs <- listChannels root
      let json =
            "[" <> T.intercalate "," (map jsonStr (map T.pack dirs)) <> "]"
      respond $
        responseLBS
          status200
          [("Content-Type", "application/json; charset=utf-8"), ("Cache-Control", "no-store")]
          (LBS.fromStrict (encodeUtf8 json))
    _ ->
      respond $
        responseLBS status404 [("Content-Type", "text/plain")] "not found"

-- | List available channel directories under the bus root.
listChannels :: FilePath -> IO [String]
listChannels root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      ents <- listDirectory root
      let isChannelDir name = do
            let full = root </> name
                tname = T.pack name
            isDir <- doesDirectoryExist full
            pure (isDir && name /= "." && name /= ".." && name /= "agents" && not ("_" `T.isPrefixOf` tname) && not ("." `T.isPrefixOf` tname))
      filterM isChannelDir ents

serveHtml :: Bool -> FilePath -> Request -> Text -> Text -> FilePath -> (Response -> IO a) -> IO a
serveHtml dev devPath req defaultChannel identity busRootPath respond = do
  html <-
    if not dev
      then pure deckHtml
      else do
        res <- try @IOException (LBS.readFile devPath)
        case res of
          Left err -> do
            putStrLn $ "muster-ws: dev mode failed to read " <> devPath <> ": " <> show err
            pure deckHtml
          Right bytes -> pure bytes
  -- Same three knobs as optparse globals, injected for the page.
  let qs = parseQuery (rawQueryString req)
      chan = case lookup "channel" qs of
        Just (Just c) -> decodeUtf8 c
        _ -> defaultChannel
      escape t = T.replace "\\" "\\\\" $ T.replace "\"" "\\\"" t
      injected =
        LBS.fromStrict
          ( encodeUtf8 $
              T.concat
                [ "<script>",
                  "window.DEFAULT_CHANNEL=\"",
                  escape chan,
                  "\";",
                  "window.DEFAULT_NAME=\"",
                  escape identity,
                  "\";",
                  "window.DEFAULT_BUS_ROOT=\"",
                  escape (T.pack busRootPath),
                  "\";",
                  "</script>\n"
                ]
          )
          <> html
  respond $
    responseLBS
      status200
      [ ("Content-Type", "text/html; charset=utf-8"),
        ("Cache-Control", "no-store, no-cache, must-revalidate")
      ]
      injected

buildStateJson :: FilePath -> FilePath -> FilePath -> Text -> Text -> IO Text
buildStateJson boardPath agentsDir logf channel identity = do
  board <- readFileUtf8 boardPath
  agents <- listAgents agentsDir
  lastBy <- lastLinesBySender logf 30  -- recent posters only
  msession <- readSessionInfo (takeDirectory logf)
  let hermesRow = agentJson (AgentInfo "hermes" "active" Nothing "") (Map.lookup "hermes" lastBy)
      allAgentRows = hermesRow : [ agentJson a (Map.lookup (agName a) lastBy) | a <- agents, agStatus a /= "down" ]
      projects = projectLines board
      -- bus participants: names that posted in the recent log tail
      busNames = Map.keys lastBy
      sessionJson = case msession of
        Nothing -> "null"
        Just s ->
          T.concat
            [ "{\"status\":"
            , jsonStr (siStatus s)
            , ",\"opened\":"
            , jsonStr (siOpened s)
            , ",\"participants\":["
            , T.intercalate "," (map jsonStr (siParticipants s))
            , "],\"logStart\":"
            , T.pack (show (siLogStart s))
            , "}"
            ]
  pure $
    T.concat
      [ "{\"channel\":",
        jsonStr channel,
        ",\"identity\":",
        jsonStr identity,
        ",\"boardPath\":",
        jsonStr (T.pack boardPath),
        ",\"board\":",
        jsonStr board,
        ",\"projects\":[",
        T.intercalate "," (map jsonStr projects),
        "],\"agents\":[",
        T.intercalate "," allAgentRows,
        "],\"bus\":[",
        T.intercalate "," (map jsonStr busNames),
        "],\"session\":",
        sessionJson,
        "}"
      ]

data SessionInfo = SessionInfo
  { siStatus :: Text,
    siOpened :: Text,
    siParticipants :: [Text],
    siLogStart :: Int
  }

readSessionInfo :: FilePath -> IO (Maybe SessionInfo)
readSessionInfo dir = do
  let path = dir </> "session.md"
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      raw <- TIO.readFile path
      let kv = map (T.breakOn ":") (T.lines raw)
          lookupKey k =
            listToMaybe
              [ T.strip (T.drop 1 v)
                | (k', v) <- kv,
                  T.strip k' == k
              ]
          participants =
            maybe
              []
              (filter (not . T.null) . map T.strip . T.words)
              (lookupKey "participants")
          logStart =
            maybe
              0
              (\v -> case reads (T.unpack (T.strip v)) of [(n, _)] -> n; _ -> 0)
              (lookupKey "log-start")
      case lookupKey "status" of
        Nothing -> pure Nothing
        Just st ->
          pure $
            Just
              SessionInfo
                { siStatus = st,
                  siOpened = fromMaybe "" (lookupKey "opened"),
                  siParticipants = participants,
                  siLogStart = logStart
                }

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
            case Framing.parseMessage line of
              Nothing -> m
              Just (sender, _) -> Map.insert sender line m
      pure $ foldl' step Map.empty ls

projectLines :: Text -> [Text]
projectLines board =
  [ T.strip l
    | l <- T.lines board,
      let s = T.strip l,
      T.isInfixOf "bus-deck" s
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

-- | Read the log for a channel (not necessarily the default one).
readHistory :: FilePath -> Int -> IO [Text]
readHistory path n = do
  exists <- doesFileExist path
  if not exists || n <= 0
    then pure []
    else do
      raw <- TIO.readFile path
      let ls = filter (not . T.null) $ T.lines raw
      pure $ drop (max 0 (length ls - n)) ls

-- ---------------------------------------------------------------------------
-- WebSocket handling — now channel-aware
-- ---------------------------------------------------------------------------

clientApp :: FilePath -> Text -> String -> ChannelCache -> Int -> ServerApp
clientApp root identity defaultChannel cache hist pending = do
  conn <- acceptRequest pending
  -- WS query ?channel=NAME; fall back to the process default channel (not the nick).
  let chan = extractChannelFromWs pending defaultChannel
  -- Attach as the deck identity on every channel so posts show as "deck", not the channel name.
  (ch, fan) <- getOrCreateChannel root chan identity cache
  let logf = root </> chan </> "log.md"
  clientSession ch fan logf hist conn

clientSession :: Channel -> TChan Text -> FilePath -> Int -> Connection -> IO ()
clientSession ch fan logf hist conn = do
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
