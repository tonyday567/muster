{-# LANGUAGE OverloadedStrings #-}

-- | WebSocket (+ HTTP) face on a muster channel.
--
-- Serves @app/deck.html@ and upgrades connections to WebSocket.
-- Browser text → bus; bus lines (incl. self) → browser.
-- On connect: last N log lines as history.
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
import Control.Exception (bracket, finally)
import Control.Monad (forever, void, when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.HTTP.Types (status200, status404)
import Network.Wai
  ( Application,
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
import System.Directory (doesFileExist)
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import Prelude

data Opts = Opts
  { optName :: String,
    optChannel :: String,
    optBusRoot :: FilePath,
    optHost :: String,
    optPort :: Int,
    optHistory :: Int,
    optHtml :: FilePath
  }

optsP :: Parser Opts
optsP =
  Opts
    <$> strOption
      ( long "name"
          <> short 'n'
          <> value "desk"
          <> showDefault
          <> help "Bus identity for this face"
      )
    <*> strOption
      ( long "channel"
          <> short 'c'
          <> value "general"
          <> showDefault
      )
    <*> strOption
      ( long "bus-root"
          <> value ""
          <> help "Default: $HOME/mg/logs/muster"
      )
    <*> strOption
      ( long "host"
          <> value "127.0.0.1"
          <> showDefault
          <> help "Bind address (HTTP+WS)"
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> value 9162
          <> showDefault
      )
    <*> option
      auto
      ( long "history"
          <> value 80
          <> showDefault
          <> help "Log lines to push on connect"
      )
    <*> strOption
      ( long "html"
          <> value "app/deck.html"
          <> showDefault
          <> help "Path to deck HTML (relative to cwd or absolute)"
      )

main :: IO ()
main = do
  o <- execParser $ info (optsP <**> helper) (progDesc "HTTP+WS face on a muster channel")
  root <- resolveRoot (optBusRoot o)
  let dir = root </> optChannel o
      fifo = dir </> "bus.fifo"
      logf = dir </> "log.md"
  ok <- doesFileExist fifo
  when (not ok) $
    error $
      "bus.fifo missing at " <> fifo <> " — run: muster bus start -c " <> optChannel o

  htmlOk <- doesFileExist (optHtml o)
  when (not htmlOk) $
    putStrLn $ "warning: html not found at " <> optHtml o <> " (HTTP / will 404)"

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
  putStrLn $ "  http  http://" <> optHost o <> ":" <> show (optPort o) <> "/"
  putStrLn $ "  ws    ws://" <> optHost o <> ":" <> show (optPort o) <> "/"
  hFlush stdout

  bracket (channelAttach cfg) channelClose $ \ch -> do
    busFan <- newBroadcastTChanIO
    void $ async $ busPump ch busFan
    let wsApp = clientApp ch name busFan logf (optHistory o)
        httpApp = staticApp (optHtml o)
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

busPump :: Channel -> TChan Text -> IO ()
busPump ch fan = forever $ do
  msgs <- channelRecv ch
  mapM_
    ( \(sender, body) ->
        atomically $ writeTChan fan (frameMessage sender body)
    )
    msgs
  when (null msgs) $ threadDelay 50_000

staticApp :: FilePath -> Application
staticApp htmlPath _req respond = do
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
        responseLBS
          status404
          [("Content-Type", "text/plain; charset=utf-8")]
          "deck.html not found — pass --html PATH"

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
