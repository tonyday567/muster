{-# LANGUAGE OverloadedStrings #-}

-- | Bare-bones WebSocket face on a muster channel.
--
-- 'Circuit.Socket' (circuits-io) + 'attachMusterRepl' (circuits-repl).
-- Browser text → bus commit; bus emit → browser.
--
-- @
--   cabal run muster-ws -- --name desk
--   open app/deck.html
-- @
module Main where

import Circuit.Comm (ChannelConfig (..), attachMusterRepl)
import Circuit.Repl (Repl, replClose, replCommit, replEmit)
import Circuit.Socket
  ( SocketConfig (..),
    defaultSocketConfig,
    wsDuplex,
    wsServerApp,
    withWSServer,
  )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM
import Control.Exception (bracket, finally)
import Control.Monad (forever, void, when)
import Data.Text (Text)
import Data.Text qualified as T
import Network.WebSockets (Connection)
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
    optHost :: Text,
    optPort :: Int
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
      )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> value 9162
          <> showDefault
          <> help "WS port (9160 often prettychart)"
      )

main :: IO ()
main = do
  o <- execParser $ info (optsP <**> helper) (progDesc "WebSocket face on a muster channel")
  root <- resolveRoot (optBusRoot o)
  let dir = root </> optChannel o
      fifo = dir </> "bus.fifo"
      logf = dir </> "log.md"
  ok <- doesFileExist fifo
  when (not ok) $
    error $
      "bus.fifo missing at " <> fifo <> " — run: muster bus start -c " <> optChannel o

  let cfg =
        ChannelConfig
          { chStdinPath = fifo,
            chStdoutPath = logf,
            chStderrPath = dir </> "err.md",
            chName = T.pack (optName o),
            chWorkingDir = dir
          }
      sock =
        defaultSocketConfig
          { wsHost = optHost o,
            wsPort = optPort o,
            wsPath = "/"
          }

  putStrLn $ "muster-ws: attaching as " <> optName o <> " on " <> optChannel o
  putStrLn $ "  log  " <> logf
  putStrLn $ "  ws   ws://" <> T.unpack (optHost o) <> ":" <> show (optPort o) <> "/"
  putStrLn "  open app/deck.html"
  hFlush stdout

  bracket (attachMusterRepl cfg) replClose $ \repl -> do
    busFan <- newBroadcastTChanIO
    void $ async $ busPump repl busFan
    withWSServer sock $
      wsServerApp $ \conn ->
        clientSession repl busFan conn

resolveRoot :: FilePath -> IO FilePath
resolveRoot explicit
  | not (null explicit) = pure explicit
  | otherwise = do
      menv <- lookupEnv "MUSTER_ROOT"
      case menv of
        Just d | not (null d) -> pure d
        _ -> (</> "mg/logs/muster") <$> getEnv "HOME"

busPump :: Repl -> TChan Text -> IO ()
busPump repl fan = forever $ do
  lines' <- replEmit repl
  mapM_ (\t -> atomically $ writeTChan fan t) lines'
  when (null lines') $ threadDelay 50_000

clientSession :: Repl -> TChan Text -> Connection -> IO ()
clientSession repl fan conn = do
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
      replCommit repl [body]
  wsDuplex conn inQ outQ
    `finally` do
      cancel tFan
      cancel tPost
