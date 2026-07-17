{-# LANGUAGE OverloadedStrings #-}

-- | Bare-bones WebSocket face on a muster channel.
--
-- 'Circuit.Socket' (circuits-io) + 'Circuit.Comm' channel attach.
-- Browser text → bus; all bus lines (including self) → browser.
--
-- @
--   cabal run muster-ws -- --name desk
--   open app/deck.html
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

  let name = T.pack (optName o)
      cfg =
        ChannelConfig
          { chStdinPath = fifo,
            chStdoutPath = logf,
            chStderrPath = dir </> "err.md",
            chName = name,
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
  putStrLn "  open app/deck.html  (self-echo on — your posts show in the scroll)"
  hFlush stdout

  bracket (channelAttach cfg) channelClose $ \ch -> do
    busFan <- newBroadcastTChanIO
    void $ async $ busPump ch busFan
    withWSServer sock $
      wsServerApp $ \conn ->
        clientSession ch name busFan conn

resolveRoot :: FilePath -> IO FilePath
resolveRoot explicit
  | not (null explicit) = pure explicit
  | otherwise = do
      menv <- lookupEnv "MUSTER_ROOT"
      case menv of
        Just d | not (null d) -> pure d
        _ -> (</> "mg/logs/muster") <$> getEnv "HOME"

-- | All framed lines, including our own (unlike muster watch / attachMusterRepl).
busPump :: Channel -> TChan Text -> IO ()
busPump ch fan = forever $ do
  msgs <- channelRecv ch
  mapM_
    ( \(sender, body) ->
        atomically $ writeTChan fan (frameMessage sender body)
    )
    msgs
  when (null msgs) $ threadDelay 50_000

clientSession :: Channel -> Text -> TChan Text -> Connection -> IO ()
clientSession ch _name fan conn = do
  inQ <- newTQueueIO
  outQ <- newTQueueIO
  myFan <- atomically $ dupTChan fan
  tFan <- async $ forever $ do
    t <- atomically $ readTChan myFan
    atomically $ writeTQueue outQ t
  tPost <- async $ forever $ do
    t <- atomically $ readTQueue inQ
    let body = T.strip t
    -- No watch-style exclude-self: busPump fans [desk] lines back via channelRecv.
    when (not (T.null body)) $
      channelSend ch body
  wsDuplex conn inQ outQ
    `finally` do
      cancel tFan
      cancel tPost
