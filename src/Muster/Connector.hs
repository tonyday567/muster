{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generalised repl connector.
--
-- Treat any agent CLI — @hermes chat -q@, @cabal repl@, a future @grok@ TUI —
-- as the same shape: an 'Ends' seat that locks the repl loop and triggers
-- evaluation, and an emit that collects prints until a boundary.
--
-- The connector owns both the muster bus attachment and the persistent
-- process. Addressed messages become commands; collected output is posted
-- back as framed messages.
--
-- Process turn is driven through a dual-seat 'ProcessSeat': 'psOut' and
-- 'psErr' share stdin ('In'); emit each companion independently. Commit once
-- per turn via 'psOut'. Bus attach is product-native ('Muster.Channel').
module Muster.Connector
  ( ConnectorConfig (..),
    defaultConnectorConfig,
    runConnector,
  )
where

import Circuit.Agent.Process
  ( ProcessSeat (..),
    ReplConfig (..),
    defaultReplConfig,
    openProcessSeat,
  )
import Circuit.Ends (Ends (..), HasUnit (..), commit, emit, open)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Muster.Agent (addressedTo, stripAddress)
import Muster.Channel (Channel, ChannelConfig (..), channelAttach, channelClose, channelRecv, channelSend, defaultChannelConfig)
import Muster.Config qualified as Config
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Prelude

-- | Configuration for a repl connector.
data ConnectorConfig = ConnectorConfig
  { connName :: Text,
    connChannel :: String,
    connBusRoot :: FilePath,
    connProject :: FilePath,
    connCommand :: String,
    connArgs :: [String],
    connBoundary :: Text -> Bool,
    connStartupTimeout :: Int,
    connCommandTimeout :: Int
  }

defaultConnectorConfig :: Text -> ConnectorConfig
defaultConnectorConfig name =
  ConnectorConfig
    { connName = name,
      connChannel = "bus",
      connBusRoot = "",
      connProject = ".",
      connCommand = "cabal",
      connArgs = ["repl"],
      connBoundary = isGhciPrompt,
      connStartupTimeout = 180_000_000,
      connCommandTimeout = 60_000_000
    }

-- | Standard GHCi prompt boundaries.
isGhciPrompt :: Text -> Bool
isGhciPrompt t =
  "ghci> " `T.isSuffixOf` t
    || "λ> " `T.isSuffixOf` t
    || "> " `T.isSuffixOf` t

-- | Run the connector until a @quit@ / @:quit@ command is received.
runConnector :: ConnectorConfig -> IO ()
runConnector cfg = do
  let chanCfg =
        (defaultChannelConfig (connName cfg))
          { chChannel = connChannel cfg,
            chBusRoot = connBusRoot cfg
          }
  ch <- channelAttach chanCfg
  dir <- Config.connectorSessionDir (connName cfg) (connChannel cfg) (connProject cfg)
  -- Wipe prior session files, then re-create the directory so process open
  -- can mkfifo stdin and open log paths under it.
  removePathForcibly dir
  createDirectoryIfMissing True dir
  let replCfg =
        defaultReplConfig
          { replCommand = connCommand cfg,
            replArgs = connArgs cfg,
            replWorkingDir = connProject cfg,
            replStdinPath = dir </> "stdin.fifo",
            replStdoutPath = dir </> "stdout.md",
            replStderrPath = dir </> "stderr.md"
          }
  seat <- openProcessSeat replCfg
  channelSend ch $ "starting persistent repl: " <> T.pack (connCommand cfg) <> " " <> T.unwords (map T.pack (connArgs cfg)) <> " in " <> T.pack (connProject cfg)
  startup <- emitUntil (connBoundary cfg) (connStartupTimeout cfg) (psOut seat) >>= \case
    Nothing -> do
      hPutStrLn stderr "connector: timed out waiting for initial prompt"
      pure []
    Just ls -> pure ls
  err0 <- emitPoll (psErr seat)
  postOutput ch 0 startup err0
  loop ch seat 1
  channelClose ch
  psClose seat
  where
    loop ch seat turn = do
      msgs <-
        try @SomeException (channelRecv ch) >>= \case
          Left err -> do
            hPutStrLn stderr $ "connector recv error: " <> show err
            pure []
          Right ms -> pure ms
      case msgs of
        [] -> do
          threadDelay 500_000
          loop ch seat turn
        _ -> do
          done <- processMessages cfg ch seat turn msgs
          if done
            then channelSend ch "quit received; closing"
            else loop ch seat (turn + length msgs)

processMessages :: ConnectorConfig -> Channel -> ProcessSeat [Text] [Text] [Text] -> Int -> [(Text, Text)] -> IO Bool
processMessages _ _ _ _ [] = pure False
processMessages cfg ch seat turn ((sender, body) : rest) = do
  if sender == connName cfg
    then processMessages cfg ch seat (turn + 1) rest
    else do
      if addressedTo (connName cfg) body
        then do
          let cmd = stripAddress (connName cfg) body
          if cmd == "quit" || cmd == ":quit"
            then pure True
            else do
              hPutStrLn stderr $ "connector exec turn " <> show turn <> ": " <> T.unpack (T.take 100 cmd)
              channelSend ch $ "exec turn " <> T.pack (show turn) <> ": " <> cmd
              -- one commit path (shared In); emit both seats
              commitLines (psOut seat) [cmd]
              outLines <- emitUntil (connBoundary cfg) (connCommandTimeout cfg) (psOut seat) >>= \case
                Nothing -> do
                  hPutStrLn stderr "connector: no new prompt; continuing"
                  pure []
                Just ls -> pure ls
              errLines <- emitPoll (psErr seat)
              postOutput ch turn outLines errLines
              processMessages cfg ch seat (turn + 1) rest
        else processMessages cfg ch seat (turn + 1) rest

postOutput :: Channel -> Int -> [Text] -> [Text] -> IO ()
postOutput ch turn outLines errLines = do
  let outHeader = "turn " <> T.pack (show turn) <> " output"
      body = T.unlines $ [outHeader, "", "-- stdout --"] <> outLines <> ["", "-- stderr --"] <> errLines
  channelSend ch body

-- | Commit lines through a seat conjoint (shared stdin).
commitLines :: Ends (Kleisli IO) [Text] [Text] -> [Text] -> IO ()
commitLines e ts = runKleisli (commit (conjoint e) outU) ts
  where
    Ends _ outU = open

-- | One poll emit through a seat companion.
emitPoll :: Ends (Kleisli IO) [Text] [Text] -> IO [Text]
emitPoll e = runKleisli (emit (companion e) inU) ()
  where
    Ends inU _ = open

-- | Poll emit until boundary predicate or timeout (microseconds).
-- Used on the stdout seat (prompt boundary lives on stdout).
emitUntil :: (Text -> Bool) -> Int -> Ends (Kleisli IO) [Text] [Text] -> IO (Maybe [Text])
emitUntil p t e = go 0 [] 10000
  where
    go elapsed acc delay = do
      news <- emitPoll e
      let acc' = acc <> news
      if any p news
        then pure (Just acc')
        else do
          let elapsed' = elapsed + delay
          if elapsed' >= t
            then pure Nothing
            else do
              threadDelay delay
              let delay' = min 500000 (floor (fromIntegral delay * 1.5 :: Double))
              go elapsed' acc' delay'
