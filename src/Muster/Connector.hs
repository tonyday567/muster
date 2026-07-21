{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generalised repl connector.
--
-- Treat any agent CLI — @hermes chat -q@, @cabal repl@, a future @grok@ TUI —
-- as the same shape: an In buffer that locks the repl loop and triggers
-- evaluation, and an Out buffer that collects prints until a boundary.
--
-- The connector owns both the muster bus attachment and the persistent
-- process. Addressed messages become commands; collected output is posted
-- back as framed messages.
module Muster.Connector
  ( ConnectorConfig (..),
    defaultConnectorConfig,
    runConnector,
  )
where

import Circuit.Ends (Ends (..), HasUnit (..), In (..), Out (..), commit, emit)
import Circuit.Repl (ProcessPorts (..), ReplConfig (..), defaultReplConfig, openProcessPorts)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Muster.Agent (addressedTo, stripAddress)
import Muster.Channel (Channel, ChannelConfig (..), channelAttach, channelClose, channelRecv, channelSend, defaultChannelConfig)
import Muster.Config qualified as Config
import System.Directory (removePathForcibly)
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
  removePathForcibly dir
  let replCfg =
        defaultReplConfig
          { replCommand = connCommand cfg,
            replArgs = connArgs cfg,
            replWorkingDir = connProject cfg,
            replStdinPath = dir </> "stdin.fifo",
            replStdoutPath = dir </> "stdout.md",
            replStderrPath = dir </> "stderr.md"
          }
  pp <- openProcessPorts replCfg
  channelSend ch $ "starting persistent repl: " <> T.pack (connCommand cfg) <> " " <> T.unwords (map T.pack (connArgs cfg)) <> " in " <> T.pack (connProject cfg)
  startup <- emitOutUntil (connBoundary cfg) (connStartupTimeout cfg) pp >>= \case
    Nothing -> do
      hPutStrLn stderr "connector: timed out waiting for initial prompt"
      pure []
    Just ls -> pure ls
  err0 <- emitErr pp
  postOutput ch 0 startup err0
  loop ch pp 1
  channelClose ch
  peClose pp
  where
    loop ch pp turn = do
      msgs <-
        try @SomeException (channelRecv ch) >>= \case
          Left err -> do
            hPutStrLn stderr $ "connector recv error: " <> show err
            pure []
          Right ms -> pure ms
      case msgs of
        [] -> do
          threadDelay 500_000
          loop ch pp turn
        _ -> do
          done <- processMessages cfg ch pp turn msgs
          if done
            then channelSend ch "quit received; closing"
            else loop ch pp (turn + length msgs)

processMessages :: ConnectorConfig -> Channel -> ProcessPorts [Text] [Text] [Text] -> Int -> [(Text, Text)] -> IO Bool
processMessages _ _ _ _ [] = pure False
processMessages cfg ch pp turn ((sender, body) : rest) = do
  if sender == connName cfg
    then processMessages cfg ch pp (turn + 1) rest
    else do
      if addressedTo (connName cfg) body
        then do
          let cmd = stripAddress (connName cfg) body
          if cmd == "quit" || cmd == ":quit"
            then pure True
            else do
              hPutStrLn stderr $ "connector exec turn " <> show turn <> ": " <> T.unpack (T.take 100 cmd)
              channelSend ch $ "exec turn " <> T.pack (show turn) <> ": " <> cmd
              commitLines pp [cmd]
              outLines <- emitOutUntil (connBoundary cfg) (connCommandTimeout cfg) pp >>= \case
                Nothing -> do
                  hPutStrLn stderr "connector: no new prompt; continuing"
                  pure []
                Just ls -> pure ls
              errLines <- emitErr pp
              postOutput ch turn outLines errLines
              processMessages cfg ch pp (turn + 1) rest
        else processMessages cfg ch pp (turn + 1) rest

postOutput :: Channel -> Int -> [Text] -> [Text] -> IO ()
postOutput ch turn outLines errLines = do
  let outHeader = "turn " <> T.pack (show turn) <> " output"
      body = T.unlines $ [outHeader, "", "-- stdout --"] <> outLines <> ["", "-- stderr --"] <> errLines
  channelSend ch body

commitLines :: ProcessPorts [Text] [Text] [Text] -> [Text] -> IO ()
commitLines pp ts = runKleisli (commit (peIn pp) outU) ts
  where
    Ends _ outU = open

emitOut :: ProcessPorts [Text] [Text] [Text] -> IO [Text]
emitOut pp = runKleisli (emit (peOut pp) inU) ()
  where
    Ends inU _ = open

emitErr :: ProcessPorts [Text] [Text] [Text] -> IO [Text]
emitErr pp = runKleisli (emit (peErr pp) inU) ()
  where
    Ends inU _ = open

emitOutUntil :: (Text -> Bool) -> Int -> ProcessPorts [Text] [Text] [Text] -> IO (Maybe [Text])
emitOutUntil p t pp = go 0 [] 10000
  where
    go elapsed acc delay = do
      news <- emitOut pp
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

