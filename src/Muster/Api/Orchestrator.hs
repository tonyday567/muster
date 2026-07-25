{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Orchestration surface for the rebuilt muster API.
--
-- This layer is the read-side and control-side convenience over the bus and
-- agent layers: listing agents, querying status, addressing agents, pinging
-- them, and running a daemon-style watch loop.
module Muster.Api.Orchestrator
  ( ps,
    status,
    tell,
    ping,
    watchLoop,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (forever, unless)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Muster.Api.Agent qualified as Ag
import Muster.Api.Bus qualified as Bus
import Muster.Api.Participant qualified as Part
import Muster.Api.Types (AgentState (..), BusRoot (..), Channel (..), Nick (..))
import Muster.Bus qualified as BusEngine
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Prelude

run :: IO a -> IO (Either Text a)
run action = do
  r <- try @SomeException action
  pure $ case r of
    Left e -> Left (T.pack (show e))
    Right a -> Right a

-- | List all agents and their current state.
ps :: BusRoot -> IO [AgentState]
ps root = do
  names <- Ag.listAgents root
  states <- mapM (Ag.agentState root) names
  pure [s | Just s <- states]

-- | Show the status of a named agent, or the caller's own current identity if
-- no name is given.
status :: BusRoot -> Maybe Nick -> IO Text
status root mName = do
  case mName of
    Just name -> do
      mState <- Ag.agentState root name
      pure $ case mState of
        Nothing -> "no agent named " <> unNick name
        Just s -> T.pack (show s)
    Nothing -> do
      mName' <- Part.currentName root
      mChan <- Part.currentChannel root
      pure $ case (mName', mChan) of
        (Just n, Just c) -> unNick n <> " on #" <> unChannel c
        (Just n, Nothing) -> unNick n <> " (no channel)"
        (Nothing, _) -> "no name set"

-- | Post a message to an agent's channel.
--
-- The sender is the current caller if they have set a name; otherwise the
-- message is sent as @muster@.
tell :: BusRoot -> Nick -> Text -> IO (Either Text ())
tell root name body = run $ do
  mSender <- Part.currentName root
  let sender = fromMaybe "muster" mSender
      chan = Channel (unNick name)
  _ <- Bus.ensureChannel root chan
  ok <- Bus.waitForChannel root chan
  unless ok $ fail $ "bus not running for #" <> T.unpack (unNick name)
  Bus.post root chan sender body

-- | Single-shot wakeup of an agent.
--
-- The agent is started if it is stopped, then a short ping message is posted to
-- its channel. The agent's own loop decides whether to run a turn and stop
-- again.
ping :: BusRoot -> Maybe Nick -> IO (Either Text ())
ping root mName = run $ do
  name <- case mName of
    Just n -> pure n
    Nothing ->
      Part.currentName root >>= \case
        Nothing -> fail "give a name or set one with 'muster name'"
        Just n -> pure n
  _ <- Ag.startAgent root name
  case T.unpack (unNick name) of
    "muster" -> fail "refusing to ping the orchestrator"
    _ -> pure ()
  let chan = Channel (unNick name)
  _ <- Bus.ensureChannel root chan
  ok <- Bus.waitForChannel root chan
  unless ok $ fail $ "bus not running for #" <> T.unpack (unNick name)
  Bus.post root chan "muster" "ping"

-- | Daemon-style continuous watch on the current channel.
--
-- The caller must have joined a channel. The loop wakes on any message
-- addressed to @name@ (or to the caller if no name is given) and prints it.
-- Pass @loop = False@ for a single wakeup.
watchLoop :: BusRoot -> Maybe Nick -> Bool -> IO (Either Text ())
watchLoop root mName loop = run $ do
  name <- case mName of
    Just n -> pure n
    Nothing ->
      Part.currentName root >>= \case
        Nothing -> fail "give a watcher name or set one with 'muster name'"
        Just n -> pure n
  chan <-
    Part.currentChannel root >>= \case
      Nothing -> fail "join a channel first (muster join <channel>)"
      Just c -> pure c
  let dir = Bus.channelPath root chan
      watchCursor = Bus.channelPath root chan </> ".watch-" <> T.unpack (unNick name)
      addressed line =
        ("@" <> unNick name) `T.isInfixOf` line
          || (unNick name <> ":") `T.isInfixOf` line
  -- Initialise watch cursor from the join cursor if possible, else tail.
  initWatchCursor root chan name watchCursor
  let once = do
        exit <- BusEngine.wait dir watchCursor 86400 addressed
        case exit of
          ExitSuccess -> pure ()
          ExitFailure 2 -> pure ()
          e -> fail $ "watch exited: " <> show e
  if loop then forever once else once

initWatchCursor :: BusRoot -> Channel -> Nick -> FilePath -> IO ()
initWatchCursor root chan name watchCursor = do
  let joinCursor = Bus.cursorPath root chan name
  exists <- doesFileExist joinCursor
  n <-
    if exists
      then do
        raw <- readFile joinCursor
        pure $ case reads raw of
          [(i, _)] -> i
          _ -> 0
      else do
        let dir = Bus.channelPath root chan
        BusEngine.countLogLines dir
  writeFile watchCursor (show n <> "\n")
