{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Agent lifecycle layer for the rebuilt muster API.
--
-- Agents are dialed up, not owned. Each agent has a directory under the bus
-- root, a reserved name, and (when running) a process id. The API implements
-- the locked verbs: @new@, @start@, @stop@, @quit@ and @rename@.
module Muster.Api.Agent
  ( -- * Configuration
    AgentConfig (..),
    defaultAgentConfig,
    writeAgentConfig,

    -- * Lifecycle
    newAgent,
    startAgent,
    stopAgent,
    quitAgent,
    renameAgent,

    -- * Directory
    listAgents,
    agentState,
    agentRunning,
    agentDirPath,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, catch, try)
import Control.Monad (void, when)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muster.Api.Bus qualified as Bus
import Muster.Api.Participant qualified as Part
import Muster.Api.Types (AgentState (..), AgentStatus (..), BusRoot (..), Channel (..), Nick (..), SessionId (..), validateNick)
import Muster.Bus qualified as BusEngine
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removePathForcibly,
    renameDirectory,
  )
import System.FilePath ((</>))
import System.Posix.Signals (sigTERM, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process (CreateProcess (..), StdStream (..), createProcess, getPid, proc)
import Text.Read (readMaybe)
import Prelude

-- $setup
-- >>> :set -XOverloadedStrings

-- | External command used to run an agent.
data AgentConfig = AgentConfig
  { agentCommand :: String,
    agentArgs :: [String]
  }
  deriving stock (Eq, Show)

-- | Path to the agent's private directory.
--
-- >>> agentDirPath (BusRoot "/tmp/muster") (Nick "hermes")
-- "/tmp/muster/agents/hermes"
agentDirPath :: BusRoot -> Nick -> FilePath
agentDirPath root (Nick n) = Bus.busRootPath root </> "agents" </> T.unpack n

agentConfigPath :: BusRoot -> Nick -> FilePath
agentConfigPath root nick = agentDirPath root nick </> "agent.config"

agentPidPath :: BusRoot -> Nick -> FilePath
agentPidPath root nick = agentDirPath root nick </> "agent.pid"

agentSessionPath :: BusRoot -> Nick -> FilePath
agentSessionPath root nick = agentDirPath root nick </> "session"

agentStatusPath :: BusRoot -> Nick -> FilePath
agentStatusPath root nick = agentDirPath root nick </> "status"

agentDoingPath :: BusRoot -> Nick -> FilePath
agentDoingPath root nick = agentDirPath root nick </> "doing"

-- | Default command: @muster-agent --name <name> --bus-root <root>@.
defaultAgentConfig :: BusRoot -> Nick -> AgentConfig
defaultAgentConfig root nick =
  AgentConfig
    { agentCommand = "muster-agent",
      agentArgs =
        [ "--name",
          T.unpack (unNick nick),
          "--bus-root",
          Bus.busRootPath root
        ]
    }

readAgentConfig :: BusRoot -> Nick -> IO AgentConfig
readAgentConfig root nick = do
  let path = agentConfigPath root nick
  exists <- doesFileExist path
  if not exists
    then pure (defaultAgentConfig root nick)
    else do
      raw <- T.unpack <$> TIO.readFile path
      case words raw of
        [] -> pure (defaultAgentConfig root nick)
        (cmd : args) -> pure (AgentConfig cmd args)

writeAgentConfig :: BusRoot -> Nick -> AgentConfig -> IO ()
writeAgentConfig root nick cfg = do
  let path = agentConfigPath root nick
      body = T.pack $ unwords (agentCommand cfg : agentArgs cfg)
  TIO.writeFile path body

readAgentPid :: BusRoot -> Nick -> IO (Maybe ProcessID)
readAgentPid root nick = do
  let path = agentPidPath root nick
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      raw <- readFile path
      pure $ case readMaybe (filter (/= '\n') raw) :: Maybe Integer of
        Nothing -> Nothing
        Just i -> Just (fromIntegral i)

writeAgentPid :: BusRoot -> Nick -> ProcessID -> IO ()
writeAgentPid root nick pid = do
  let path = agentPidPath root nick
  writeFile path (show pid <> "\n")

removeAgentPid :: BusRoot -> Nick -> IO ()
removeAgentPid root nick =
  void $ try @IOException $ removePathForcibly (agentPidPath root nick)

processAlive :: ProcessID -> IO Bool
processAlive pid =
  (signalProcess 0 pid >> pure True) `catch` \(_ :: IOException) -> pure False

terminateAgent :: ProcessID -> IO ()
terminateAgent pid = void $ try @IOException $ signalProcess sigTERM pid

waitForProcessExit :: ProcessID -> Int -> IO ()
waitForProcessExit _ 0 = pure ()
waitForProcessExit pid n = do
  alive <- processAlive pid
  when alive $ do
    threadDelay 100000
    waitForProcessExit pid (n - 1)

agentRunning :: BusRoot -> Nick -> IO Bool
agentRunning root nick = do
  mpid <- readAgentPid root nick
  case mpid of
    Nothing -> pure False
    Just pid -> processAlive pid

-- | Pool of auto-generated agent names. Once the friendly names are exhausted
-- we fall back to @agent-N@.
namePool :: [Text]
namePool =
  ["fable", "grok", "kimi", "claude", "deep", "deck", "lemon", "mini", "stinky"]
    ++ map (\n -> "agent-" <> T.pack (show (n :: Int))) [1 ..]

-- | Pick the next free name from the pool.
generateName :: BusRoot -> IO (Maybe Nick)
generateName root = do
  used <- Part.readUsedNames root
  pure $ find used namePool
  where
    find _ [] = Nothing
    find used (x : xs)
      | Nick x `Set.member` used = find used xs
      | otherwise = Just (Nick x)

-- | Create a new agent. If no name is supplied, one is auto-generated from the
-- pool. The name is reserved in the shared used-names list and an agent
-- directory is created.
newAgent :: BusRoot -> Maybe Text -> IO (Either Text Nick)
newAgent root mRaw = do
  Bus.ensureBusRoot root
  mNick <- case mRaw of
    Just raw -> pure $ validateNick raw
    Nothing -> do
      mg <- generateName root
      pure $ maybe (Left "no free names in pool") Right mg
  case mNick of
    Left err -> pure (Left err)
    Right nick -> do
      ok <- Part.claimName root nick
      if not ok
        then pure $ Left $ "name " <> unNick nick <> " is already in use"
        else do
          createDirectoryIfMissing True (agentDirPath root nick)
          writeAgentConfig root nick (defaultAgentConfig root nick)
          pure (Right nick)

-- | Start an agent (or resume a stopped one). If the agent is already running
-- this is a no-op.
--
-- The agent is joined to its own channel (named after the agent) so that
-- @muster tell <name>@ has a place to land.
startAgent :: BusRoot -> Nick -> IO (Either Text ())
startAgent root nick = do
  Bus.ensureBusRoot root
  exists <- doesDirectoryExist (agentDirPath root nick)
  if not exists
    then pure $ Left $ "agent " <> unNick nick <> " does not exist"
    else do
      running <- agentRunning root nick
      if running
        then pure (Right ())
        else do
          let chan = Channel (unNick nick)
          _ <- Bus.ensureChannel root chan
          total <- BusEngine.countLogLines (Bus.busRootPath root)
          writeFile (Bus.cursorPath root chan nick) (show total <> "\n")
          cfg <- readAgentConfig root nick
          er <- try @SomeException $ do
            (_, _, _, ph) <-
              createProcess
                (proc (agentCommand cfg) (agentArgs cfg))
                  { std_in = NoStream,
                    std_out = NoStream,
                    std_err = NoStream,
                    close_fds = True
                  }
            mpid <- getPid ph
            case mpid of
              Nothing -> fail "failed to obtain agent process id"
              Just pid -> do
                writeAgentPid root nick pid
                pure (Right ())
          pure $ case er of
            Left e -> Left (T.pack (show e))
            Right r -> r

-- | Stop a running agent but keep its directory and session so it can be
-- resumed later.
stopAgent :: BusRoot -> Nick -> IO (Either Text ())
stopAgent root nick = do
  exists <- doesDirectoryExist (agentDirPath root nick)
  if not exists
    then pure $ Left $ "agent " <> unNick nick <> " does not exist"
    else do
      mpid <- readAgentPid root nick
      case mpid of
        Nothing -> pure (Right ())
        Just pid -> do
          alive <- processAlive pid
          if alive
            then do
              terminateAgent pid
              waitForProcessExit pid 30
            else pure ()
          removeAgentPid root nick
          pure (Right ())

-- | Tear down an agent and free its name.
quitAgent :: BusRoot -> Nick -> IO (Either Text ())
quitAgent root nick = do
  _ <- stopAgent root nick
  exists <- doesDirectoryExist (agentDirPath root nick)
  when exists $ removePathForcibly (agentDirPath root nick)
  Part.releaseName root nick
  pure (Right ())

-- | Rename an agent.
--
-- The old agent is stopped, its directory renamed, and the name reservation
-- updated. The agent's channel also changes to the new name.
renameAgent :: BusRoot -> Nick -> Nick -> IO (Either Text ())
renameAgent root old new
  | old == new = pure (Right ())
  | otherwise = do
      ok <- Part.renameName root old new
      if not ok
        then pure $ Left $ "name " <> unNick new <> " is already in use"
        else do
          _ <- stopAgent root old
          let oldDir = agentDirPath root old
              newDir = agentDirPath root new
          oldExists <- doesDirectoryExist oldDir
          when oldExists $ renameDirectory oldDir newDir
          writeAgentConfig root new (defaultAgentConfig root new)
          -- Move the agent's join cursor from the old channel to the new one.
          void $ try @IOException $ removePathForcibly (Bus.cursorPath root (Channel (unNick old)) old)
          let newChan = Channel (unNick new)
          _ <- Bus.ensureChannel root newChan
          total <- BusEngine.countLogLines (Bus.busRootPath root)
          writeFile (Bus.cursorPath root newChan new) (show total <> "\n")
          pure (Right ())

-- | List all agents that have a directory under the bus root.
listAgents :: BusRoot -> IO [Nick]
listAgents root = do
  let dir = Bus.busRootPath root </> "agents"
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- listDirectory dir
      pure $ map (Nick . T.pack) $ filter (not . null) entries

-- | Read the current state of an agent from disk.
agentState :: BusRoot -> Nick -> IO (Maybe AgentState)
agentState root nick = do
  let dir = agentDirPath root nick
  exists <- doesDirectoryExist dir
  if not exists
    then pure Nothing
    else do
      mpid <- readAgentPid root nick
      status <-
        agentRunning root nick >>= \case
          True -> pure Running
          False ->
            case mpid of
              Just _ -> pure Stopped
              Nothing -> pure Unknown
      lastDone <- readOptional dir agentStatusPath
      currentDoing <- readOptional dir agentDoingPath
      mSession <- readOptional dir agentSessionPath
      pure
        $ Just
        $ AgentState
          { agentName = nick,
            agentPid = fromIntegral <$> mpid,
            agentSessionId = SessionId <$> mSession,
            agentStatus = status,
            agentLastDone = lastDone,
            agentCurrentDoing = currentDoing
          }
  where
    readOptional dir0 path = do
      let full = dir0 </> path root nick
      e <- doesFileExist full
      if e
        then Just . T.strip <$> TIO.readFile full
        else pure Nothing
