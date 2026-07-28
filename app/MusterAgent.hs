{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Long-lived agent process that participates on one or more muster bus
-- channels.
--
-- Agents are symmetric to human participants: they are joined to channels
-- explicitly via `muster join <agent> -c <channel>`. The agent scans the
-- bus root for `.cursor-<name>` files and attaches to every channel where it
-- has been joined.
--
-- Two modes:
--
--   * __oneshot__: runs @<agent> chat -q <prompt>@ for each addressed message.
--   * __persistent__: keeps a single repl process alive (e.g. @cabal repl@)
--     on the agent's primary/default channel.
--
-- The oneshot path is a composed pipeline of 'circuits-agent' seats:
--
-- @
--   wake → filter → meta → main → bus → bucket → diag
-- @
--
-- Usage:
--
-- @
--   muster-agent --name deep --agent hermes
--   muster-agent --name cabal --agent cabal --mode persistent --project ~/haskell/circuits
-- @
module Main (main) where

import Circuit.Agent ((>:>))
import Control.Concurrent (Chan, newChan, readChan, threadDelay, writeChan)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
  ( TChan,
    TQueue,
    atomically,
    newTChanIO,
    newTQueueIO,
    readTQueue,
    tryReadTChan,
    writeTQueue,
  )
import Control.Exception (SomeException, bracket, try)
import Control.Monad (foldM, forever, unless, void, when)
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muster.Agent
  ( AgentConfig (..),
    AgentMode (..),
    Post (..),
    Shard,
    defaultAgentConfig,
    oneshotShard,
    runShard,
  )
import Muster.Agent.Seat
  ( MetaAction (..),
    bucketShard,
    busSink,
    diagShard,
    filterShard,
    metaShard,
  )
import Muster.Channel
  ( Channel,
    ChannelConfig (..),
    channelAttach,
    channelRecv,
    channelSend,
    defaultChannelConfig,
  )
import Muster.Connector (ConnectorConfig (..), defaultConnectorConfig, runConnector)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory)
import System.Environment (getEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO
  ( BufferMode (..),
    Handle,
    hClose,
    hGetLine,
    hIsEOF,
    hSetBuffering,
    hSetEncoding,
    stderr,
    utf8,
  )
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    proc,
    terminateProcess,
    waitForProcess,
  )
import Prelude

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

data MusterAgentConfig = MusterAgentConfig
  { maName :: String,
    maModel :: Maybe String,
    maProvider :: Maybe String,
    maAgent :: String,
    maMode :: AgentMode,
    maProject :: Maybe FilePath,
    maBusRoot :: FilePath,
    maFilter :: String
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- CLI
-- ---------------------------------------------------------------------------

modeReader :: ReadM AgentMode
modeReader = eitherReader $ \case
  "oneshot" -> Right OneShot
  "persistent" -> Right Persistent
  s -> Left $ "unknown mode: " <> s <> " (expected oneshot or persistent)"

configParser :: Parser MusterAgentConfig
configParser =
  MusterAgentConfig
    <$> strOption
      ( long "name"
          <> short 'n'
          <> metavar "NAME"
          <> help "Agent name on the muster bus"
      )
    <*> optional
      ( strOption
          ( long "model"
              <> short 'm'
              <> metavar "MODEL"
              <> help "Model override (default: env HERMES_MODEL or deepseek-v4-pro)"
          )
      )
    <*> optional
      ( strOption
          ( long "provider"
              <> metavar "PROVIDER"
              <> help "Provider override (default: env HERMES_PROVIDER or deepseek)"
          )
      )
    <*> strOption
      ( long "agent"
          <> value "hermes"
          <> showDefault
          <> metavar "CMD"
          <> help "Agent CLI command (oneshot: <cmd> chat -q; persistent: <cmd> repl)"
      )
    <*> option
      modeReader
      ( long "mode"
          <> value OneShot
          <> showDefault
          <> metavar "MODE"
          <> help "Agent mode: oneshot | persistent"
      )
    <*> optional
      ( strOption
          ( long "project"
              <> metavar "DIR"
              <> help "Project directory for persistent repl (default: ~/haskell/circuits)"
          )
      )
    <*> strOption
      ( long "bus-root"
          <> value ""
          <> showDefault
          <> metavar "DIR"
          <> help "Root directory for all bus state (default: $HOME/.config/muster)"
      )
    <*> strOption
      ( long "filter"
          <> short 'f'
          <> value ""
          <> metavar "REGEX"
          <> help "Alert filter regex (default: @<name> — wake only when addressed)"
      )

optsInfo :: ParserInfo MusterAgentConfig
optsInfo =
  info
    (configParser <**> helper)
    ( fullDesc
        <> progDesc "Long-lived agent on one or more muster bus channels"
        <> header "muster-agent — join, listen, dispatch, reply"
    )

-- ---------------------------------------------------------------------------
-- Paths
-- ---------------------------------------------------------------------------

busRoot :: MusterAgentConfig -> IO FilePath
busRoot cfg = do
  let explicit = maBusRoot cfg
  if not (null explicit)
    then pure explicit
    else (</> ".config/muster") <$> getEnv "HOME"

agentDir :: MusterAgentConfig -> IO FilePath
agentDir cfg = do
  root <- busRoot cfg
  pure (root </> "agents" </> maName cfg)

agentCursorPattern :: String -> String
agentCursorPattern name = ".cursor-" <> name

-- | Discover every channel under the bus root where this agent has a cursor.
discoveredChannels :: MusterAgentConfig -> IO [String]
discoveredChannels cfg = do
  root <- busRoot cfg
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      entries <- listDirectory root
      fmap (sort . concat) $ mapM (channelFor root) entries
  where
    channelFor :: FilePath -> FilePath -> IO [String]
    channelFor root entry = do
      let dir = root </> entry
      isDir <- doesDirectoryExist dir
      if not isDir
        then pure []
        else do
          cursorExists <- doesFileExist (dir </> agentCursorPattern (maName cfg))
          pure [entry | cursorExists]

toChannelConfig :: MusterAgentConfig -> String -> ChannelConfig
toChannelConfig cfg channel =
  (defaultChannelConfig (T.pack (maName cfg)))
    { chChannel = channel,
      chBusRoot = maBusRoot cfg
    }

toAgentConfig :: MusterAgentConfig -> AgentConfig
toAgentConfig cfg =
  defaultAgentConfig
    { cfgName = maName cfg,
      cfgAgent = maAgent cfg,
      cfgModel = maModel cfg,
      cfgProvider = maProvider cfg
    }

projectDir :: MusterAgentConfig -> IO FilePath
projectDir cfg = case maProject cfg of
  Just d -> pure d
  Nothing -> (</> "haskell" </> "circuits") <$> getHomeDirectory

-- ---------------------------------------------------------------------------
-- Diagnostics sink
-- ---------------------------------------------------------------------------

-- | Write one diagnostic line to the queue.
diagLine :: TQueue Text -> String -> IO ()
diagLine q msg = atomically (writeTQueue q (T.pack msg))

-- | Drain the diagnostic queue to stderr forever.
startDiagDrain :: TQueue Text -> IO ()
startDiagDrain q = void $ async $ forever $ do
  msg <- atomically $ readTQueue q
  TIO.hPutStrLn stderr msg

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cfg <- execParser optsInfo
  let filter' = if null (maFilter cfg) then "@" <> maName cfg else maFilter cfg
      cfg' = cfg {maFilter = filter'}
  diagQ <- newTQueueIO
  startDiagDrain diagQ
  diagLine diagQ $ "muster-agent [" <> maName cfg' <> "] starting"
  diagLine diagQ $ "  mode: " <> show (maMode cfg')
  diagLine diagQ $ "  agent command: " <> maAgent cfg'
  diagLine diagQ $ "  filter: " <> maFilter cfg'
  case maMode cfg' of
    OneShot -> runOneShot diagQ cfg'
    Persistent -> runPersistent cfg'

runOneShot :: TQueue Text -> MusterAgentConfig -> IO ()
runOneShot diagQ cfg = do
  let agentCfg = toAgentConfig cfg
      who = T.pack (maName cfg)
      diag = diagLine diagQ
  mainSeat <- oneshotShard agentCfg who
  metaChan <- newTChanIO
  adir <- agentDir cfg
  createDirectoryIfMissing True adir
  let outPath = adir </> "output.md"
  wakeQueue <- newChan
  diag "scanning for joined channels..."
  diag "  seat: oneshot Shard IO [Post] (circuits-agent)"
  let go :: Map String Channel -> Map String (Async ()) -> IO ()
      go channels watchers = do
        newChannels <- refreshChannels diag cfg channels
        -- Drain any meta actions produced by the previous turn; refresh
        -- channels and restart watchers if they changed.
        channelsAfterMeta <- drainMeta diagQ cfg metaChan newChannels
        if Map.keysSet channelsAfterMeta /= Map.keysSet newChannels
          then do
            mapM_ cancel (Map.elems watchers)
            go channelsAfterMeta Map.empty
          else do
            newWatchers <- refreshWatchers diag cfg wakeQueue watchers newChannels
            if Map.null newChannels
              then do
                diag "  no channels joined; sleeping 5s before rescan..."
                threadDelay 5_000_000
                go newChannels newWatchers
              else do
                channelName <- readChan wakeQueue
                case Map.lookup channelName newChannels of
                  Nothing -> go newChannels newWatchers
                  Just ch -> do
                    msgs <- channelRecv ch
                    unless (null msgs) $
                      mapM_ (\(s, b) -> handleWake diagQ who outPath mainSeat metaChan newChannels (channelName, s, b)) msgs
                    go newChannels newWatchers
  go Map.empty Map.empty

-- | Drain meta actions from the pipeline and refresh the channel map.
drainMeta ::
  TQueue Text ->
  MusterAgentConfig ->
  TChan MetaAction ->
  Map String Channel ->
  IO (Map String Channel)
drainMeta diagQ cfg metaChan channels = do
  let diag = diagLine diagQ
  mAct <- atomically (tryReadTChan metaChan)
  case mAct of
    Nothing -> pure channels
    Just act -> do
      channels' <- case act of
        JoinChannel c -> do
          diag $ "  meta: joining #" <> c
          runMusterJoin diag cfg c
          refreshChannels diag cfg channels
        LeaveChannel _ -> do
          diag "  meta: leaving current channel"
          runMusterLeave diag cfg ""
          refreshChannels diag cfg channels
      drainMeta diagQ cfg metaChan channels'

-- | Build the oneshot pipeline for the current channel map.
--
-- The main seat is supplied from outside so its hermes session state survives
-- channel refreshes.
buildPipeline ::
  Text ->
  TQueue Text ->
  FilePath ->
  TChan MetaAction ->
  Map String Channel ->
  Shard IO [Post] ->
  IO (Shard IO [Post])
buildPipeline who diagQ outPath metaChan channels mainSeat = do
  f <- filterShard who
  m <- metaShard metaChan diagQ
  b <- busSink (Map.mapKeys T.pack (Map.map channelSend channels))
  buck <- bucketShard outPath
  d <- diagShard diagQ
  pure (f >:> m >:> mainSeat >:> b >:> buck >:> d)

-- | Wake → addressed message → run pipeline → bus + bucket + diag.
handleWake ::
  TQueue Text ->
  Text ->
  FilePath ->
  Shard IO [Post] ->
  TChan MetaAction ->
  Map String Channel ->
  (String, Text, Text) ->
  IO ()
handleWake diagQ who outPath mainSeat metaChan channels (channel, sender, rawBody) = do
  let diag = diagLine diagQ
  diag $ "  task on #" <> channel <> ": " <> T.unpack (T.take 100 rawBody)
  pipeline <- buildPipeline who diagQ outPath metaChan channels mainSeat
  let pIn =
        Post
          { author = sender,
            addr = who,
            channel = T.pack channel,
            body = rawBody
          }
  void $ try @SomeException (runShard pipeline [pIn])

-- | Refresh the set of attached channels.
refreshChannels ::
  (String -> IO ()) ->
  MusterAgentConfig ->
  Map String Channel ->
  IO (Map String Channel)
refreshChannels diag cfg old = do
  names <- discoveredChannels cfg
  let addNew acc name =
        case Map.lookup name acc of
          Just _ -> pure acc
          Nothing -> do
            let chanCfg = toChannelConfig cfg name
            res <- try @SomeException (channelAttach chanCfg)
            case res of
              Left err -> do
                diag $ "  failed to attach to #" <> name <> ": " <> show err
                pure acc
              Right ch -> do
                diag $ "  joined #" <> name
                _ <- channelRecv ch
                pure (Map.insert name ch acc)
  added <- foldM addNew old names
  let current = Set.fromList names
      stale = Map.keysSet added `Set.difference` current
  unless (Set.null stale) $
    diag $ "  left channels: " <> unwords (Set.toList stale)
  pure (foldr Map.delete added (Set.toList stale))

-- | Start alert watchers for newly-joined channels and cancel watchers for
-- channels that have been left.
refreshWatchers ::
  (String -> IO ()) ->
  MusterAgentConfig ->
  Chan String ->
  Map String (Async ()) ->
  Map String Channel ->
  IO (Map String (Async ()))
refreshWatchers diag cfg wakeQueue old channels = do
  let current = Set.fromList (Map.keys channels)
      stale = Map.keysSet old `Set.difference` current
      newNames = Set.toList (current `Set.difference` Map.keysSet old)
  mapM_ cancel (Map.elems (Map.filterWithKey (\k _ -> Set.member k stale) old))
  added <- mapM (\name -> (name,) <$> async (alertWatcher diag cfg wakeQueue name)) newNames
  pure $ Map.union (Map.difference old (Map.fromSet (const ()) stale)) (Map.fromList added)

alertWatcher :: (String -> IO ()) -> MusterAgentConfig -> Chan String -> String -> IO ()
alertWatcher diag cfg wakeQueue channel = forever $ do
  res <- try @SomeException $
    bracket (startAlert cfg channel) stopAlert $ \(h, _) -> do
      hSetBuffering h LineBuffering
      hSetEncoding h utf8
      forever $ do
        eof <- hIsEOF h
        when eof $ fail "alert stdout closed"
        _ <- hGetLine h
        writeChan wakeQueue channel
  case res of
    Left err -> do
      diag $ "  watcher for #" <> channel <> " died: " <> show err
      threadDelay 1_000_000
    Right _ -> pure ()

startAlert :: MusterAgentConfig -> String -> IO (Handle, ProcessHandle)
startAlert cfg channel = do
  let rootArgs = case maBusRoot cfg of
        "" -> []
        r -> ["-r", r]
      args = ["-c", channel] ++ rootArgs ++ [maFilter cfg, maName cfg]
  res <- try @SomeException $
    createProcess
      (proc "muster-alert" args)
        { std_out = CreatePipe,
          std_err = Inherit
        }
  case res of
    Left err -> fail $ "failed to start muster-alert: " <> show err
    Right (Nothing, Just h, Nothing, ph) -> pure (h, ph)
    Right _ -> fail "muster-alert: unexpected process streams"

stopAlert :: (Handle, ProcessHandle) -> IO ()
stopAlert (h, ph) = do
  void $ try @SomeException $ hClose h
  void $ try @SomeException $ terminateProcess ph
  void $ try @SomeException $ waitForProcess ph
  pure ()

runPersistent :: MusterAgentConfig -> IO ()
runPersistent cfg = do
  channels <- discoveredChannels cfg
  case channels of
    [] -> do
      TIO.hPutStrLn stderr "no channels joined for persistent agent — run 'muster join <name> -c <channel>' first"
      exitWith (ExitFailure 1)
    (primary : _) -> do
      TIO.hPutStrLn stderr $ "persistent mode using primary channel #" <> T.pack primary
      project <- projectDir cfg
      let connCfg =
            (defaultConnectorConfig (T.pack (maName cfg)))
              { connChannel = primary,
                connBusRoot = maBusRoot cfg,
                connProject = project,
                connCommand = maAgent cfg,
                connArgs = ["repl"]
              }
      runConnector connCfg

-- ---------------------------------------------------------------------------
-- Meta-commands
-- ---------------------------------------------------------------------------

runMusterJoin :: (String -> IO ()) -> MusterAgentConfig -> String -> IO ()
runMusterJoin diag cfg channel = do
  diag $ "  joining #" <> channel
  void $ try @SomeException $ runMuster cfg ["name", maName cfg]
  void $ try @SomeException $ runMuster cfg ["join", channel]

runMusterLeave :: (String -> IO ()) -> MusterAgentConfig -> String -> IO ()
runMusterLeave diag cfg _channel = do
  diag "  leaving current channel"
  void $ try @SomeException $ runMuster cfg ["leave"]

runMuster :: MusterAgentConfig -> [String] -> IO ()
runMuster cfg args = do
  let rootArg = case maBusRoot cfg of
        "" -> []
        r -> ["--bus-root", r]
      cmd = proc "muster" (rootArg <> args)
  (_, _, _, ph) <- createProcess cmd
  _ <- waitForProcess ph
  pure ()
