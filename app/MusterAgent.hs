{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Long-lived agent process that participates on one or more muster bus
-- channels.
--
-- Agents are symmetric to human participants: they are joined to channels
-- explicitly via `muster join <agent> -c <channel>`.  The agent scans the
-- bus root for `.cursor-<name>` files and attaches to every channel where it
-- has been joined.
--
-- Two modes:
--
--   * __oneshot__: runs @\<agent\> chat -q \<prompt\>@ for each addressed message.
--   * __persistent__: keeps a single repl process alive (e.g. @cabal repl@)
--     on the agent's primary/default channel.
--
-- Usage:
--
-- @
--   muster-agent --name deep --agent hermes
--   muster-agent --name cabal --agent cabal --mode persistent --project ~/haskell/circuits
-- @
module Main (main) where

import Control.Concurrent (Chan, newChan, readChan, threadDelay, writeChan)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (foldM, forever, unless, void, when)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muster.Agent (AgentConfig (..), AgentMode (..), addressedTo, agentQuery, defaultAgentConfig, stripAddress)
import Muster.Channel (Channel, ChannelConfig (..), channelAttach, channelRecv, channelSend, defaultChannelConfig)
import Muster.Connector (ConnectorConfig (..), defaultConnectorConfig, runConnector)
import Options.Applicative
import System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory)
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
    maBusRoot :: FilePath
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
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cfg <- execParser optsInfo

  logErr $ "muster-agent [" <> maName cfg <> "] starting"
  logErr $ "  mode: " <> show (maMode cfg)
  logErr $ "  agent command: " <> maAgent cfg

  case maMode cfg of
    OneShot -> runOneShot cfg
    Persistent -> runPersistent cfg

runOneShot :: MusterAgentConfig -> IO ()
runOneShot cfg = do
  let agentCfg = toAgentConfig cfg
  wakeQueue <- newChan
  logErr "scanning for joined channels..."
  let go :: Map String Channel -> Map String (Async ()) -> IO ()
      go channels watchers = do
        newChannels <- refreshChannels cfg channels
        newWatchers <- refreshWatchers cfg wakeQueue watchers newChannels
        channelName <- readChan wakeQueue
        case Map.lookup channelName newChannels of
          Nothing -> go newChannels newWatchers
          Just ch -> do
            msgs <- channelRecv ch
            unless (null msgs) $
              mapM_ (\(s, b) -> handleMessage agentCfg cfg (channelName, s, b)) msgs
            go newChannels newWatchers
  go Map.empty Map.empty

-- | Refresh the set of attached channels.  Returns a map from channel name to
-- open handle, attaching to any newly-joined channels and dropping any whose
-- cursor files have disappeared.
refreshChannels :: MusterAgentConfig -> Map String Channel -> IO (Map String Channel)
refreshChannels cfg old = do
  names <- discoveredChannels cfg
  let addNew acc name =
        case Map.lookup name acc of
          Just _ -> pure acc
          Nothing -> do
            let chanCfg = toChannelConfig cfg name
            res <- try @SomeException (channelAttach chanCfg)
            case res of
              Left err -> do
                logErr $ "  failed to attach to #" <> name <> ": " <> show err
                pure acc
              Right ch -> do
                logErr $ "  joined #" <> name
                -- drain backlog so we don't react to old messages
                _ <- channelRecv ch
                pure (Map.insert name ch acc)
  added <- foldM addNew old names
  -- Remove channels whose cursor disappeared
  let current = Set.fromList names
      stale = Map.keysSet added `Set.difference` current
  unless (Set.null stale) $
    logErr $ "  left channels: " <> unwords (Set.toList stale)
  pure (foldr Map.delete added (Set.toList stale))

-- | Start alert watchers for newly-joined channels and cancel watchers for
-- channels that have been left.  Each watcher runs @muster-alert -c <channel>
-- -l "[" <name>@ in a loop, signalling the main thread on every post.
refreshWatchers ::
  MusterAgentConfig ->
  Chan String ->
  Map String (Async ()) ->
  Map String Channel ->
  IO (Map String (Async ()))
refreshWatchers cfg wakeQueue old channels = do
  let current = Set.fromList (Map.keys channels)
      stale = Map.keysSet old `Set.difference` current
      newNames = Set.toList (current `Set.difference` Map.keysSet old)
  mapM_ cancel (Map.elems (Map.filterWithKey (\k _ -> Set.member k stale) old))
  added <- mapM (\name -> (name,) <$> async (alertWatcher cfg wakeQueue name)) newNames
  pure $ Map.union (Map.difference old (Map.fromSet (const ()) stale)) (Map.fromList added)

alertWatcher :: MusterAgentConfig -> Chan String -> String -> IO ()
alertWatcher cfg wakeQueue channel = forever $ do
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
      logErr $ "  watcher for #" <> channel <> " died: " <> show err
      threadDelay 1_000_000
    Right _ -> pure ()

startAlert :: MusterAgentConfig -> String -> IO (Handle, ProcessHandle)
startAlert cfg channel = do
  -- The filter "[" matches every framed bus line. The watcher exits on
  -- the first new message, the agent wakes up and re-arms it.
  let rootArgs = case maBusRoot cfg of
        "" -> []
        r -> ["-r", r]
      args = ["-c", channel] ++ rootArgs ++ ["[", maName cfg]
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
      logErr "no channels joined for persistent agent — run 'muster join <name> -c <channel>' first"
      exitWith (ExitFailure 1)
    (primary : _) -> do
      logErr $ "persistent mode using primary channel #" <> primary
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

handleMessage :: AgentConfig -> MusterAgentConfig -> (String, Text, Text) -> IO ()
handleMessage agentCfg cfg (channel, sender, body) = do
  let name = T.pack (maName cfg)
  when (sender /= name && addressedTo name body) do
    let task = stripAddress name body
    logErr $ "  task on #" <> channel <> ": " <> T.unpack (T.take 100 task)
    -- Special meta-commands: agent can join/leave channels on request.
    case parseMetaCommand task of
      Just (Join c) -> runMusterJoin cfg c
      Just (Leave c) -> runMusterLeave cfg c
      Nothing -> do
        er <- try @SomeException (agentQuery agentCfg task)
        case er of
          Left err -> logErr $ "  agent error: " <> show err
          Right reply -> do
            channels <- discoveredChannels cfg
            case Map.lookup channel (Map.fromList [(c, ()) | c <- channels]) of
              Nothing -> logErr "  channel disappeared before reply"
              Just () -> do
                let chCfg = toChannelConfig cfg channel
                res <- try @SomeException (channelAttach chCfg)
                case res of
                  Left err -> logErr $ "  failed to re-attach to #" <> channel <> ": " <> show err
                  Right ch -> do
                    let lines' = filter (not . T.null) $ map T.strip $ T.lines reply
                    if null lines'
                      then logErr "  (empty agent reply)"
                      else
                        mapM_
                          ( \line -> do
                              logErr $ "  post to #" <> channel <> ": " <> T.unpack (T.take 100 line)
                              channelSend ch line
                          )
                          lines'

-- ---------------------------------------------------------------------------
-- Meta-commands
-- ---------------------------------------------------------------------------

data MetaCommand = Join String | Leave String

parseMetaCommand :: Text -> Maybe MetaCommand
parseMetaCommand t =
  let low = T.toLower $ T.strip t
      stripHash c = fromMaybe c (T.stripPrefix "#" c)
   in case T.words low of
        ["join", c] -> Just (Join (T.unpack (stripHash c)))
        ["leave", c] -> Just (Leave (T.unpack (stripHash c)))
        _ -> Nothing

runMusterJoin :: MusterAgentConfig -> String -> IO ()
runMusterJoin cfg channel = do
  logErr $ "  joining #" <> channel
  void $ try @SomeException $ runMuster cfg channel ["join", maName cfg]

runMusterLeave :: MusterAgentConfig -> String -> IO ()
runMusterLeave cfg channel = do
  logErr $ "  leaving #" <> channel
  void $ try @SomeException $ runMuster cfg channel ["leave", maName cfg]

runMuster :: MusterAgentConfig -> String -> [String] -> IO ()
runMuster cfg channel args = do
  let rootArg = case maBusRoot cfg of
        "" -> []
        r -> ["--bus-root", r]
      allArgs = ["-c", channel] <> rootArg <> args
      cmd = proc "muster" allArgs
  (_, _, _, ph) <- createProcess cmd
  _ <- waitForProcess ph
  pure ()

logErr :: String -> IO ()
logErr = TIO.hPutStrLn stderr . T.pack
