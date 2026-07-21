{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Long-lived agent process that participates on a muster bus channel.
--
-- Reads addressed messages from the bus and dispatches them to an external
-- process. Two modes:
--
--   * __oneshot__: runs @\<agent\> chat -q \<prompt\>@ for each addressed message.
--   * __persistent__: keeps a single repl process alive (e.g. @cabal repl@).
--
-- Usage:
--
-- @
--   muster-agent --name deep --agent hermes
--   muster-agent --name cabal --agent cabal --mode persistent --project ~/haskell/circuits -c dev
-- @
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever, unless, when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muster.Agent (AgentConfig (..), AgentMode (..), addressedTo, agentQuery, defaultAgentConfig, stripAddress)
import Muster.Channel (Channel, ChannelConfig (..), channelAttach, channelRecv, channelSend, defaultChannelConfig)
import Muster.Connector (ConnectorConfig (..), defaultConnectorConfig, runConnector)
import Options.Applicative
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (stderr)
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
    maChannel :: String
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
      ( long "channel"
          <> short 'c'
          <> value "bus"
          <> showDefault
          <> metavar "CHANNEL"
          <> help "Muster channel"
      )

optsInfo :: ParserInfo MusterAgentConfig
optsInfo =
  info
    (configParser <**> helper)
    ( fullDesc
        <> progDesc "Long-lived agent on a muster bus channel"
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

channelDir :: MusterAgentConfig -> IO FilePath
channelDir cfg = do
  root <- busRoot cfg
  pure (root </> maChannel cfg)

toChannelConfig :: MusterAgentConfig -> ChannelConfig
toChannelConfig cfg =
  (defaultChannelConfig (T.pack (maName cfg)))
    { chChannel = maChannel cfg,
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
  dir <- channelDir cfg

  logErr $ "muster-agent [" <> maName cfg <> "] starting on " <> dir
  logErr $ "  mode: " <> show (maMode cfg)
  logErr $ "  agent command: " <> maAgent cfg

  fifoExists <- doesFileExist (dir </> "bus.fifo")
  unless fifoExists do
    logErr "bus.fifo not found — run 'muster bus start' first"
    exitWith (ExitFailure 1)

  case maMode cfg of
    OneShot -> runOneShot cfg
    Persistent -> runPersistent cfg

runOneShot :: MusterAgentConfig -> IO ()
runOneShot cfg = do
  let chanCfg = toChannelConfig cfg
      agentCfg = toAgentConfig cfg
  ch <- channelAttach chanCfg
  logErr "attached to bus; waiting for addressed messages"
  _ <- channelRecv ch -- drain backlog
  forever do
    msgs <-
      try @SomeException (channelRecv ch) >>= \case
        Left err -> do
          logErr $ "recv error: " <> show err
          pure []
        Right ms -> pure ms
    if null msgs
      then threadDelay 500_000
      else mapM_ (handleMessage agentCfg ch) msgs

runPersistent :: MusterAgentConfig -> IO ()
runPersistent cfg = do
  project <- projectDir cfg
  let connCfg =
        (defaultConnectorConfig (T.pack (maName cfg)))
          { connChannel = maChannel cfg,
            connBusRoot = maBusRoot cfg,
            connProject = project,
            connCommand = maAgent cfg,
            connArgs = ["repl"]
          }
  runConnector connCfg

handleMessage :: AgentConfig -> Channel -> (Text, Text) -> IO ()
handleMessage cfg ch (sender, body) = do
  let name = T.pack (cfgName cfg)
  when (sender /= name && addressedTo name body) do
    let task = stripAddress name body
    logErr $ "  task: " <> T.unpack (T.take 100 task)
    er <- try @SomeException (agentQuery cfg task)
    case er of
      Left err -> logErr $ "  agent error: " <> show err
      Right reply -> do
        let lines' = filter (not . T.null) $ map T.strip $ T.lines reply
        if null lines'
          then logErr "  (empty agent reply)"
          else
            mapM_
              ( \line -> do
                  logErr $ "  post: " <> T.unpack (T.take 100 line)
                  channelSend ch line
              )
              lines'

logErr :: String -> IO ()
logErr = TIO.hPutStrLn stderr . T.pack
