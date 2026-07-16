{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Long-lived agent process that participates on a muster bus channel.
--
-- Reads addressed messages from the bus, dispatches them to an external
-- agent command (default: @hermes@), and posts the replies back.
--
-- Usage:
--
-- @
--   muster-agent --name deep --agent hermes
-- @
--
-- The agent command is invoked as @\<agent\> chat -q \<prompt\>@. Set
-- @--agent@ to any CLI tool that accepts @-q@ for single-prompt mode and
-- @--continue@ for session resumption (optional — falls back to fresh
-- sessions on failure).
module Main (main, MusterAgentConfig (..), defaultMusterAgentConfig) where

import Circuit.Comm (ChannelConfig (..), attachMusterRepl)
import Circuit.Repl (Repl, replCommit, replEmit)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forever, unless, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
import System.Directory (doesFileExist)
import System.Environment (getEnv, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (stderr)
import System.Process (readCreateProcessWithExitCode, shell)
import Prelude

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

-- | Agent configuration. Everything the agent needs to join a channel and
-- dispatch messages.
data MusterAgentConfig = MusterAgentConfig
  { cfgName :: String
  -- ^ Agent name on the muster bus.
  , cfgModel :: Maybe String
  -- ^ Model override (passed as @-m@ to the agent command).
  , cfgProvider :: Maybe String
  -- ^ Provider override (passed as @--provider@ to the agent command).
  , cfgAgent :: String
  -- ^ Agent CLI command (default: @\"hermes\"@). Invoked as @<agent> chat -q
  -- <prompt>@.
  , cfgBusRoot :: FilePath
  -- ^ Root directory for all bus state (default: @$HOME\/mg\/logs\/muster@).
  , cfgChannel :: String
  -- ^ Channel name (default: @\"general\"@).
  , cfgFifo :: FilePath
  -- ^ FIFO path inside the channel directory (default: @bus.fifo@).
  , cfgLog :: FilePath
  -- ^ Log file path inside the channel directory (default: @log.md@).
  }
  deriving (Show)

-- | Sensible defaults for a muster agent on the local machine.
defaultMusterAgentConfig :: MusterAgentConfig
defaultMusterAgentConfig =
  MusterAgentConfig
    { cfgName = "agent"
    , cfgModel = Nothing
    , cfgProvider = Nothing
    , cfgAgent = "hermes"
    , cfgBusRoot = ""
    , cfgChannel = "general"
    , cfgFifo = "bus.fifo"
    , cfgLog = "log.md"
    }

-- ---------------------------------------------------------------------------
-- CLI (optparse-applicative)
-- ---------------------------------------------------------------------------

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
          <> help "Agent CLI command (invoked as <cmd> chat -q <prompt>)"
      )
    <*> strOption
      ( long "bus-root"
          <> value ""
          <> showDefault
          <> metavar "DIR"
          <> help "Root directory for all bus state (default: $HOME/mg/logs/muster)"
      )
    <*> strOption
      ( long "channel"
          <> short 'c'
          <> value "general"
          <> showDefault
          <> metavar "CHANNEL"
          <> help "Muster channel"
      )
    <*> strOption
      ( long "fifo"
          <> value "bus.fifo"
          <> showDefault
          <> metavar "PATH"
          <> help "FIFO path inside the channel directory"
      )
    <*> strOption
      ( long "log"
          <> value "log.md"
          <> showDefault
          <> metavar "PATH"
          <> help "Log file path inside the channel directory"
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
  let explicit = cfgBusRoot cfg
  if not (null explicit)
    then pure explicit
    else do
      menv <- lookupEnv "MUSTER_ROOT"
      case menv of
        Just d | not (null d) -> pure d
        _ -> (</> "mg/logs/muster") <$> getEnv "HOME"

channelDir :: MusterAgentConfig -> IO FilePath
channelDir cfg = do
  root <- busRoot cfg
  pure (root </> cfgChannel cfg)

toChannelConfig :: MusterAgentConfig -> FilePath -> ChannelConfig
toChannelConfig cfg dir =
  ChannelConfig
    { chStdinPath = dir </> cfgFifo cfg
    , chStdoutPath = dir </> cfgLog cfg
    , chStderrPath = dir </> "err.md"
    , chName = T.pack (cfgName cfg)
    , chWorkingDir = dir
    }

-- ---------------------------------------------------------------------------
-- Agent invocation
-- ---------------------------------------------------------------------------

-- | Query the external agent process. Runs @\<agent\> chat -q \<prompt\>@,
-- attempting session continuation first, falling back to a fresh session.
agentQuery :: MusterAgentConfig -> Text -> IO Text
agentQuery cfg prompt = do
  homeModel <- lookupEnv "HERMES_MODEL"
  homeProv <- lookupEnv "HERMES_PROVIDER"
  let model = fromMaybe (fromMaybe "deepseek-v4-pro" homeModel) (cfgModel cfg)
      provider = fromMaybe (fromMaybe "deepseek" homeProv) (cfgProvider cfg)
      session = cfgName cfg <> "-session"
      q = shellQuote (T.unpack prompt)
      baseArgs =
        [ cfgAgent cfg <> " chat -q"
        , q
        , "-m", shellQuote model
        , "--provider", shellQuote provider
        , "--yolo -Q --max-turns 90"
        ]
      continueArgs = baseArgs <> ["--continue", shellQuote session]
      run args = do
        let cmd = unwords ("export PATH=\"$HOME/.local/bin:$PATH\";" : args)
        (code, out, err) <- readCreateProcessWithExitCode (shell cmd) ""
        pure (code, T.pack out <> T.pack err)
  (code1, out1) <- run continueArgs
  let missing =
        "No session found matching" `T.isInfixOf` out1
          || code1 /= ExitSuccess
  if missing
    then do
      logErr $ "  continue failed (code " <> show code1 <> ") — fresh session: " <> session
      (code2, out2) <- run baseArgs
      when (code2 /= ExitSuccess) $
        logErr $ "  fresh agent also failed (code " <> show code2 <> "): " <> T.unpack (T.take 200 out2)
      pure $ cleanAgentOut out2
    else pure $ cleanAgentOut out1

shellQuote :: String -> String
shellQuote s = "'" <> concatMap esc s <> "'"
  where
    esc '\'' = "'\\''"
    esc c = [c]

cleanAgentOut :: Text -> Text
cleanAgentOut =
  T.unlines
    . filter keep
    . map T.strip
    . T.lines
  where
    keep l
      | T.null l = False
      | "session_id:" `T.isPrefixOf` l = False
      | "Warning:" `T.isPrefixOf` l = False
      | "Resumed session" `T.isInfixOf` l = False
      | "Reached maximum" `T.isInfixOf` l = False
      | "Requesting summary" `T.isInfixOf` l = False
      | "No session found matching" `T.isInfixOf` l = False
      | "Use 'hermes sessions list'" `T.isInfixOf` l = False
      | otherwise = True

-- ---------------------------------------------------------------------------
-- Address matching
-- ---------------------------------------------------------------------------

addressedTo :: Text -> Text -> Bool
addressedTo name body =
  let low = T.toLower body
      n = T.toLower name
   in T.isPrefixOf (n <> ":") low
        || (" " <> n <> ":") `T.isInfixOf` low
        || T.isPrefixOf ("@" <> n) low
        || (" @" <> n) `T.isInfixOf` low

stripAddress :: Text -> Text -> Text
stripAddress name body =
  let n = T.toLower name
      low = T.toLower body
      stripAt i = T.strip $ T.dropWhile (== ' ') $ T.drop i body
      tryPrefix prefix =
        let p = prefix <> ":"
         in case T.breakOn p low of
              (_, r) | not (T.null r) ->
                let i = T.length body - T.length r + T.length p
                 in Just (stripAt i)
              _ -> Nothing
      tryAt =
        let at = "@" <> n
         in case T.breakOn at low of
              (_, r) | not (T.null r) ->
                let i = T.length body - T.length r + T.length at
                 in Just (T.strip $ T.drop i body)
              _ -> Nothing
   in case tryPrefix n of
        Just rest -> rest
        Nothing -> case tryAt of
          Just rest -> rest
          Nothing -> T.strip body

-- ---------------------------------------------------------------------------
-- Main loop
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cfg <- execParser optsInfo
  dir <- channelDir cfg
  let chanCfg = toChannelConfig cfg dir

  logErr $ "muster-agent [" <> cfgName cfg <> "] starting on " <> dir
  logErr $ "  agent command: " <> cfgAgent cfg

  fifoExists <- doesFileExist (chStdinPath chanCfg)
  unless fifoExists do
    logErr "bus.fifo not found — run 'muster bus start' first"
    exitWith (ExitFailure 1)

  repl <- attachMusterRepl chanCfg
  logErr "attached to bus; waiting for addressed messages"

  -- Drain backlog so we only react to new traffic.
  _ <- replEmit repl

  forever do
    msgs <-
      try @SomeException (replEmit repl) >>= \case
        Left err -> do
          logErr $ "recv error: " <> show err
          pure []
        Right ms -> pure ms

    if null msgs
      then threadDelay 500_000
      else mapM_ (handleMessage cfg repl) msgs

handleMessage :: MusterAgentConfig -> Repl -> Text -> IO ()
handleMessage cfg repl body = do
  let name = T.pack (cfgName cfg)
  when (addressedTo name body) do
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
            mapM_ (\line -> do
              logErr $ "  post: " <> T.unpack (T.take 100 line)
              replCommit repl [line]) lines'

logErr :: String -> IO ()
logErr = TIO.hPutStrLn stderr . T.pack