{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haskell-native agent bridge for muster.
--
-- Replaces the Python 'agent_bridge.py' + 'coord_server.py' pair. The agent
-- participates directly on the muster bus via 'Circuit.Comm.attachMusterRepl'
-- and runs 'hermes chat -q' per inbound addressed message.
--
-- Usage:
--
-- @
--   muster-agent --name deep --model deepseek-v4-pro --provider deepseek
-- @
module Main (main) where

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
-- CLI
-- ---------------------------------------------------------------------------

data AgentOpts = AgentOpts
  { optName :: String,
    optModel :: Maybe String,
    optProvider :: Maybe String,
    optBusRoot :: FilePath,
    optChannel :: String
  }
  deriving (Show)

agentOptsParser :: Parser AgentOpts
agentOptsParser =
  AgentOpts
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
              <> help "Hermes model (default: env HERMES_MODEL or deepseek-v4-pro)"
          )
      )
    <*> optional
      ( strOption
          ( long "provider"
              <> metavar "PROVIDER"
              <> help "Hermes provider (default: env HERMES_PROVIDER or deepseek)"
          )
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

optsInfo :: ParserInfo AgentOpts
optsInfo =
  info
    (agentOptsParser <**> helper)
    ( fullDesc
        <> progDesc "Long-lived muster agent bridge"
        <> header "muster-agent — participate on the muster bus via circuits-repl"
    )

-- ---------------------------------------------------------------------------
-- Paths
-- ---------------------------------------------------------------------------

busRoot :: AgentOpts -> IO FilePath
busRoot opts = do
  let explicit = optBusRoot opts
  if not (null explicit)
    then pure explicit
    else do
      menv <- lookupEnv "MUSTER_ROOT"
      case menv of
        Just d | not (null d) -> pure d
        _ -> (</> "mg/logs/muster") <$> getEnv "HOME"

channelDir :: AgentOpts -> IO FilePath
channelDir opts = do
  root <- busRoot opts
  pure (root </> optChannel opts)

-- ---------------------------------------------------------------------------
-- Hermes invocation
-- ---------------------------------------------------------------------------

hermesQuery :: AgentOpts -> Text -> IO Text
hermesQuery opts prompt = do
  homeModel <- lookupEnv "HERMES_MODEL"
  homeProv <- lookupEnv "HERMES_PROVIDER"
  let model = fromMaybe (fromMaybe "deepseek-v4-pro" homeModel) (optModel opts)
      provider = fromMaybe (fromMaybe "deepseek" homeProv) (optProvider opts)
      session = optName opts <> "-session"
      q = shellQuote (T.unpack prompt)
      baseArgs =
        [ "hermes chat -q",
          q,
          "-m",
          shellQuote model,
          "--provider",
          shellQuote provider,
          "--yolo -Q --max-turns 90"
        ]
      continueArgs = baseArgs ++ ["--continue", shellQuote session]
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
        logErr $ "  fresh hermes also failed (code " <> show code2 <> "): " <> T.unpack (T.take 200 out2)
      pure $ cleanHermesOut out2
    else pure $ cleanHermesOut out1

shellQuote :: String -> String
shellQuote s = "'" <> concatMap esc s <> "'"
  where
    esc '\'' = "'\\''"
    esc c = [c]

cleanHermesOut :: Text -> Text
cleanHermesOut =
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
-- Address matching (parity with Python coord_server / agent_bridge)
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
  opts <- execParser optsInfo
  dir <- channelDir opts
  let name = T.pack (optName opts)
      cfg =
        ChannelConfig
          { chStdinPath = dir </> "bus.fifo",
            chStdoutPath = dir </> "log.md",
            chStderrPath = dir </> "err.md",
            chName = name,
            chWorkingDir = dir
          }

  logErr $ "muster-agent [" <> optName opts <> "] starting on " <> dir

  -- Verify the bus is reachable before attaching.
  fifoExists <- doesFileExist (chStdinPath cfg)
  unless fifoExists do
    logErr "bus.fifo not found — run 'muster bus start' first"
    exitWith (ExitFailure 1)

  repl <- attachMusterRepl cfg
  logErr "attached to bus; waiting for addressed messages"

  -- Drain any backlog so we only react to new traffic.
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
      else mapM_ (handleMessage opts repl) msgs

handleMessage :: AgentOpts -> Repl -> Text -> IO ()
handleMessage opts repl body = do
  let name = T.pack (optName opts)
  when (addressedTo name body) do
    let task = stripAddress name body
    logErr $ "  task: " <> T.unpack (T.take 100 task)
    -- Never let a hermes failure kill the long-lived agent process.
    er <- try @SomeException (hermesQuery opts task)
    case er of
      Left err -> logErr $ "  hermes error: " <> show err
      Right reply -> do
        let lines' = filter (not . T.null) $ map T.strip $ T.lines reply
        if null lines'
          then logErr "  (empty hermes reply)"
          else
            mapM_ (\line -> do
              logErr $ "  post: " <> T.unpack (T.take 100 line)
              replCommit repl [line]) lines'

logErr :: String -> IO ()
logErr = TIO.hPutStrLn stderr . T.pack
