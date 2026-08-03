{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | L4c: detached live-runner experiment.
--
-- Starts a muster bus and two hermes agents in the background, posts a card,
-- then exits so the human can observe.  Commands:
--
--   muster-runner-live start --root DIR --card FILE
--   muster-runner-live status --root DIR
--   muster-runner-live retry --root DIR
--   muster-runner-live stop --root DIR
--
-- The agents share a wake filter ("WAKE-RUNNER-LIVE") so only seeded prompts
-- wake them; their replies do not re-trigger each other.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muster.Api.Agent
  ( AgentConfig (..),
    agentState,
    listAgents,
    startAgent,
    stopAgent,
    writeAgentConfig,
  )
import Muster.Api.Agent qualified as Agent
import Muster.Api.Bus
  ( busRootPath,
    ensureChannel,
    isRunning,
    post,
    readTail,
    stopCentral,
  )
import Muster.Api.Bus qualified as Bus
import Muster.Api.Types
  ( AgentState (..),
    AgentStatus (..),
    BusRoot (..),
    Channel (..),
    Nick (..),
  )
import Muster.Bus qualified as BusEngine
import Options.Applicative
import System.Directory (createDirectoryIfMissing, findExecutable, removePathForcibly)
import System.Process (callProcess)
import Prelude

-- ---------------------------------------------------------------------------
-- CLI
-- ---------------------------------------------------------------------------

data GlobalOpts = GlobalOpts
  { optRoot :: FilePath,
    optChannel :: Text,
    optAlpha :: Text,
    optBeta :: Text
  }
  deriving (Show)

globalParser :: Parser GlobalOpts
globalParser =
  GlobalOpts
    <$> strOption
      ( long "root"
          <> short 'r'
          <> metavar "DIR"
          <> value "/tmp/muster-runner-live"
          <> showDefault
          <> help "Isolated bus root for this experiment"
      )
    <*> strOption
      ( long "channel"
          <> short 'c'
          <> metavar "NAME"
          <> value "lab"
          <> showDefault
          <> help "Shared channel name"
      )
    <*> strOption
      ( long "alpha"
          <> metavar "NAME"
          <> value "alpha"
          <> showDefault
          <> help "Name of the first agent"
      )
    <*> strOption
      ( long "beta"
          <> metavar "NAME"
          <> value "beta"
          <> showDefault
          <> help "Name of the second agent"
      )

data StartOpts = StartOpts
  { startGlobal :: GlobalOpts,
    startCard :: FilePath
  }
  deriving (Show)

type StatusOpts = GlobalOpts

type RetryOpts = GlobalOpts

type StopOpts = GlobalOpts

data Cmd = CmdStart StartOpts | CmdStatus StatusOpts | CmdRetry RetryOpts | CmdStop StopOpts

cmdParser :: Parser Cmd
cmdParser =
  subparser
    ( command
        "start"
        ( info
            (CmdStart <$> (StartOpts <$> globalParser <*> cardOption) <**> helper)
            (progDesc "Start the bus, agents, and post the card; then exit")
        )
        <> command
          "status"
          ( info
              (CmdStatus <$> globalParser <**> helper)
              (progDesc "Observe the experiment: bus health, agents, log tail")
          )
        <> command
          "retry"
          ( info
              (CmdRetry <$> globalParser <**> helper)
              (progDesc "Post a retry nudge to both agents")
          )
        <> command
          "stop"
          ( info
              (CmdStop <$> globalParser <**> helper)
              (progDesc "Stop agents and the bus")
          )
    )
  where
    cardOption =
      strOption
        ( long "card"
            <> short 'f'
            <> metavar "FILE"
            <> value "/Users/tonyday567/coffee/loom/runner.md"
            <> showDefault
            <> help "Card to post as the initial prompt"
        )

optsInfo :: ParserInfo Cmd
optsInfo =
  info
    (cmdParser <**> helper)
    ( fullDesc
        <> progDesc "Detached runner experiment: start agents, get out of the way, observe later"
        <> header "muster-runner-live — live agent lab bench"
    )

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

wakeToken :: Text
wakeToken = "WAKE-RUNNER-LIVE"

bus :: GlobalOpts -> BusRoot
bus = BusRoot . optRoot

chan :: GlobalOpts -> Channel
chan = Channel . optChannel

withNick :: Text -> Nick
withNick = Nick

agentCfg :: GlobalOpts -> Text -> AgentConfig
agentCfg opts name =
  AgentConfig
    { agentCommand = "muster-agent",
      agentArgs =
        [ "--name",
          T.unpack name,
          "--bus-root",
          busRootPath (bus opts),
          "--filter",
          T.unpack wakeToken
        ]
    }

joinChannel :: BusRoot -> Channel -> Nick -> IO ()
joinChannel root c nick = do
  dir <- ensureChannel root c
  n <- BusEngine.countLogLines dir
  writeFile (Bus.cursorPath root c nick) (show n <> "\n")

waitForBus :: BusRoot -> Int -> IO Bool
waitForBus _ 0 = pure False
waitForBus root n = do
  r <- isRunning root
  if r then pure True else threadDelay 100_000 >> waitForBus root (n - 1)

senderOfLine :: Text -> Maybe Text
senderOfLine l =
  let afterBracket = T.drop 1 (T.dropWhile (/= ']') l)
      afterSpace = T.stripStart (T.drop 1 afterBracket)
   in case T.breakOn ":" afterSpace of
        (name, _) | not (T.null name) -> Just name
        _ -> Nothing

replyCounts :: [Text] -> Text -> Text -> (Int, Int)
replyCounts ls alpha beta =
  let m = Map.fromListWith (+) [(name, 1) | l <- ls, Just name <- [senderOfLine l]]
   in (Map.findWithDefault 0 alpha m, Map.findWithDefault 0 beta m)

statusString :: AgentStatus -> String
statusString Running = "running"
statusString Stopped = "stopped"
statusString Unknown = "unknown"
statusString Unavailable = "unavailable"

-- ---------------------------------------------------------------------------
-- Commands
-- ---------------------------------------------------------------------------

runStart :: StartOpts -> IO ()
runStart StartOpts {..} = do
  let opts = startGlobal
      root = bus opts
      c = chan opts
      alpha = optAlpha opts
      beta = optBeta opts

  -- Clean, isolated state.
  _ <- try @SomeException (removePathForcibly (unBusRoot root))
  createDirectoryIfMissing True (unBusRoot root)

  putStrLn "muster-runner-live: starting detached experiment"
  musterExe <- fromMaybe "muster" <$> findExecutable "muster"
  callProcess musterExe ["--bus-root", optRoot opts, "bus", "start"]
  running <- waitForBus root 30
  unless running $ fail "bus failed to start"

  -- Ensure shared channel exists before agents arrive.
  void $ ensureChannel root c

  forM_ [alpha, beta] $ \name -> do
    let nick = withNick name
    er <- Agent.newAgent root (Just name)
    case er of
      Left err -> putStrLn $ "  warn creating " <> T.unpack name <> ": " <> T.unpack err
      Right _ -> pure ()
    writeAgentConfig root nick (agentCfg opts name)
    joinChannel root c nick
    res <- startAgent root nick
    case res of
      Left err -> putStrLn $ "  warn starting " <> T.unpack name <> ": " <> T.unpack err
      Right () -> putStrLn $ "  started agent " <> T.unpack name

  cardText <- TIO.readFile startCard
  -- Keep the prompt as a single log line; newlines in the card become spaces.
  let oneLine = T.unwords (T.lines cardText)
      prompt = wakeToken <> " @" <> alpha <> " @" <> beta <> " " <> oneLine
  post root c (Nick "runner") prompt

  putStrLn "  posted initial card"
  putStrLn $ "  observe with: cabal exec muster-runner-live -- status --root " <> optRoot opts
  putStrLn $ "  stop with:    cabal exec muster-runner-live -- stop --root " <> optRoot opts

runStatus :: StatusOpts -> IO ()
runStatus opts = do
  let root = bus opts
      c = chan opts
      alpha = optAlpha opts
      beta = optBeta opts

  running <- isRunning root
  putStrLn $ "bus: " <> if running then "running" else "down"

  agents <- listAgents root
  forM_ agents $ \nick -> do
    mst <- agentState root nick
    case mst of
      Nothing -> putStrLn $ "agent " <> T.unpack (unNick nick) <> ": unknown"
      Just st ->
        putStrLn $
          "agent " <> T.unpack (unNick nick) <> ": " <> statusString (agentStatus st)

  ls <- readTail root c (Nick "observer") 20
  putStrLn "--- log tail ---"
  mapM_ TIO.putStrLn ls

  let (aCount, bCount) = replyCounts ls alpha beta
  putStrLn $ "alpha replies: " <> show aCount
  putStrLn $ "beta replies:  " <> show bCount

runRetry :: RetryOpts -> IO ()
runRetry opts = do
  let root = bus opts
      c = chan opts
      alpha = optAlpha opts
      beta = optBeta opts
      prompt =
        wakeToken
          <> " @"
          <> alpha
          <> " @"
          <> beta
          <> " retry: reply to the card with a runner mark. One line only."
  post root c (Nick "runner") prompt
  putStrLn "retry nudge posted"

runStop :: StopOpts -> IO ()
runStop opts = do
  let root = bus opts
  agents <- listAgents root
  forM_ agents $ \nick -> do
    void $ stopAgent root nick
    putStrLn $ "stopped agent " <> T.unpack (unNick nick)
  stopCentral root
  putStrLn "bus stopped"

main :: IO ()
main = do
  cmd <- execParser optsInfo
  case cmd of
    CmdStart o -> runStart o
    CmdStatus o -> runStatus o
    CmdRetry o -> runRetry o
    CmdStop o -> runStop o
