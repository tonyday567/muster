{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | New muster CLI implementing the locked API from
-- @loom/muster-orchestrator.md@.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, try)
import Control.Monad (void, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muster.Api.Agent qualified as Ag
import Muster.Api.Bus qualified as Bus
import Muster.Api.Orchestrator qualified as Orc
import Muster.Api.Participant qualified as Part
import Muster.Api.Types (AgentState (..), BusRoot (..), Channel (..), Nick (..), unBusRoot)
import Muster.Cli.Opts
  ( busRootOpt,
    channelOpt,
    devOpt,
    nameOpt,
    portOpt,
    resolveBusRoot,
  )
import Options.Applicative
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (CPid (..))
import System.Process (StdStream (..), createProcess, proc, std_in, waitForProcess)
import Text.Read (readMaybe)
import Prelude

-- ---------------------------------------------------------------------------
-- Global options — same vocabulary as muster-ws / deck
-- ---------------------------------------------------------------------------

data Global = Global
  { globalBusRoot :: FilePath,
    globalChannel :: String,
    globalName :: String
  }
  deriving (Show)

globalParser :: Parser Global
globalParser =
  Global
    <$> busRootOpt
    <*> channelOpt
    <*> nameOpt "deck"

resolveRoot :: Global -> IO BusRoot
resolveRoot g = BusRoot <$> resolveBusRoot (globalBusRoot g)

-- ---------------------------------------------------------------------------
-- Commands
-- ---------------------------------------------------------------------------

data ParticipantCmd
  = Name Text
  | Join Channel
  | Leave
  | Post Text
  | ReadNext
  | ReadTail (Maybe Int)
  deriving (Show)

data AgentCmd
  = AgentNew (Maybe Text)
  | AgentStart [Nick]
  | AgentStop [Nick]
  | AgentQuit [Nick]
  | AgentRename Nick Nick
  | AgentList
  deriving (Show)

data BusCmd = BusStart | BusStop | BusStatus deriving (Show)

-- | Deck process options (port / dev). Channel, name, bus-root come from 'Global'.
data DeckCmd
  = DeckStart Int Bool
  | DeckStop
  | DeckStatus
  deriving (Show)

data Cmd
  = CmdParticipant ParticipantCmd
  | CmdAgent AgentCmd
  | CmdPs
  | CmdStatus (Maybe Nick)
  | CmdTell Nick Text
  | CmdPing (Maybe Nick)
  | CmdWatch (Maybe Nick) Bool
  | CmdBus BusCmd
  | CmdDeck DeckCmd
  | CmdInternalDaemon
  deriving (Show)

nickArg :: String -> Parser Nick
nickArg meta = Nick . T.pack <$> argument str (metavar meta)

optionalNickArg :: String -> Parser (Maybe Nick)
optionalNickArg meta = optional $ Nick . T.pack <$> argument str (metavar meta)

channelArg :: Parser Channel
channelArg = Channel . T.pack <$> argument str (metavar "CHANNEL")

messageArgs :: Parser Text
messageArgs = T.unwords . map T.pack <$> some (argument str (metavar "MESSAGE"))

namesParser :: Parser [Nick]
namesParser = some (nickArg "NAME")

agentCmdParser :: Parser AgentCmd
agentCmdParser =
  hsubparser
    ( command "new" (info (AgentNew . fmap T.pack <$> optional (argument str (metavar "NAME"))) (progDesc "Create a new agent (auto-name if omitted)"))
        <> command "start" (info (AgentStart <$> namesParser) (progDesc "Start/resume one or more agents"))
        <> command "stop" (info (AgentStop <$> namesParser) (progDesc "Stop one or more agents"))
        <> command "quit" (info (AgentQuit <$> namesParser) (progDesc "Tear down one or more agents"))
        <> command "rename" (info (AgentRename <$> nickArg "OLD" <*> nickArg "NEW") (progDesc "Rename an agent"))
        <> command "list" (info (pure AgentList) (progDesc "List agents"))
    )

busCmdParser :: Parser BusCmd
busCmdParser =
  hsubparser
    ( command "start" (info (pure BusStart) (progDesc "Start the central bus daemon"))
        <> command "stop" (info (pure BusStop) (progDesc "Stop the central bus daemon"))
        <> command "status" (info (pure BusStatus) (progDesc "Report bus daemon status"))
    )

deckCmdParser :: Parser DeckCmd
deckCmdParser =
  hsubparser
    ( command
        "start"
        ( info
            (DeckStart <$> portOpt 9162 <*> devOpt)
            (progDesc "Start the deck web UI (uses global -c/-n/--bus-root)")
        )
        <> command "stop" (info (pure DeckStop) (progDesc "Stop the deck web UI"))
        <> command "status" (info (pure DeckStatus) (progDesc "Report deck health"))
    )

cmdParser :: Parser Cmd
cmdParser =
  hsubparser
    ( command "name" (info (CmdParticipant . Name . T.pack <$> argument str (metavar "NICK")) (progDesc "Set your nick"))
        <> command "join" (info (CmdParticipant . Join <$> channelArg) (progDesc "Join a channel"))
        <> command "leave" (info (pure (CmdParticipant Leave)) (progDesc "Leave the current channel"))
        <> command "post" (info (CmdParticipant . Post <$> messageArgs) (progDesc "Post to the current channel"))
        <> command "read-next" (info (pure (CmdParticipant ReadNext)) (progDesc "Read unread messages since your cursor"))
        <> command "read-tail" (info (CmdParticipant . ReadTail <$> optional (argument auto (metavar "N"))) (progDesc "Read last N lines (default 20)"))
        <> command "agent" (info (CmdAgent <$> agentCmdParser) (progDesc "Agent lifecycle"))
        <> command "ps" (info (pure CmdPs) (progDesc "List agents"))
        <> command "status" (info (CmdStatus <$> optionalNickArg "NAME") (progDesc "Show status"))
        <> command "tell" (info (CmdTell <$> nickArg "NAME" <*> messageArgs) (progDesc "Post to an agent's channel"))
        <> command "ping" (info (CmdPing <$> optionalNickArg "NAME") (progDesc "Ping an agent"))
        <> command "watch" (info (CmdWatch <$> optionalNickArg "NAME" <*> switch (long "loop" <> help "Loop continuously")) (progDesc "Watch for addressed messages"))
        <> command "bus" (info (CmdBus <$> busCmdParser) (progDesc "Central bus daemon lifecycle"))
        <> command "deck" (info (CmdDeck <$> deckCmdParser) (progDesc "Deck web UI"))
        <> command "internal-daemon" (info (pure CmdInternalDaemon) (progDesc "Internal: run the central daemon"))
    )

optionsParser :: ParserInfo (Global, Cmd)
optionsParser =
  info
    ((,) <$> globalParser <*> cmdParser <**> helper)
    ( fullDesc
        <> progDesc "muster: closed comms environment for agent pools"
        <> header "muster — name, join, post, ping"
    )

-- ---------------------------------------------------------------------------
-- Execution
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  (global, cmd) <- execParser optionsParser
  root <- resolveRoot global
  runCmd global root cmd

runCmd :: Global -> BusRoot -> Cmd -> IO ()
runCmd global root = \case
  CmdParticipant c -> runParticipant root c
  CmdAgent c -> runAgent root c
  CmdPs -> do
    states <- Orc.ps root
    if null states
      then putStrLn "no agents"
      else mapM_ (TIO.putStrLn . formatAgentState) states
  CmdStatus mName -> Orc.status root mName >>= TIO.putStrLn
  CmdTell name msg -> printEither =<< Orc.tell root name msg
  CmdPing mName -> printEither =<< Orc.ping root mName
  CmdWatch mName loop -> printEither =<< Orc.watchLoop root mName loop
  CmdBus c -> runBus root c
  CmdDeck c -> runDeck global root c
  CmdInternalDaemon -> Bus.centralDaemon root

runParticipant :: BusRoot -> ParticipantCmd -> IO ()
runParticipant root = \case
  Name raw -> do
    r <- Part.setName root raw
    printEither' (unNick <$> r)
  Join chan -> printEither =<< Part.joinChannel root chan
  Leave -> Part.leaveChannel root >> putStrLn "left"
  Post body -> printEither =<< Part.post root body
  ReadNext -> do
    e <- Part.readNext root
    case e of
      Left err -> TIO.hPutStrLn stderr err >> exitFailure
      Right ls -> mapM_ TIO.putStrLn ls
  ReadTail mn -> do
    let n = fromMaybe 20 mn
    e <- Part.readTail root n
    case e of
      Left err -> TIO.hPutStrLn stderr err >> exitFailure
      Right ls -> mapM_ TIO.putStrLn ls

runAgent :: BusRoot -> AgentCmd -> IO ()
runAgent root = \case
  AgentNew mName -> printEither' . fmap unNick =<< Ag.newAgent root mName
  AgentStart names -> mapM_ (\n -> printEither =<< Ag.startAgent root n) names
  AgentStop names -> mapM_ (\n -> printEither =<< Ag.stopAgent root n) names
  AgentQuit names -> mapM_ (\n -> printEither =<< Ag.quitAgent root n) names
  AgentRename old new -> printEither =<< Ag.renameAgent root old new
  AgentList -> do
    names <- Ag.listAgents root
    if null names
      then putStrLn "no agents"
      else mapM_ (TIO.putStrLn . unNick) names

runBus :: BusRoot -> BusCmd -> IO ()
runBus root = \case
  BusStart -> do
    ok <- Bus.startCentral root
    putStrLn $ if ok then "bus started" else "bus failed to start"
  BusStop -> Bus.stopCentral root >> putStrLn "bus stopped"
  BusStatus -> Bus.statusCentral root

-- ---------------------------------------------------------------------------
-- Deck lifecycle — spawns muster-ws with the same globals
-- ---------------------------------------------------------------------------

deckPidFile :: FilePath -> FilePath
deckPidFile root = root </> "deck.pid"

deckLogFile :: FilePath -> FilePath
deckLogFile root = root </> "deck.log"

readPidInt :: FilePath -> IO (Maybe Int)
readPidInt path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      raw <- readFile path
      pure $ readMaybe (filter (/= '\n') raw)

pidAlive :: Int -> IO Bool
pidAlive p =
  (True <$ signalProcess nullSignal (CPid (fromIntegral p)))
    `catch` (\(_ :: IOError) -> pure False)

killPid :: Int -> IO ()
killPid p = void $ (try (signalProcess sigterm (CPid (fromIntegral p))) :: IO (Either IOError ()))
  where
    sigterm = toEnum 15 -- SIGTERM

runDeck :: Global -> BusRoot -> DeckCmd -> IO ()
runDeck global root = \case
  DeckStart port dev -> runDeckStart global root port dev
  DeckStop -> runDeckStop root
  DeckStatus -> runDeckStatus root

runDeckStart :: Global -> BusRoot -> Int -> Bool -> IO ()
runDeckStart global root port dev = do
  let rootPath = unBusRoot root
  createDirectoryIfMissing True rootPath
  let pidPath = deckPidFile rootPath
      logPath = deckLogFile rootPath
  mpid <- readPidInt pidPath
  case mpid of
    Just p -> do
      alive <- pidAlive p
      when alive do
        hPutStrLn stderr $ "muster: deck already running (pid " <> show p <> ")"
        exitWith (ExitFailure 1)
    Nothing -> pure ()
  let args =
        [ "--name",
          globalName global,
          "--channel",
          globalChannel global,
          "--port",
          show port,
          "--bus-root",
          rootPath
        ]
          <> [ "--dev" | dev ]
      sh =
        "nohup muster-ws "
          <> unwords (map shellQuote args)
          <> " >> "
          <> shellQuote logPath
          <> " 2>&1 & echo $! > "
          <> shellQuote pidPath
  (_, _, _, ph) <-
    createProcess
      (proc "/bin/sh" ["-c", sh]) {std_in = NoStream}
  void $ waitForProcess ph
  threadDelay 500000
  mpid' <- readPidInt pidPath
  case mpid' of
    Nothing -> do
      hPutStrLn stderr $ "muster: deck pid not written — see " <> logPath
      exitWith (ExitFailure 1)
    Just p -> do
      alive <- pidAlive p
      if alive
        then
          putStrLn $
            "deck started on http://127.0.0.1:"
              <> show port
              <> " as "
              <> globalName global
              <> " -c "
              <> globalChannel global
              <> " (pid "
              <> show p
              <> ")"
        else do
          hPutStrLn stderr $ "muster: deck died immediately — see " <> logPath
          exitWith (ExitFailure 1)

runDeckStop :: BusRoot -> IO ()
runDeckStop root = do
  let rootPath = unBusRoot root
      pidPath = deckPidFile rootPath
  mpid <- readPidInt pidPath
  case mpid of
    Nothing -> putStrLn "no deck running"
    Just p -> do
      alive <- pidAlive p
      if alive
        then do
          killPid p
          removeFile pidPath
          putStrLn $ "deck stopped (was pid " <> show p <> ")"
        else do
          removeFile pidPath
          putStrLn $ "deck stale pid " <> show p <> " removed"

runDeckStatus :: BusRoot -> IO ()
runDeckStatus root = do
  let rootPath = unBusRoot root
      pidPath = deckPidFile rootPath
  mpid <- readPidInt pidPath
  case mpid of
    Nothing -> putStrLn "no deck"
    Just p -> do
      alive <- pidAlive p
      if alive
        then putStrLn $ "deck alive (pid " <> show p <> ")"
        else do
          removeFile pidPath
          putStrLn $ "deck stale pid " <> show p <> " removed"

shellQuote :: String -> String
shellQuote s = "'" <> concatMap (\c -> if c == '\'' then "'\\''" else [c]) s <> "'"

formatAgentState :: AgentState -> Text
formatAgentState s =
  unNick (agentName s)
    <> "  "
    <> T.pack (show (agentStatus s))
    <> "  "
    <> fromMaybe "-" (agentCurrentDoing s)

printEither :: Either Text () -> IO ()
printEither = \case
  Left err -> TIO.hPutStrLn stderr err >> exitFailure
  Right () -> pure ()

printEither' :: Either Text Text -> IO ()
printEither' = \case
  Left err -> TIO.hPutStrLn stderr err >> exitFailure
  Right t -> TIO.putStrLn t
