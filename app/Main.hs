{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | muster — ad-hoc multiplicity events over named bus channels.
--
-- An IRC-flavoured front-end to the shared append-only mailbox. Channels are
-- isolated under @~/.config/muster/<channel>@; the default channel is
-- @bus@.  The engine is native Haskell ('Mailbox'); the bash
-- @mailbox.sh@ remains available on disk for rollback but is no longer
-- invoked from this binary.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (unless, void, when)
import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd, intercalate, isPrefixOf, sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (defaultTimeLocale, addUTCTime, formatTime, getCurrentTime)
import Muster.Bus qualified as Bus
import Options.Applicative
import System.Directory
import System.Environment (getEnv, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    createProcess,
    proc,
    waitForProcess,
  )
import Text.Read (readMaybe)
import Prelude

-- ---------------------------------------------------------------------------
-- Global options
-- ---------------------------------------------------------------------------

data Global = Global
  { globalChannel :: String,
    globalBusRoot :: FilePath
  }
  deriving (Show)

globalParser :: Parser Global
globalParser =
  Global
    <$> strOption
      ( long "channel"
          <> short 'c'
          <> value "bus"
          <> showDefault
          <> metavar "CHANNEL"
          <> help "Muster channel"
      )
    <*> strOption
      ( long "bus-root"
          <> value ""
          <> showDefault
          <> metavar "DIR"
          <> help "Root directory for all bus state (default: $HOME/.config/muster)"
      )

-- ---------------------------------------------------------------------------
-- Bus layout
-- ---------------------------------------------------------------------------

-- | Determine the root directory for all muster channels.
--
-- Preference order:
--   1. @--bus-root@ if non-empty
--   2. @MAILBOX_DIR@ env if non-empty (treated as a single-channel dir root parent)
--   3. @$HOME/.config/muster@
busRoot :: Global -> IO FilePath
busRoot (Global _ root)
  | not (null root) = pure root
busRoot _ = (</> ".config/muster") <$> getEnv "HOME"

-- | Directory for a specific channel.
channelDir :: Global -> IO FilePath
channelDir opts = do
  root <- busRoot opts
  pure (root </> globalChannel opts)

-- | Cursor file for a participant inside a channel directory.
cursorFile :: FilePath -> String -> FilePath
cursorFile dir name = dir </> ".cursor-" <> name

-- | Dedicated watch cursor for a participant.  Separating this from the join
-- cursor lets an agent read and watch concurrently, and lets N agents block on
-- the same channel without contending on a single cursor.
watchCursorFile :: FilePath -> String -> FilePath
watchCursorFile dir name = dir </> ".watch-" <> name

-- | Log file path inside a channel directory.
logFile :: FilePath -> FilePath
logFile dir = dir </> "log.md"

-- | Session metadata file inside a channel directory.
sessionFile :: FilePath -> FilePath
sessionFile dir = dir </> "session.md"

-- | Session state persisted to @session.md@.
data SessionState = SessionState
  { sessStatus :: String,
    sessOpened :: String,
    sessParticipants :: [String],
    sessLogStart :: Int
  }
  deriving (Show)

-- | Render session state as simple key-value text.
writeSession :: FilePath -> SessionState -> IO ()
writeSession path s =
  writeFile path $
    unlines
      [ "status: " <> sessStatus s,
        "opened: " <> sessOpened s,
        "participants: " <> unwords (sessParticipants s),
        "log-start: " <> show (sessLogStart s)
      ]

-- | Parse session state from key-value text.
readSession :: FilePath -> IO (Maybe SessionState)
readSession path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      raw <- lines . T.unpack <$> TIO.readFile path
      let kv = map (break (== ':')) raw
          lookupKey k =
            listToMaybe
              [ trim (drop 1 v)
                | (k', v) <- kv,
                  trim k' == k
              ]
          participants = maybe [] words (lookupKey "participants")
          logStart = maybe 0 (\v -> fromMaybe 0 (readMaybe (trim v))) (lookupKey "log-start")
      case lookupKey "status" of
        Nothing -> pure Nothing
        Just st ->
          pure $
            Just
              SessionState
                { sessStatus = st,
                  sessOpened = fromMaybe "" (lookupKey "opened"),
                  sessParticipants = participants,
                  sessLogStart = logStart
                }
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Current UTC timestamp in ISO-8601 format.
nowIso :: IO String
nowIso = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

-- | Parse a duration string like \"5m\", \"1h\", \"30s\" into seconds.
parseDuration :: String -> Maybe Int
parseDuration s = case span isDigit s of
  ([], _) -> Nothing
  (ds, unit)
    | null unit || unit == "s" -> Just (read ds)
    | unit == "m" -> Just (read ds * 60)
    | unit == "h" -> Just (read ds * 3600)
    | unit == "d" -> Just (read ds * 86400)
    | otherwise -> Nothing

-- | Produce an ISO-8601 timestamp N seconds before now.
formatBefore :: Int -> IO String
formatBefore secs = do
  t <- getCurrentTime
  let t' = addUTCTime (fromIntegral (-secs)) t
  pure $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" t'

-- ---------------------------------------------------------------------------
-- Commands
-- ---------------------------------------------------------------------------

data BusCmd = BusStart | BusStop | BusStatus | BusDaemon deriving (Show, Eq)

data AgentCmd
  = AgentNew String (Maybe String) (Maybe String) (Maybe String)
  | AgentStop String
  | AgentStatus (Maybe String)
  | AgentList
  deriving (Show)

data SessionCmd
  = SessionOpen
  | SessionClose
  | SessionStatus
  | SessionList
  deriving (Show, Eq)

data DeskCmd
  = DeskStart String Int Bool
  | DeskStop
  | DeskStatus
  deriving (Show, Eq)

data Cmd
  = CmdBus BusCmd
  | CmdJoin String
  | CmdLeave String
  | CmdPost [String]
  | CmdRead (Maybe String) (Maybe Int) Bool
  | CmdWatch (Maybe String) (Maybe Int) (Maybe String) (Maybe String) Bool
  | CmdNames
  | CmdLog
  | CmdChannels
  | CmdClean String Bool
  | CmdPrune String Int Bool
  | CmdAgent AgentCmd
  | CmdSession SessionCmd
  | CmdDesk DeskCmd
  deriving (Show)

nameArg :: Parser String
nameArg = argument str (metavar "NAME" <> help "Participant name")

channelArg :: Parser String
channelArg = argument str (metavar "CHANNEL" <> help "Channel name under bus root")

optionalNameArg :: Parser (Maybe String)
optionalNameArg =
  optional $
    argument
      str
      ( metavar "NAME"
          <> help "Participant name (default: MUSTER_NAME, else sole cursor)"
      )

-- | Positional args for post: optional NAME then MESSAGE words, or MESSAGE alone
-- when identity is resolvable (see 'resolvePostArgs').
postArgs :: Parser [String]
postArgs =
  some $
    argument
      str
      ( metavar "ARGS"
          <> help "Optional NAME then MESSAGE, or MESSAGE with MUSTER_NAME/sole cursor"
      )

timeoutOpt :: Parser (Maybe Int)
timeoutOpt =
  optional
    $ option
      auto
      ( long "timeout"
          <> short 't'
          <> metavar "SECONDS"
          <> help "Give up after this many seconds (default: block until mail)"
      )

filterOpt :: Parser (Maybe String)
filterOpt =
  optional
    $ strOption
      ( long "filter"
          <> short 'f'
          <> metavar "REGEX"
          <> help "Only wake on lines matching this substring/regex"
      )

sinceOpt :: Parser (Maybe String)
sinceOpt =
  optional
    $ strOption
      ( long "since"
          <> metavar "DURATION"
          <> help "Only wake on messages newer than this (e.g. 5m, 1h)"
      )

loopOpt :: Parser Bool
loopOpt =
  switch
    ( long "loop"
        <> short 'l'
        <> help "Continuous mode: re-arm after each match"
    )

yesOpt :: Parser Bool
yesOpt =
  switch
    ( long "yes"
        <> short 'y'
        <> help "Skip confirmation prompt"
    )

tailOpt :: Parser (Maybe Int)
tailOpt =
  optional
    $ option
      auto
      ( long "tail"
          <> short 'n'
          <> metavar "N"
          <> help "Show the last N log lines (default: 20)"
      )

allOpt :: Parser Bool
allOpt =
  switch
    ( long "all"
        <> help "Show all unread lines since the cursor (old default)"
    )

keepOpt :: Parser Int
keepOpt =
  option
    auto
    ( long "keep"
        <> metavar "N"
        <> value 100
        <> showDefault
        <> help "Number of newest log lines to retain"
    )

cmdParser :: Parser Cmd
cmdParser =
  hsubparser
    ( command "bus" (info (CmdBus <$> busCmdParser) (progDesc "Bus daemon lifecycle"))
        <> command "join" (info (CmdJoin <$> nameArg) (progDesc "Join the muster"))
        <> command "leave" (info (CmdLeave <$> nameArg) (progDesc "Leave the muster"))
        <> command
          "post"
          ( info
              (CmdPost <$> postArgs)
              (progDesc "Post a message (NAME optional if MUSTER_NAME or sole cursor)")
          )
        <> command
          "read"
          ( info
              (CmdRead <$> optionalNameArg <*> tailOpt <*> allOpt)
              (progDesc "Read messages (NAME optional if MUSTER_NAME or sole cursor; default --tail 20)")
          )
        <> command "watch" (info (CmdWatch <$> optionalNameArg <*> timeoutOpt <*> filterOpt <*> sinceOpt <*> loopOpt) (progDesc "Block until someone posts"))
        <> command "names" (info (pure CmdNames) (progDesc "List participants"))
        <> command "log" (info (pure CmdLog) (progDesc "Dump the channel log"))
        <> command "channels" (info (pure CmdChannels) (progDesc "List all channels with metadata"))
        <> command
          "clean"
          ( info
              (CmdClean <$> channelArg <*> yesOpt)
              (progDesc "Wipe a channel (bus must be stopped; confirms unless -y)")
          )
        <> command
          "prune"
          ( info
              (CmdPrune <$> channelArg <*> keepOpt <*> yesOpt)
              (progDesc "Archive old log lines, keep last N, reset cursors (bus must be stopped)")
          )
        <> command
          "agent"
          ( info
              (CmdAgent <$> agentCmdParser)
              (progDesc "Agent lifecycle (muster-agent)")
          )
        <> command
          "session"
          ( info
              (CmdSession <$> sessionCmdParser)
              (progDesc "Session lifecycle (open/close/status/list)")
          )
        <> command
          "desk"
          ( info
              (CmdDesk <$> deskCmdParser)
              (progDesc "Desk web UI lifecycle")
          )
    )

busCmdParser :: Parser BusCmd
busCmdParser =
  hsubparser
    ( command "start" (info (pure BusStart) (progDesc "Start the bus daemon"))
        <> command "stop" (info (pure BusStop) (progDesc "Stop the bus daemon"))
        <> command "status" (info (pure BusStatus) (progDesc "Report bus health"))
        <> command "daemon" (info (pure BusDaemon) (progDesc "Internal: run the bus relay loop"))
    )

agentCmdOpt :: Parser (Maybe String)
agentCmdOpt =
  optional $
    strOption
      ( long "agent"
          <> short 'a'
          <> metavar "CMD"
          <> help "Agent CLI command (default: hermes)"
      )

agentModelOpt :: Parser (Maybe String)
agentModelOpt =
  optional $
    strOption
      ( long "model"
          <> short 'm'
          <> metavar "MODEL"
          <> help "Hermes model (default: deepseek-v4-pro or HERMES_MODEL)"
      )

agentProviderOpt :: Parser (Maybe String)
agentProviderOpt =
  optional $
    strOption
      ( long "provider"
          <> metavar "PROVIDER"
          <> help "Hermes provider (default: deepseek or HERMES_PROVIDER)"
      )

agentCmdParser :: Parser AgentCmd
agentCmdParser =
  hsubparser
    ( command
        "new"
        ( info
            ( AgentNew
                <$> nameArg
                <*> agentCmdOpt
                <*> agentModelOpt
                <*> agentProviderOpt
            )
            (progDesc "Start muster-agent for NAME")
        )
        <> command
          "stop"
          ( info
              (AgentStop <$> nameArg)
              (progDesc "Stop agent NAME (kill bridge PID)")
          )
        <> command
          "status"
          ( info
              (AgentStatus <$> optionalNameArg)
              (progDesc "Report agent status (one NAME or all)")
          )
        <> command
          "list"
          ( info
              (pure AgentList)
              (progDesc "List agents under bus-root/agents/")
          )
    )

sessionCmdParser :: Parser SessionCmd
sessionCmdParser =
  hsubparser
    ( command "open" (info (pure SessionOpen) (progDesc "Open a session on the current channel"))
        <> command "close" (info (pure SessionClose) (progDesc "Close the session and archive its log"))
        <> command "status" (info (pure SessionStatus) (progDesc "Show current channel session"))
        <> command "list" (info (pure SessionList) (progDesc "List all open sessions"))
    )

deskNameOpt :: Parser String
deskNameOpt =
  strOption
    ( long "name"
        <> short 'n'
        <> value "desk"
        <> showDefault
        <> help "Desk identity on the bus"
    )

deskPortOpt :: Parser Int
deskPortOpt =
  option
    auto
    ( long "port"
        <> short 'p'
        <> value 9162
        <> showDefault
        <> help "HTTP port"
    )

deskDevOpt :: Parser Bool
deskDevOpt =
  switch
    ( long "dev"
        <> help "Serve deck.html from disk (app/deck.html) instead of embedded"
    )

deskCmdParser :: Parser DeskCmd
deskCmdParser =
  hsubparser
    ( command
        "start"
        ( info
            (DeskStart <$> deskNameOpt <*> deskPortOpt <*> deskDevOpt)
            (progDesc "Start the web desk")
        )
        <> command "stop" (info (pure DeskStop) (progDesc "Stop the web desk"))
        <> command "status" (info (pure DeskStatus) (progDesc "Report desk health"))
    )

data Options = Options Global Cmd

optionsParser :: ParserInfo Options
optionsParser =
  info
    (Options <$> globalParser <*> cmdParser <**> helper)
    ( fullDesc
        <> progDesc "muster: IRC-shaped shells for named coordination channels"
        <> header "muster — join, post, read, watch"
    )

-- ---------------------------------------------------------------------------
-- Execution
-- ---------------------------------------------------------------------------

runBus :: Global -> BusCmd -> IO ()
runBus opts = \case
  BusStart -> do
    dir <- channelDir opts
    root <- busRoot opts
    Bus.busStart dir (globalChannel opts) root
  BusStop -> channelDir opts >>= Bus.busStop
  BusStatus -> channelDir opts >>= Bus.busStatus
  BusDaemon -> channelDir opts >>= Bus.busDaemon

runJoin :: Global -> String -> IO ()
runJoin opts name = do
  dir <- channelDir opts
  createDirectoryIfMissing True dir
  let cursor = cursorFile dir name
  exists <- doesFileExist cursor
  if exists
    then putStrLn $ "[" <> name <> "] already joined in " <> globalChannel opts
    else do
      total <- Bus.countLogLines dir
      writeFile cursor (show total <> "\n")
      putStrLn $ "[" <> name <> "] joined " <> globalChannel opts <> "; cursor at line " <> show total
  -- If a session is open on this channel, add the participant.
  ms <- readSession (sessionFile dir)
  case ms of
    Just s | sessStatus s == "open" && name `notElem` sessParticipants s -> do
      let s' = s {sessParticipants = sort (name : sessParticipants s)}
      writeSession (sessionFile dir) s'
    _ -> pure ()

runLeave :: Global -> String -> IO ()
runLeave opts name = do
  dir <- channelDir opts
  let cursor = cursorFile dir name
  exists <- doesFileExist cursor
  if exists
    then removeFile cursor >> putStrLn ("[" <> name <> "] left " <> globalChannel opts)
    else putStrLn $ "[" <> name <> "] was not joined in " <> globalChannel opts

runPost :: Global -> [String] -> IO ()
runPost opts args = do
  (name, msg) <- resolvePostArgs opts args
  validateName name
  when (T.null msg) do
    hPutStrLn stderr "muster: empty message"
    exitWith (ExitFailure 1)
  dir <- channelDir opts
  Bus.post dir name msg

requireCursor :: Global -> String -> IO FilePath
requireCursor opts name = do
  dir <- channelDir opts
  let cursor = cursorFile dir name
  exists <- doesFileExist cursor
  unless exists do
    hPutStrLn stderr $ "muster: " <> name <> " has not joined " <> globalChannel opts
    exitWith (ExitFailure 1)
  pure cursor

runRead :: Global -> Maybe String -> Maybe Int -> Bool -> IO ()
runRead opts mname mtail allFlag = do
  name <- resolveName opts mname
  cursor <- requireCursor opts name
  dir <- channelDir opts
  if allFlag
    then Bus.readNew dir cursor
    else Bus.readTail dir cursor (fromMaybe 20 mtail)

-- | Cursor participant names in a channel directory (sorted).
cursorNames :: FilePath -> IO [String]
cursorNames dir = do
  exists <- doesDirectoryExist dir
  entries <- if exists then listDirectory dir else pure []
  pure $ sort [drop 8 e | e <- entries, ".cursor-" `isPrefixOf` e]

-- | Resolve participant identity.
--
-- Preference:
--   1. explicit @NAME@ argument
--   2. @MUSTER_NAME@ environment variable
--   3. sole @.cursor-*@ in the channel
--
-- Errors if zero or many cursors and no explicit/env identity.
resolveName :: Global -> Maybe String -> IO String
resolveName _ (Just name) = pure name
resolveName opts Nothing = do
  menv <- lookupEnv "MUSTER_NAME"
  case menv of
    Just n | not (null n) -> pure n
    _ -> do
      dir <- channelDir opts
      names <- cursorNames dir
      case names of
        [name] -> pure name
        [] -> do
          hPutStrLn stderr $ "muster: no participants in " <> globalChannel opts
          exitWith (ExitFailure 1)
        _ -> do
          hPutStrLn stderr $
            "muster: ambiguous name; set MUSTER_NAME or pass NAME; candidates: "
              <> intercalate ", " names
          exitWith (ExitFailure 1)

-- | Split post positionals into @(name, message)@.
--
-- Back-compat: @muster post NAME MSG...@ still works when @NAME@ is a known
-- cursor (or equals @MUSTER_NAME@ / the sole cursor).  With identity resolved
-- via env or sole cursor, all words are the message.
resolvePostArgs :: Global -> [String] -> IO (String, Text)
resolvePostArgs opts args = case args of
  [] -> do
    hPutStrLn stderr "muster: empty message"
    exitWith (ExitFailure 1)
  ws@(w : rest) -> do
    menv <- lookupEnv "MUSTER_NAME"
    dir <- channelDir opts
    cursors <- cursorNames dir
    let msg = T.pack . unwords
    case menv of
      Just n | not (null n) ->
        -- Env wins. Strip a leading explicit self-name for back-compat.
        if w == n && not (null rest)
          then pure (n, msg rest)
          else pure (n, msg ws)
      _ -> case cursors of
        [sole] ->
          if w == sole && not (null rest)
            then pure (sole, msg rest)
            else pure (sole, msg ws)
        _ ->
          if w `elem` cursors && not (null rest)
            then pure (w, msg rest)
            else
              if w `elem` cursors
                then do
                  hPutStrLn stderr "muster: empty message"
                  exitWith (ExitFailure 1)
                else do
                  hPutStrLn stderr $
                    "muster: ambiguous name; set MUSTER_NAME or pass NAME MSG; candidates: "
                      <> intercalate ", " cursors
                  exitWith (ExitFailure 1)

-- | Initialise a watch cursor from the participant's join cursor, or from the
-- current log length if no join cursor exists.
initWatchCursor :: FilePath -> FilePath -> FilePath -> IO ()
initWatchCursor dir joinCursor watchCursor = do
  n <-
    doesFileExist joinCursor >>= \case
      True -> do
        txt <- readFile joinCursor
        case readMaybe (filter (not . isSpace) txt) of
          Just i -> pure i
          Nothing -> Bus.countLogLines dir
      False -> Bus.countLogLines dir
  writeFile watchCursor (show n <> "\n")

runWatch :: Global -> Maybe String -> Maybe Int -> Maybe String -> Maybe String -> Bool -> IO ()
runWatch opts mname mto mfilter msince loopMode = do
  name <- resolveName opts mname
  dir <- channelDir opts
  joinCursor <- requireCursor opts name
  let wc = watchCursorFile dir name
  exists <- doesFileExist wc
  unless exists $ initWatchCursor dir joinCursor wc
  let timeout = fromMaybe 86400 mto
      -- Build the combined filter from --filter, --since, and self-exclude.
      selfExclude line =
        not $ (T.pack ("[" <> name <> "] ") `T.isPrefixOf` line)
           || (T.pack ("] " <> name <> ": ") `T.isInfixOf` line)
      regexFilter = case mfilter of
        Nothing -> const True
        Just pat -> \line ->
          let r = T.pack pat
           in r `T.isInfixOf` line
  cutoffText <- case msince of
    Nothing -> pure Nothing
    Just dur -> case parseDuration dur of
      Nothing -> pure Nothing
      Just secs -> Just . T.pack <$> formatBefore secs
  let sinceFilter = case cutoffText of
        Nothing -> const True
        Just cutoff -> \line ->
          case T.stripPrefix "[" line of
            Nothing -> True
            Just rest ->
              case T.breakOn "]" rest of
                (ts, _) -> T.null ts || ts >= cutoff
      combined line = selfExclude line && regexFilter line && sinceFilter line
      loop = do
        exit <- Bus.wait dir wc timeout combined
        case exit of
          ExitSuccess | loopMode -> loop
          ExitSuccess -> pure ()
          ExitFailure 2
            | loopMode -> loop -- timeout: restart the watch
            | otherwise -> pure ()
          e -> exitWith e
  loop

runNames :: Global -> IO ()
runNames opts = do
  dir <- channelDir opts
  exists <- doesDirectoryExist dir
  if not exists
    then putStrLn $ "no bus directory for " <> globalChannel opts
    else do
      entries <- listDirectory dir
      let cursors = sort [drop 8 e | e <- entries, ".cursor-" `isPrefixOf` e]
      if null cursors
        then putStrLn $ "no participants in " <> globalChannel opts
        else mapM_ putStrLn cursors

runLog :: Global -> IO ()
runLog opts = channelDir opts >>= Bus.dumpLog

-- | Wipe a channel: cursors, watches, log, fifo, pid. Bus must be down.
--
-- Confirms on stdin unless @--yes@. Does not remove the channel directory.
runClean :: Global -> String -> Bool -> IO ()
runClean opts channel yes = do
  when (null channel || any isSpace channel || channel == "." || channel == "..") do
    hPutStrLn stderr $ "muster: invalid channel '" <> channel <> "'"
    exitWith (ExitFailure 1)
  when ("_" `isPrefixOf` channel) do
    hPutStrLn stderr $ "muster: refusing to clean reserved path '" <> channel <> "'"
    exitWith (ExitFailure 1)
  root <- busRoot opts
  let dir = root </> channel
  exists <- doesDirectoryExist dir
  unless exists do
    hPutStrLn stderr $ "muster: no channel directory " <> dir
    exitWith (ExitFailure 1)
  live <- Bus.busLiveness dir
  unless (live == "down") do
    hPutStrLn stderr $
      "muster: bus is "
        <> live
        <> " on "
        <> channel
        <> " — stop it first (muster -c "
        <> channel
        <> " bus stop)"
    exitWith (ExitFailure 1)
  nLines <- Bus.countLogLines dir
  nParts <- length <$> cursorNames dir
  unless yes do
    putStr $
      "Wipe channel "
        <> channel
        <> " ("
        <> show nLines
        <> " lines, "
        <> show nParts
        <> " participants)? [y/N] "
    hFlush stdout
    ans <- getLine
    unless (ans == "y" || ans == "Y" || ans == "yes") do
      putStrLn "aborted"
      exitWith (ExitFailure 1)
  entries <- listDirectory dir
  let wipe =
        [ e
        | e <- entries
        , ".cursor-" `isPrefixOf` e
            || ".watch-" `isPrefixOf` e
            || ".watch.pid-" `isPrefixOf` e
            || e `elem` ["log.md", "err.md", "bus.pid", "bus.fifo"]
        ]
  mapM_ (\e -> removePathForcibly (dir </> e)) wipe
  putStrLn $
    "cleaned "
      <> channel
      <> " ("
      <> show (length wipe)
      <> " files removed; "
      <> show nLines
      <> " lines / "
      <> show nParts
      <> " participants gone)"

-- | Archive old log lines, retain the newest @keep@, reset join/watch cursors.
--
-- Bus must be down (rewriting @log.md@ under a live daemon races).  Dropped
-- lines are appended to @log-archive.md@ with a separator header.
runPrune :: Global -> String -> Int -> Bool -> IO ()
runPrune opts channel keep yes = do
  when (null channel || any isSpace channel || channel == "." || channel == "..") do
    hPutStrLn stderr $ "muster: invalid channel '" <> channel <> "'"
    exitWith (ExitFailure 1)
  when ("_" `isPrefixOf` channel) do
    hPutStrLn stderr $ "muster: refusing to prune reserved path '" <> channel <> "'"
    exitWith (ExitFailure 1)
  when (keep < 0) do
    hPutStrLn stderr "muster: --keep must be >= 0"
    exitWith (ExitFailure 1)
  root <- busRoot opts
  let dir = root </> channel
      logPath = logFile dir
      archivePath = dir </> "log-archive.md"
  exists <- doesDirectoryExist dir
  unless exists do
    hPutStrLn stderr $ "muster: no channel directory " <> dir
    exitWith (ExitFailure 1)
  live <- Bus.busLiveness dir
  unless (live == "down") do
    hPutStrLn stderr $
      "muster: bus is "
        <> live
        <> " on "
        <> channel
        <> " — stop it first (muster -c "
        <> channel
        <> " bus stop)"
    exitWith (ExitFailure 1)
  hasLog <- doesFileExist logPath
  if not hasLog
    then putStrLn $ "prune: no log.md in " <> channel <> " — nothing to do"
    else do
      raw <- TIO.readFile logPath
      let ls = T.lines raw
          total = length ls
          dropN = max 0 (total - keep)
          (dropped, kept) = splitAt dropN ls
      if dropN == 0
        then
          putStrLn $
            "prune: "
              <> channel
              <> " already within keep="
              <> show keep
              <> " ("
              <> show total
              <> " lines)"
        else do
          unless yes do
            putStr $
              "Prune "
                <> channel
                <> " (drop "
                <> show dropN
                <> ", keep "
                <> show (length kept)
                <> " of "
                <> show total
                <> ")? [y/N] "
            hFlush stdout
            ans <- getLine
            unless (ans == "y" || ans == "Y" || ans == "yes") do
              putStrLn "aborted"
              exitWith (ExitFailure 1)
          let archiveHeader =
                T.pack $
                  "\n--- pruned keep="
                    <> show keep
                    <> " dropped="
                    <> show dropN
                    <> " from="
                    <> show total
                    <> " ---\n"
              archived = archiveHeader <> T.unlines dropped
              newLog =
                if null kept
                  then ""
                  else T.unlines kept
          TIO.appendFile archivePath archived
          TIO.writeFile logPath newLog
          -- Reset join + watch cursors to the new tail so nothing re-fires as "new".
          entries <- listDirectory dir
          let cursors =
                [ e
                | e <- entries
                , ".cursor-" `isPrefixOf` e || ".watch-" `isPrefixOf` e
                ]
              newPos = length kept
          mapM_ (\e -> writeFile (dir </> e) (show newPos <> "\n")) cursors
          putStrLn $
            "pruned "
              <> channel
              <> ": dropped "
              <> show dropN
              <> ", kept "
              <> show newPos
              <> ", archived → "
              <> archivePath
              <> ", reset "
              <> show (length cursors)
              <> " cursors"

-- | List every channel under the bus root that has a @log.md@.
--
-- Output (aligned columns):
--
-- > general      152 lines  6 participants  alive (pid 12044)
-- > engine-test    3 lines  2 participants  down
runChannels :: Global -> IO ()
runChannels opts = do
  root <- busRoot opts
  exists <- doesDirectoryExist root
  unless exists do
    hPutStrLn stderr $ "muster: no bus root at " <> root
    exitWith (ExitFailure 1)
  entries <- listDirectory root
  let candidates = sort entries
  rows <- concat <$> mapM (channelRow root) candidates
  case rows of
    [] -> putStrLn $ "no channels under " <> root
    _ -> do
      let w = maximum (map (\(n, _, _, _) -> length n) rows)
      mapM_ (putStrLn . formatRow w) rows
  where
    channelRow :: FilePath -> FilePath -> IO [(String, Int, Int, String)]
    channelRow root name = do
      let dir = root </> name
      isDir <- doesDirectoryExist dir
      hasLog <- doesFileExist (logFile dir)
      if isDir && hasLog
        then do
          nLines <- Bus.countLogLines dir
          nParts <- countParticipants dir
          live <- Bus.busLiveness dir
          pure [(name, nLines, nParts, live)]
        else pure []

    countParticipants :: FilePath -> IO Int
    countParticipants dir = do
      es <- listDirectory dir
      pure $ length [e | e <- es, ".cursor-" `isPrefixOf` e]

    formatRow :: Int -> (String, Int, Int, String) -> String
    formatRow w (name, nLines, nParts, live) =
      padR w name
        <> "  "
        <> show nLines
        <> " lines  "
        <> show nParts
        <> " participants  "
        <> live

    padR :: Int -> String -> String
    padR n s = s <> replicate (max 0 (n - length s)) ' '

-- | Session lifecycle on a channel.
runSession :: Global -> SessionCmd -> IO ()
runSession opts = \case
  SessionOpen -> runSessionOpen opts
  SessionClose -> runSessionClose opts
  SessionStatus -> runSessionStatus opts
  SessionList -> runSessionList opts

runSessionOpen :: Global -> IO ()
runSessionOpen opts = do
  dir <- channelDir opts
  createDirectoryIfMissing True dir
  root <- busRoot opts
  Bus.busStart dir (globalChannel opts) root
  ms <- readSession (sessionFile dir)
  case ms of
    Just s | sessStatus s == "open" ->
      putStrLn $ "session already open on " <> globalChannel opts <> " (since " <> sessOpened s <> ")"
    _ -> do
      n <- Bus.countLogLines dir
      t <- nowIso
      let s = SessionState {sessStatus = "open", sessOpened = t, sessParticipants = [], sessLogStart = n}
      writeSession (sessionFile dir) s
      putStrLn $ "session opened on " <> globalChannel opts <> " at line " <> show n

runSessionClose :: Global -> IO ()
runSessionClose opts = do
  dir <- channelDir opts
  ms <- readSession (sessionFile dir)
  case ms of
    Nothing -> putStrLn $ "no session on " <> globalChannel opts
    Just s | sessStatus s /= "open" ->
      putStrLn $ "session on " <> globalChannel opts <> " is not open"
    Just s -> do
      let logPath = logFile dir
          archivePath = dir </> "log-archive.md"
      hasLog <- doesFileExist logPath
      when hasLog do
        raw <- TIO.readFile logPath
        let ls = T.lines raw
            start = sessLogStart s
            (kept, dropped) = splitAt start ls
        -- Archive lines from log-start to end.
        unless (null dropped) do
          let archiveHeader = T.pack $ "\n--- session close " <> globalChannel opts <> " opened=" <> sessOpened s <> " ---\n"
          TIO.appendFile archivePath (archiveHeader <> T.unlines dropped)
        -- Retain only lines before log-start.
        TIO.writeFile logPath (if null kept then "" else T.unlines kept)
        -- Reset cursors to new tail.
        entries <- listDirectory dir
        let cursors = [e | e <- entries, ".cursor-" `isPrefixOf` e || ".watch-" `isPrefixOf` e]
            newPos = length kept
        mapM_ (\e -> writeFile (dir </> e) (show newPos <> "\n")) cursors
      t <- nowIso
      let s' = s {sessStatus = "closed", sessOpened = sessOpened s <> "; closed: " <> t}
      writeSession (sessionFile dir) s'
      Bus.busStop dir
      putStrLn $ "session closed on " <> globalChannel opts

runSessionStatus :: Global -> IO ()
runSessionStatus opts = do
  dir <- channelDir opts
  ms <- readSession (sessionFile dir)
  case ms of
    Nothing -> putStrLn $ "no session on " <> globalChannel opts
    Just s -> do
      putStrLn $ "session on " <> globalChannel opts
      putStrLn $ "  status:       " <> sessStatus s
      putStrLn $ "  opened:       " <> sessOpened s
      putStrLn $ "  participants: " <> unwords (sessParticipants s)
      putStrLn $ "  log-start:    " <> show (sessLogStart s)

runSessionList :: Global -> IO ()
runSessionList opts = do
  root <- busRoot opts
  exists <- doesDirectoryExist root
  unless exists do
    hPutStrLn stderr $ "muster: no bus root at " <> root
    exitWith (ExitFailure 1)
  entries <- listDirectory root
  let dirs = sort entries
  rows <- concat <$> mapM (\n -> sessionRow (root </> n) n) dirs
  if null rows
    then putStrLn "no open sessions"
    else mapM_ putStrLn rows
  where
    sessionRow :: FilePath -> String -> IO [String]
    sessionRow dir name = do
      ms <- readSession (sessionFile dir)
      case ms of
        Just s | sessStatus s == "open" ->
          pure [name <> "  open  " <> sessOpened s <> "  [" <> unwords (sessParticipants s) <> "]"]
        _ -> pure []

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

validateName :: String -> IO ()
validateName name
  | null name = bad "empty name"
  | any isSpace name = bad "name cannot contain whitespace"
  | '[' `elem` name || ']' `elem` name = bad "name cannot contain brackets"
  | otherwise = pure ()
  where
    bad msg = do
      hPutStrLn stderr $ "muster: invalid name '" <> name <> "': " <> msg
      exitWith (ExitFailure 1)

-- ---------------------------------------------------------------------------
-- Agent lifecycle (muster-agent)
-- ---------------------------------------------------------------------------

agentsRoot :: Global -> IO FilePath
agentsRoot opts = (</> "agents") <$> busRoot opts

agentDir :: Global -> String -> IO FilePath
agentDir opts name = (</> name) <$> agentsRoot opts

pidAlive :: Int -> IO Bool
pidAlive pid = do
  r <- try @IOException $ do
    (_, _, _, ph) <-
      createProcess
        (proc "kill" ["-0", show pid])
          { std_in = NoStream,
            std_out = NoStream,
            std_err = NoStream
          }
    waitForProcess ph
  pure $ case r of
    Right ExitSuccess -> True
    _ -> False

readPidInt :: FilePath -> IO (Maybe Int)
readPidInt path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      raw <- readFile path
      pure $ readMaybe (filter (not . isSpace) raw)

killPid :: Int -> IO ()
killPid pid = void $ do
  (_, _, _, ph) <-
    createProcess
      (proc "kill" [show pid])
        { std_in = NoStream,
          std_out = NoStream,
          std_err = NoStream
        }
  waitForProcess ph

-- | Start the long-lived Haskell agent bridge.
runAgentNew :: Global -> String -> Maybe String -> Maybe String -> Maybe String -> IO ()
runAgentNew opts name mAgent mModel mProvider = do
  validateName name
  -- Agents are symmetric to humans: `muster agent new` creates the agent
  -- state but does NOT join any channel.  The operator joins the agent to
  -- one or more channels explicitly with `muster join <name> -c <channel>`.
  adir <- agentDir opts name
  createDirectoryIfMissing True adir
  let pidPath = adir </> "agent.pid"
      logPath = adir </> "bridge.log"
      cfgPath = adir </> "config"
  mpid <- readPidInt pidPath
  case mpid of
    Just p -> do
      alive <- pidAlive p
      when alive do
        hPutStrLn stderr $ "muster: agent " <> name <> " already running (pid " <> show p <> ")"
        exitWith (ExitFailure 1)
    Nothing -> pure ()
  homeModel <- lookupEnv "HERMES_MODEL"
  homeProv <- lookupEnv "HERMES_PROVIDER"
  let agentCmd = fromMaybe "hermes" mAgent
      model = fromMaybe (fromMaybe "deepseek-v4-pro" homeModel) mModel
      provider = fromMaybe (fromMaybe "deepseek" homeProv) mProvider
  writeFile cfgPath $
    unlines
      [ "name=" <> name,
        "channel=" <> name,
        "agent=" <> agentCmd,
        "model=" <> model,
        "provider=" <> provider
      ]
  let busRootArg = case globalBusRoot opts of
        "" -> []
        r -> ["--bus-root", r]
      args =
        busRootArg
          <> ["--name", name]
          <> ["--agent", agentCmd]
          <> ["--model", model]
          <> ["--provider", provider]
      sh =
        "nohup muster-agent "
          <> unwords (map show args)
          <> " >> "
          <> show logPath
          <> " 2>&1 & echo $! > "
          <> show pidPath
  (_, _, _, ph) <-
    createProcess
      (proc "/bin/sh" ["-c", sh]) {std_in = NoStream}
  void $ waitForProcess ph
  threadDelay 400000
  mpid' <- readPidInt pidPath
  case mpid' of
    Just p -> do
      alive <- pidAlive p
      if alive
        then putStrLn $ "[agent " <> name <> "] started pid " <> show p
        else do
          hPutStrLn stderr $ "muster: agent " <> name <> " died — see " <> logPath
          exitWith (ExitFailure 1)
    Nothing -> do
      hPutStrLn stderr $ "muster: agent " <> name <> " pid not written — see " <> logPath
      exitWith (ExitFailure 1)

runAgentStop :: Global -> String -> IO ()
runAgentStop opts name = do
  validateName name
  adir <- agentDir opts name
  let pidPath = adir </> "agent.pid"
  mpid <- readPidInt pidPath
  case mpid of
    Nothing -> putStrLn $ "[agent " <> name <> "] not running (no pidfile)"
    Just p -> do
      alive <- pidAlive p
      if alive
        then do
          killPid p
          threadDelay 200000
          -- if still alive, SIGKILL
          still <- pidAlive p
          when still $ void $ do
            (_, _, _, ph) <-
              createProcess
                (proc "kill" ["-9", show p])
                  { std_in = NoStream,
                    std_out = NoStream,
                    std_err = NoStream
                  }
            waitForProcess ph
          removeFile pidPath
          putStrLn $ "[agent " <> name <> "] stopped (was pid " <> show p <> ")"
        else do
          removeFile pidPath
          putStrLn $ "[agent " <> name <> "] stale pid " <> show p <> " removed"

runAgentStatusOne :: Global -> String -> IO ()
runAgentStatusOne opts name = do
  adir <- agentDir opts name
  let pidPath = adir </> "agent.pid"
      cfgPath = adir </> "config"
  exists <- doesDirectoryExist adir
  if not exists
    then putStrLn $ name <> "  no agent dir"
    else do
      mpid <- readPidInt pidPath
      state <- case mpid of
        Nothing -> pure "down"
        Just p -> do
          alive <- pidAlive p
          pure $ if alive then "alive (pid " <> show p <> ")" else "stale (pid " <> show p <> ")"
      cfg <- doesFileExist cfgPath >>= \case
        True -> intercalate " " . filter (not . null) . lines <$> readFile cfgPath
        False -> pure ""
      putStrLn $ name <> "  " <> state <> if null cfg then "" else "  " <> take 100 cfg

runAgentList :: Global -> IO ()
runAgentList opts = do
  root <- agentsRoot opts
  exists <- doesDirectoryExist root
  if not exists
    then putStrLn "no agents/"
    else do
      entries <- listDirectory root
      let names = sort [e | e <- entries, not ("_" `isPrefixOf` e)]
      if null names
        then putStrLn "no agents"
        else mapM_ (runAgentStatusOne opts) names

runAgent :: Global -> AgentCmd -> IO ()
runAgent opts = \case
  AgentNew name mAgent mModel mProvider -> runAgentNew opts name mAgent mModel mProvider
  AgentStop name -> runAgentStop opts name
  AgentStatus Nothing -> runAgentList opts
  AgentStatus (Just name) -> runAgentStatusOne opts name
  AgentList -> runAgentList opts

-- | Desk pid file inside a channel directory.
deskPidFile :: FilePath -> FilePath
deskPidFile dir = dir </> "desk.pid"

runDesk :: Global -> DeskCmd -> IO ()
runDesk opts = \case
  DeskStart name port dev -> runDeskStart opts name port dev
  DeskStop -> runDeskStop opts
  DeskStatus -> runDeskStatus opts

runDeskStart :: Global -> String -> Int -> Bool -> IO ()
runDeskStart opts name port dev = do
  dir <- channelDir opts
  createDirectoryIfMissing True dir
  let pidPath = deskPidFile dir
      logPath = dir </> "desk.log"
  mpid <- readPidInt pidPath
  case mpid of
    Just p -> do
      alive <- pidAlive p
      when alive do
        hPutStrLn stderr $ "muster: desk already running (pid " <> show p <> ")"
        exitWith (ExitFailure 1)
    Nothing -> pure ()
  let args =
        [ "--name", name,
          "--channel", globalChannel opts,
          "--port", show port
        ]
      devArg = if dev then ["--dev"] else []
      rootArg = case globalBusRoot opts of
        "" -> []
        r -> ["--bus-root", r]
      allArgs = args <> devArg <> rootArg
      sh =
        "nohup muster-ws "
          <> unwords allArgs
          <> " >> "
          <> show logPath
          <> " 2>&1 & echo $! > "
          <> show pidPath
  (_, _, _, ph) <-
    createProcess
      (proc "/bin/sh" ["-c", sh]) {std_in = NoStream}
  void $ waitForProcess ph
  threadDelay 500000
  mpid' <- readPidInt pidPath
  case mpid' of
    Nothing -> do
      hPutStrLn stderr $ "muster: desk pid not written — see " <> logPath
      exitWith (ExitFailure 1)
    Just p -> do
      alive <- pidAlive p
      if alive
        then putStrLn $ "desk started on http://127.0.0.1:" <> show port <> " (pid " <> show p <> ")"
        else do
          hPutStrLn stderr $ "muster: desk died immediately — see " <> logPath
          exitWith (ExitFailure 1)

runDeskStop :: Global -> IO ()
runDeskStop opts = do
  dir <- channelDir opts
  let pidPath = deskPidFile dir
  mpid <- readPidInt pidPath
  case mpid of
    Nothing -> putStrLn $ "no desk running on " <> globalChannel opts
    Just p -> do
      alive <- pidAlive p
      if alive
        then do
          killPid p
          removeFile pidPath
          putStrLn $ "desk stopped (was pid " <> show p <> ")"
        else do
          removeFile pidPath
          putStrLn $ "desk stale pid " <> show p <> " removed"

runDeskStatus :: Global -> IO ()
runDeskStatus opts = do
  dir <- channelDir opts
  let pidPath = deskPidFile dir
  mpid <- readPidInt pidPath
  case mpid of
    Nothing -> putStrLn $ "no desk on " <> globalChannel opts
    Just p -> do
      alive <- pidAlive p
      if alive
        then putStrLn $ "desk alive on " <> globalChannel opts <> " (pid " <> show p <> ")"
        else do
          removeFile pidPath
          putStrLn $ "desk stale pid " <> show p <> " removed"

main :: IO ()
main = do
  Options opts cmd <- execParser optionsParser
  case cmd of
    CmdBus bc -> runBus opts bc
    CmdJoin name -> runJoin opts name
    CmdLeave name -> runLeave opts name
    CmdPost args -> runPost opts args
    CmdRead mname mtail allFlag -> runRead opts mname mtail allFlag
    CmdWatch mname mto mfilter msince loopMode -> runWatch opts mname mto mfilter msince loopMode
    CmdNames -> runNames opts
    CmdLog -> runLog opts
    CmdChannels -> runChannels opts
    CmdClean channel yes -> runClean opts channel yes
    CmdPrune channel keep yes -> runPrune opts channel keep yes
    CmdAgent ac -> runAgent opts ac
    CmdSession sc -> runSession opts sc
    CmdDesk dc -> runDesk opts dc
