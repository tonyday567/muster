{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | muster — ad-hoc multiplicity events over named bus channels.
--
-- An IRC-flavoured front-end to the shared append-only mailbox. Channels are
-- isolated under @~/mg/logs/muster/<channel>@; the default channel is
-- @general@.  The engine is native Haskell ('Mailbox'); the bash
-- @mailbox.sh@ remains available on disk for rollback but is no longer
-- invoked from this binary.
module Main (main) where

import Control.Monad (unless, when)
import Data.Char (isSpace)
import Data.List (intercalate, isPrefixOf, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Mailbox qualified as MB
import Options.Applicative
import System.Directory
import System.Environment (getEnv, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO
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
          <> value "general"
          <> showDefault
          <> metavar "CHANNEL"
          <> help "Muster channel"
      )
    <*> strOption
      ( long "bus-root"
          <> value ""
          <> showDefault
          <> metavar "DIR"
          <> help "Root directory for all bus state (default: $HOME/mg/logs/muster)"
      )

-- ---------------------------------------------------------------------------
-- Bus layout
-- ---------------------------------------------------------------------------

-- | Determine the root directory for all muster channels.
--
-- Preference order:
--   1. @--bus-root@ if non-empty
--   2. @MAILBOX_DIR@ env if non-empty (treated as a single-channel dir root parent)
--   3. @$HOME/mg/logs/muster@
busRoot :: Global -> IO FilePath
busRoot (Global _ root)
  | not (null root) = pure root
busRoot _ = do
  menv <- lookupEnv "MAILBOX_DIR"
  case menv of
    Just d | not (null d) -> pure d
    _ -> (</> "mg/logs/muster") <$> getEnv "HOME"

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

-- ---------------------------------------------------------------------------
-- Commands
-- ---------------------------------------------------------------------------

data BusCmd = BusStart | BusStop | BusStatus | BusDaemon deriving (Show, Eq)

data Cmd
  = CmdBus BusCmd
  | CmdJoin String
  | CmdLeave String
  | CmdPost [String]
  | CmdRead (Maybe String)
  | CmdWatch (Maybe String) (Maybe Int)
  | CmdNames
  | CmdLog
  | CmdChannels
  | CmdClean String Bool
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

yesOpt :: Parser Bool
yesOpt =
  switch
    ( long "yes"
        <> short 'y'
        <> help "Skip confirmation prompt"
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
              (CmdRead <$> optionalNameArg)
              (progDesc "Read new messages (NAME optional if MUSTER_NAME or sole cursor)")
          )
        <> command "watch" (info (CmdWatch <$> optionalNameArg <*> timeoutOpt) (progDesc "Block until someone posts"))
        <> command "names" (info (pure CmdNames) (progDesc "List participants"))
        <> command "log" (info (pure CmdLog) (progDesc "Dump the channel log"))
        <> command "channels" (info (pure CmdChannels) (progDesc "List all channels with metadata"))
        <> command
          "clean"
          ( info
              (CmdClean <$> channelArg <*> yesOpt)
              (progDesc "Wipe a channel (bus must be stopped; confirms unless -y)")
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
    MB.busStart dir (globalChannel opts) root
  BusStop -> channelDir opts >>= MB.busStop
  BusStatus -> channelDir opts >>= MB.busStatus
  BusDaemon -> channelDir opts >>= MB.busDaemon

runJoin :: Global -> String -> IO ()
runJoin opts name = do
  dir <- channelDir opts
  createDirectoryIfMissing True dir
  let cursor = cursorFile dir name
  exists <- doesFileExist cursor
  if exists
    then putStrLn $ "[" <> name <> "] already joined in " <> globalChannel opts
    else do
      total <- countLogLines dir
      writeFile cursor (show total <> "\n")
      putStrLn $ "[" <> name <> "] joined " <> globalChannel opts <> "; cursor at line " <> show total

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
  MB.post dir name msg

requireCursor :: Global -> String -> IO FilePath
requireCursor opts name = do
  dir <- channelDir opts
  let cursor = cursorFile dir name
  exists <- doesFileExist cursor
  unless exists do
    hPutStrLn stderr $ "muster: " <> name <> " has not joined " <> globalChannel opts
    exitWith (ExitFailure 1)
  pure cursor

runRead :: Global -> Maybe String -> IO ()
runRead opts mname = do
  name <- resolveName opts mname
  cursor <- requireCursor opts name
  dir <- channelDir opts
  MB.readNew dir cursor

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
          Nothing -> countLogLines dir
      False -> countLogLines dir
  writeFile watchCursor (show n <> "\n")

runWatch :: Global -> Maybe String -> Maybe Int -> IO ()
runWatch opts mname mto = do
  name <- resolveName opts mname
  dir <- channelDir opts
  joinCursor <- requireCursor opts name
  let wc = watchCursorFile dir name
  exists <- doesFileExist wc
  unless exists $ initWatchCursor dir joinCursor wc
  let timeout = fromMaybe 86400 mto
  let loop = do
        exit <- MB.wait dir wc timeout ("[" <> name <> "] ")
        case exit of
          ExitSuccess -> loop
          ExitFailure 2 -> pure () -- timeout expired
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
runLog opts = channelDir opts >>= MB.dumpLog

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
  live <- MB.busLiveness dir
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
  nLines <- countLogLines dir
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
          nLines <- countLogLines dir
          nParts <- countParticipants dir
          live <- MB.busLiveness dir
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

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

countLogLines :: FilePath -> IO Int
countLogLines dir = do
  let logPath = logFile dir
  exists <- doesFileExist logPath
  if not exists
    then pure 0
    else T.count "\n" <$> TIO.readFile logPath

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

main :: IO ()
main = do
  Options opts cmd <- execParser optionsParser
  case cmd of
    CmdBus bc -> runBus opts bc
    CmdJoin name -> runJoin opts name
    CmdLeave name -> runLeave opts name
    CmdPost args -> runPost opts args
    CmdRead mname -> runRead opts mname
    CmdWatch mname mto -> runWatch opts mname mto
    CmdNames -> runNames opts
    CmdLog -> runLog opts
    CmdChannels -> runChannels opts
    CmdClean channel yes -> runClean opts channel yes
