{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | muster — ad-hoc multiplicity events over named bus channels.
--
-- An IRC-flavoured front-end to the shared append-only mailbox. Channels are
-- isolated under @~/mg/logs/muster/<channel>@; the default channel is
-- @general@.
module Main (main) where

import Control.Monad (unless, when)
import Data.Char (isSpace)
import Data.List (isPrefixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
import System.Directory
import System.Environment (getEnv, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO
import System.Process
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
--   2. @MAILBOX_DIR@ env if non-empty
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

-- | Path to the hardened bash mailbox script.
mailboxScript :: Global -> IO FilePath
mailboxScript _ = (</> "mg/logs/board/mailbox.sh") <$> getEnv "HOME"

-- | Cursor file for a participant inside a channel directory.
cursorFile :: FilePath -> String -> FilePath
cursorFile dir name = dir </> ".cursor-" <> name

-- | Log file path inside a channel directory.
logFile :: FilePath -> FilePath
logFile dir = dir </> "log.md"

-- ---------------------------------------------------------------------------
-- Commands
-- ---------------------------------------------------------------------------

data BusCmd = BusStart | BusStop | BusStatus deriving (Show, Eq)

data Cmd
  = CmdBus BusCmd
  | CmdJoin String
  | CmdLeave String
  | CmdPost String Text
  | CmdRead String
  | CmdWatch String (Maybe Int)
  | CmdNames
  | CmdLog
  deriving (Show)

nameArg :: Parser String
nameArg = argument str (metavar "NAME" <> help "Participant name")

msgArgs :: Parser Text
msgArgs = fmap T.unwords $ some $ argument str (metavar "MESSAGE" <> help "Message body")

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

cmdParser :: Parser Cmd
cmdParser =
  hsubparser
    ( command "bus" (info (CmdBus <$> busCmdParser) (progDesc "Bus daemon lifecycle"))
        <> command "join" (info (CmdJoin <$> nameArg) (progDesc "Join the muster"))
        <> command "leave" (info (CmdLeave <$> nameArg) (progDesc "Leave the muster"))
        <> command "post" (info (CmdPost <$> nameArg <*> msgArgs) (progDesc "Post a message"))
        <> command "read" (info (CmdRead <$> nameArg) (progDesc "Read new messages"))
        <> command "watch" (info (CmdWatch <$> nameArg <*> timeoutOpt) (progDesc "Block until someone posts"))
        <> command "names" (info (pure CmdNames) (progDesc "List participants"))
        <> command "log" (info (pure CmdLog) (progDesc "Dump the channel log"))
    )

busCmdParser :: Parser BusCmd
busCmdParser =
  hsubparser
    ( command "start" (info (pure BusStart) (progDesc "Start the bus daemon"))
        <> command "stop" (info (pure BusStop) (progDesc "Stop the bus daemon"))
        <> command "status" (info (pure BusStatus) (progDesc "Report bus health"))
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

withMailboxDir :: FilePath -> IO a -> IO a
withMailboxDir dir io = do
  old <- lookupEnv "MAILBOX_DIR"
  setEnv "MAILBOX_DIR" dir
  res <- io
  case old of
    Just v | not (null v) -> setEnv "MAILBOX_DIR" v
    _ -> unsetEnv "MAILBOX_DIR"
  pure res

runMailbox :: Global -> [String] -> IO ()
runMailbox opts args = do
  dir <- channelDir opts
  script <- mailboxScript opts
  createDirectoryIfMissing True dir
  withMailboxDir dir do
    (exit, out, err) <- readProcessWithExitCode script args ""
    unless (null out) $ putStr out
    unless (null err) $ hPutStr stderr err
    case exit of
      ExitSuccess -> pure ()
      e -> exitWith e

runBus :: Global -> BusCmd -> IO ()
runBus opts = \case
  BusStart -> runMailbox opts ["start"]
  BusStop -> runMailbox opts ["stop"]
  BusStatus -> runMailbox opts ["status"]

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

runPost :: Global -> String -> Text -> IO ()
runPost opts name msg = do
  validateName name
  when (T.null msg) do
    hPutStrLn stderr "muster: empty message"
    exitWith (ExitFailure 1)
  runMailbox opts ["post", name, T.unpack msg]

requireCursor :: Global -> String -> IO FilePath
requireCursor opts name = do
  dir <- channelDir opts
  let cursor = cursorFile dir name
  exists <- doesFileExist cursor
  unless exists do
    hPutStrLn stderr $ "muster: " <> name <> " has not joined " <> globalChannel opts
    exitWith (ExitFailure 1)
  pure cursor

runRead :: Global -> String -> IO ()
runRead opts name = do
  cursor <- requireCursor opts name
  runMailbox opts ["read", cursor]

runWatch :: Global -> String -> Maybe Int -> IO ()
runWatch opts name mto = do
  cursor <- requireCursor opts name
  let timeout = maybe "3600" show mto
  runMailbox opts ["wait", cursor, timeout, "[" <> name <> "] "]

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
runLog opts = do
  dir <- channelDir opts
  let path = logFile dir
  exists <- doesFileExist path
  if exists
    then TIO.readFile path >>= TIO.putStr
    else pure ()

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

countLogLines :: FilePath -> IO Int
countLogLines dir = do
  let logPath = logFile dir
  exists <- doesFileExist logPath
  if not exists
    then pure 0
    else do
      content <- TIO.readFile logPath
      pure $ length $ filter (not . T.null) $ T.splitOn "\n" content

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
    CmdPost name msg -> runPost opts name msg
    CmdRead name -> runRead opts name
    CmdWatch name mto -> runWatch opts name mto
    CmdNames -> runNames opts
    CmdLog -> runLog opts
