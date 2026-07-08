{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | muster — ad-hoc multiplicity events over the bus.
--
-- An IRC-flavoured front-end to the shared append-only mailbox.
-- Every command is visible in `--help`; no onboarding doc required.
module Main (main) where

import Control.Monad (unless, when)
import Data.Char (isSpace)
import Data.List (isPrefixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
import System.Directory
import System.Environment (getEnv, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO
import System.Process
import Prelude

-- ---------------------------------------------------------------------------
-- Bus layout
-- ---------------------------------------------------------------------------

-- | Determine the bus directory.
--
-- Environment @MAILBOX_DIR@ overrides the default @$HOME/mg/logs/board@.
busDir :: IO FilePath
busDir = do
  menv <- lookupEnv "MAILBOX_DIR"
  case menv of
    Just d -> pure d
    Nothing -> (</> "mg/logs/board") <$> getEnv "HOME"

-- | Path to the hardened bash mailbox script.
mailboxScript :: IO FilePath
mailboxScript = (</> "mailbox.sh") <$> busDir

-- | Cursor file for a participant.
cursorFile :: FilePath -> String -> FilePath
cursorFile dir name = dir </> ".cursor-" <> name

-- | Log file path.
logFile :: FilePath -> FilePath
logFile dir = dir </> "log.md"

-- ---------------------------------------------------------------------------
-- CLI shape
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
  deriving (Show, Eq)

cmdP :: Parser Cmd
cmdP =
  subparser
    ( command
        "bus"
        ( info
            (CmdBus <$> busCmdP <**> helper)
            (progDesc "Lifecycle of the shared bus daemon")
        )
        <> command
          "join"
          ( info
              (CmdJoin <$> argument str (metavar "NAME" <> help "Participant name"))
              (progDesc "Join the muster (create a read cursor)")
          )
        <> command
          "leave"
          ( info
              (CmdLeave <$> argument str (metavar "NAME" <> help "Participant name"))
              (progDesc "Leave the muster (remove read cursor)")
          )
        <> command
          "post"
          ( info
              ( CmdPost
                  <$> argument str (metavar "NAME" <> help "Who is posting")
                  <*> fmap T.pack (argument str (metavar "MESSAGE" <> help "Message body"))
              )
              (progDesc "Post a [named] message to the bus")
          )
        <> command
          "read"
          ( info
              (CmdRead <$> argument str (metavar "NAME" <> help "Participant name"))
              (progDesc "Read new messages since your last read")
          )
        <> command
          "watch"
          ( info
              ( CmdWatch
                  <$> argument str (metavar "NAME" <> help "Participant name")
                  <*> optional
                    ( option
                        auto
                        ( long "timeout"
                            <> short 't'
                            <> metavar "SECONDS"
                            <> help "Give up after this many seconds (default: block until mail)"
                        )
                    )
              )
              (progDesc "Block until someone else posts, then print it")
          )
        <> command
          "names"
          (info (pure CmdNames) (progDesc "List participants with cursors"))
    )

busCmdP :: Parser BusCmd
busCmdP =
  subparser
    ( command "start" (info (pure BusStart) (progDesc "Start the bus daemon"))
        <> command "stop" (info (pure BusStop) (progDesc "Stop the bus daemon"))
        <> command "status" (info (pure BusStatus) (progDesc "Report bus health"))
    )

opts :: ParserInfo Cmd
opts =
  info
    (cmdP <**> helper)
    ( fullDesc
        <> progDesc "muster: an IRC-shaped shell for the coordination bus"
        <> header "muster — join, post, read, watch"
    )

-- ---------------------------------------------------------------------------
-- Execution
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    CmdBus bc -> runBus bc
    CmdJoin name -> runJoin name
    CmdLeave name -> runLeave name
    CmdPost name msg -> runPost name msg
    CmdRead name -> runRead name
    CmdWatch name mto -> runWatch name mto
    CmdNames -> runNames

-- | Run the mailbox script with the given arguments.
runMailbox :: [String] -> IO ()
runMailbox args = do
  script <- mailboxScript
  (exit, out, err) <- readProcessWithExitCode script args ""
  unless (null out) $ putStr out
  unless (null err) $ hPutStr stderr err
  case exit of
    ExitSuccess -> pure ()
    e -> exitWith e

runBus :: BusCmd -> IO ()
runBus = \case
  BusStart -> runMailbox ["start"]
  BusStop -> runMailbox ["stop"]
  BusStatus -> runMailbox ["status"]

runJoin :: String -> IO ()
runJoin name = do
  dir <- busDir
  createDirectoryIfMissing True dir
  let cursor = cursorFile dir name
  exists <- doesFileExist cursor
  if exists
    then putStrLn $ "[" <> name <> "] already joined"
    else do
      -- Start cursor at the current end of the log so the participant
      -- only sees mail that arrives after they join.
      total <- countLogLines dir
      writeFile cursor (show total <> "\n")
      putStrLn $ "[" <> name <> "] joined; cursor at line " <> show total

runLeave :: String -> IO ()
runLeave name = do
  dir <- busDir
  let cursor = cursorFile dir name
  exists <- doesFileExist cursor
  if exists
    then removeFile cursor >> putStrLn ("[" <> name <> "] left")
    else putStrLn $ "[" <> name <> "] was not joined"

runPost :: String -> Text -> IO ()
runPost name msg = do
  validateName name
  when (T.null msg) do
    hPutStrLn stderr "muster: empty message"
    exitWith (ExitFailure 1)
  runMailbox ["post", name, T.unpack msg]

runRead :: String -> IO ()
runRead name = do
  dir <- busDir
  let cursor = cursorFile dir name
  exists <- doesFileExist cursor
  unless exists do
    hPutStrLn stderr $ "muster: " <> name <> " has not joined (muster join " <> name <> ")"
    exitWith (ExitFailure 1)
  runMailbox ["read", cursor]

runWatch :: String -> Maybe Int -> IO ()
runWatch name mto = do
  dir <- busDir
  let cursor = cursorFile dir name
  exists <- doesFileExist cursor
  unless exists do
    hPutStrLn stderr $ "muster: " <> name <> " has not joined (muster join " <> name <> ")"
    exitWith (ExitFailure 1)
  let timeout = maybe "3600" show mto
  -- exclude own posts to avoid self-wake loops
  runMailbox ["wait", cursor, timeout, "[" <> name <> "] "]

runNames :: IO ()
runNames = do
  dir <- busDir
  exists <- doesDirectoryExist dir
  if not exists
    then putStrLn "no bus directory"
    else do
      entries <- listDirectory dir
      let cursors = sort [drop 8 e | e <- entries, ".cursor-" `isPrefixOf` e]
      if null cursors
        then putStrLn "no participants"
        else mapM_ putStrLn cursors

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
      -- same rule as the bash bus: trailing newline means drop final empty part
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
