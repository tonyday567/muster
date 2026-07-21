{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | One-shot (or looping) muster watcher.
--
-- Spawns @muster watch NAME@, waits for a bus line from a specific sender,
-- prints that line, and exits.  With @--loop@ it re-arms after each match
-- and keeps watching until SIGTERM.
--
-- This is the Haskell twin of the Python @muster-alert.py@ used by
-- turn-based Hermes sessions to get a @notify_on_complete@ ping when a
-- specific participant posts.
--
-- Matches the sender in V2 frames @[timestamp] sender: body@.  For backward
-- compat, a prefix of the form @[name]@ is treated as @name@.
--
-- Usage: @muster-alert [-c <channel>] [-l|--loop] <sender> <name>@
-- Example: @muster-alert "desk" hermes@
-- Example: @muster-alert -c dev --loop "desk" hermes@
module Main (main) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (void, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesFileExist, removeFile)
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO
  ( BufferMode (..),
    Handle,
    hFlush,
    hGetLine,
    hIsEOF,
    hSetBuffering,
    hSetEncoding,
    stdout,
    utf8,
  )
import System.Posix.Signals (signalProcess, nullSignal, sigTERM)
import System.Posix.Types (CPid (..))
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    getProcessExitCode,
    proc,
    terminateProcess,
    waitForProcess,
  )

data Config = Config
  { cfgChannel :: String,
    cfgLoop :: Bool,
    cfgPrefix :: String,
    cfgName :: String
  }
  deriving (Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case parseArgs args of
    Nothing -> usage
    Just cfg | null (cfgPrefix cfg) || null (cfgName cfg) -> usage
    Just cfg -> runAlert cfg

usage :: IO ()
usage = do
  putStrLn "Usage: muster-alert [-c <channel>] [-l|--loop] <sender> <name>"
  putStrLn "  -c        channel (default: bus)"
  putStrLn "  -l, --loop  continuous mode: re-arm after each match"
  putStrLn "  sender:   sender name to match, e.g. 'desk' (or '[desk]' for back-compat)"
  putStrLn "  name:     watcher name passed to 'muster watch'"
  exitFailure

parseArgs :: [String] -> Maybe Config
parseArgs args = go args (Config "bus" False "" "")
  where
    go [] cfg = Just cfg
    go ("-c" : c : rest) cfg = go rest (cfg {cfgChannel = c})
    go ("--loop" : rest) cfg = go rest (cfg {cfgLoop = True})
    go ("-l" : rest) cfg = go rest (cfg {cfgLoop = True})
    go ("-h" : _) _ = Nothing
    go ("--help" : _) _ = Nothing
    go [p, n] cfg = Just (cfg {cfgPrefix = p, cfgName = n})
    go (_ : rest) cfg = go rest cfg

runAlert :: Config -> IO ()
runAlert cfg = do
  -- Clean stale watch pidfile before starting.  Important: this is the
  -- /watch pidfile/ (".watch.pid-.watch-NAME") written by Bus.wait, not the
  -- /watch cursor/ (".watch-NAME").  Deleting the cursor would reset the
  -- reader to the join cursor and replay old messages.
  root <- resolveRoot
  let watchPidFile = root </> cfgChannel cfg </> (".watch.pid-.watch-" <> cfgName cfg)
  cleanStalePid watchPidFile
  ok <- bracket (start cfg) cleanup $ \(_, mOut, _, ph) ->
    case mOut of
      Nothing -> pure False
      Just h -> do
        hSetBuffering h LineBuffering
        hSetEncoding h utf8
        loop h ph
  if ok then exitSuccess else exitFailure
  where
    start c =
      createProcess
        (proc "muster" args)
          { std_out = CreatePipe,
            std_err = Inherit
          }
      where
        args =
          ["-c", cfgChannel c, "watch", cfgName cfg]
            <> if cfgLoop cfg then ["--loop"] else []

    loop h ph = do
      done <- getProcessExitCode ph
      case done of
        Just _ -> pure False
        Nothing -> do
          eof <- hIsEOF h
          if eof
            then pure False
            else do
              line <- hGetLine h
              if senderMatches cfg line
                then do
                  putStrLn line
                  hFlush stdout
                  if cfgLoop cfg
                    then loop h ph
                    else pure True
                else loop h ph

-- | Match the configured sender against a V2-framed bus line.
--
-- V2 format: @[timestamp] sender: body@.  For convenience, a prefix
-- wrapped in brackets like @[desk]@ is normalised to @desk@.
senderMatches :: Config -> String -> Bool
senderMatches cfg line =
  case parseSender (T.pack line) of
    Nothing -> False
    Just sender -> normalise (cfgPrefix cfg) `T.isInfixOf` sender
  where
    normalise p =
      let t = T.pack p
          noOpen = fromMaybe t (T.stripPrefix "[" t)
          noClose = fromMaybe noOpen (T.stripSuffix "]" noOpen)
       in noClose

-- | Parse the sender from a V2 frame @[timestamp] sender: body@.
parseSender :: Text -> Maybe Text
parseSender t =
  case T.stripPrefix "[" t of
    Nothing -> Nothing
    Just rest ->
      case T.breakOn "] " rest of
        (_, "") -> Nothing
        (_, afterBracket) ->
          let after = T.drop 2 afterBracket
           in case T.breakOn ": " after of
                ("", _) -> Nothing
                (sender, _) -> Just sender

resolveRoot :: IO FilePath
resolveRoot = (</> ".config/muster") <$> getEnv "HOME"

cleanStalePid :: FilePath -> IO ()
cleanStalePid path = do
  exists <- doesFileExist path
  when exists $ do
    content <- readFile path
    case reads content of
      [(pid, _)] -> do
        alive <- pidAlive pid
        when alive $
          void (try @IOException $ signalProcess sigTERM (CPid (fromIntegral pid)))
        void (try @IOException $ removeFile path)
      _ -> void (try @IOException $ removeFile path)

pidAlive :: Int -> IO Bool
pidAlive pid = do
  r <- try @IOException $ signalProcess nullSignal (CPid (fromIntegral pid))
  pure $ case r of
    Left _ -> False
    Right _ -> True

cleanup :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
cleanup (_, _, _, ph) = do
  terminateProcess ph
  _ <- waitForProcess ph
  return ()
