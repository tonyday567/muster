{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

-- | One-shot muster watcher.
--
-- Spawns @muster watch NAME@, waits for a bus line beginning with PREFIX,
-- prints that line, and exits.  This is the Haskell twin of the Python
-- @muster-alert.py@ used by turn-based Hermes sessions to get a
-- @notify_on_complete@ ping when a specific participant posts.
--
-- Usage: @muster-alert [-c <channel>] <prefix> <name>@
-- Example: @muster-alert "[desk]" hermes@
-- Example: @muster-alert -c dev "[desk]" hermes@
module Main (main) where

import Control.Exception (bracket, try)
import Control.Monad (void, when)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist, removeFile)
import System.Environment (getArgs, getEnv, lookupEnv)
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
import System.IO.Error (IOError, isDoesNotExistError)
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

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  getArgs >>= \case
    [prefix, name] -> runAlert "general" prefix name
    ["-c", channel, prefix, name] -> runAlert channel prefix name
    _ -> do
      putStrLn "Usage: muster-alert [-c <channel>] <prefix> <name>"
      putStrLn "  -c        channel (default: general)"
      putStrLn "  prefix:   line prefix to match, e.g. '[desk]'"
      putStrLn "  name:     watcher name passed to 'muster watch'"
      exitFailure

runAlert :: String -> String -> String -> IO ()
runAlert channel prefix name = do
  -- Clean stale watch pidfile before starting
  root <- resolveRoot
  let watchPidFile = root </> channel </> (".watch-" <> name)
  cleanStalePid watchPidFile
  ok <- bracket (start channel name) cleanup $ \(_, mOut, _, ph) ->
    case mOut of
      Nothing -> pure False
      Just h -> do
        hSetBuffering h LineBuffering
        hSetEncoding h utf8
        loop h ph
  if ok then exitSuccess else exitFailure
  where
    start chan n =
      createProcess
        (proc "muster" ["-c", chan, "watch", n])
          { std_out = CreatePipe,
            std_err = Inherit
          }

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
              if prefix `isPrefixOf` line
                then do
                  putStrLn line
                  hFlush stdout
                  pure True
                else loop h ph

resolveRoot :: IO FilePath
resolveRoot = do
  menv <- lookupEnv "MUSTER_ROOT"
  case menv of
    Just d | not (null d) -> pure d
    _ -> (</> "mg/logs/muster") <$> getEnv "HOME"

cleanStalePid :: FilePath -> IO ()
cleanStalePid path = do
  exists <- doesFileExist path
  when exists $ do
    content <- readFile path
    case reads content of
      [(pid, _)] -> do
        alive <- pidAlive pid
        when alive $
          void (try @IOError $ signalProcess sigTERM (CPid (fromIntegral pid)))
        void (try @IOError $ removeFile path)
      _ -> void (try @IOError $ removeFile path)

pidAlive :: Int -> IO Bool
pidAlive pid = do
  r <- try @IOError $ signalProcess nullSignal (CPid (fromIntegral pid))
  pure $ case r of
    Left _ -> False
    Right _ -> True

cleanup :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
cleanup (_, _, _, ph) = do
  terminateProcess ph
  _ <- waitForProcess ph
  return ()
