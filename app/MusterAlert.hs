{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Exit-on-match muster watcher.
--
-- Blocks on the bus until a message matching the filter arrives, prints the
-- matched line, and exits. This is the primitive for waking an agent: run it
-- as a background task; when it exits, the agent has a new message to read.
--
-- Idempotency: if @--lock PATH@ is given and the lock file already exists,
-- the watcher exits silently without starting a bus wait. The agent removes
-- the lock when it is ready to be woken again.
--
-- Usage: @muster-alert [-c channel] [-r root] [--lock path] [--timeout secs] <filter-regex> <name>@
-- Example: @muster-alert "@kimi" kimi@
module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muster.Bus qualified as Bus
import Muster.Cursor qualified as Cur
import Muster.Config qualified as Config
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import Prelude

data AlertConfig = AlertConfig
  { acChannel :: String,
    acRoot :: FilePath,
    acLock :: Maybe FilePath,
    acTimeout :: Int,
    acFilter :: String,
    acName :: String
  }
  deriving (Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case parseArgs args of
    Nothing -> usage
    Just cfg
      | null (acFilter cfg) || null (acName cfg) -> usage
      | otherwise -> runAlert cfg

usage :: IO ()
usage = do
  putStrLn "Usage: muster-alert [-c channel] [-r root] [--lock path] [--timeout secs] <filter-regex> <name>"
  putStrLn "  -c channel      channel (default: bus)"
  putStrLn "  -r root         bus root (default: $HOME/.config/muster)"
  putStrLn "  --lock path     lock file; exit silently if it already exists"
  putStrLn "  --timeout secs  max seconds to wait (default: 86400)"
  putStrLn "  filter-regex    substring/regex to match against each bus line"
  putStrLn "  name            watcher name (used for cursor file)"
  exitFailure

parseArgs :: [String] -> Maybe AlertConfig
parseArgs args = go args defaultCfg
  where
    defaultCfg =
      AlertConfig
        { acChannel = "bus",
          acRoot = "",
          acLock = Nothing,
          acTimeout = 86400,
          acFilter = "",
          acName = ""
        }
    go [] cfg = Just cfg
    go ("-c" : c : rest) cfg = go rest (cfg {acChannel = c})
    go ("-r" : r : rest) cfg = go rest (cfg {acRoot = r})
    go ("--lock" : p : rest) cfg = go rest (cfg {acLock = Just p})
    go ("--timeout" : t : rest) cfg =
      case reads t of
        [(n, "")] -> go rest (cfg {acTimeout = n})
        _ -> Nothing
    go ("-h" : _) _ = Nothing
    go ("--help" : _) _ = Nothing
    go [f, n] cfg = Just (cfg {acFilter = f, acName = n})
    go (_ : rest) cfg = go rest cfg

runAlert :: AlertConfig -> IO ()
runAlert cfg = do
  root <- if null (acRoot cfg) then Config.musterHome else pure (acRoot cfg)
  let dir = root </> acChannel cfg
      cursorFile = dir </> ".watch-" <> acName cfg
      joinCursor = dir </> ".cursor-" <> acName cfg

  -- Idempotency: don't wake a woken agent.
  case acLock cfg of
    Just path -> do
      locked <- doesFileExist path
      when locked exitSuccess
    Nothing -> pure ()

  -- Initialise the watch cursor from the join cursor so a fresh watcher does
  -- not fire on old messages. If there is no join cursor, start at the end of
  -- the current log.
  watchExists <- doesFileExist cursorFile
  unless watchExists $ do
    joinExists <- doesFileExist joinCursor
    if joinExists
      then do
        content <- try @IOException (TIO.readFile joinCursor)
        case content of
          Left _ -> do
            c <- Cur.newFile cursorFile
            _ <- Cur.pollFile c (dir </> "log.md")
            pure ()
          Right txt -> TIO.writeFile cursorFile txt
      else do
        c <- Cur.newFile cursorFile
        _ <- Cur.pollFile c (dir </> "log.md")
        pure ()

  let pat = T.pack (acFilter cfg)
      keep line =
        -- Self-exclude: ignore lines framed by the watcher itself.
        not (("] " `T.append` T.pack (acName cfg) `T.append` ": ") `T.isInfixOf` line)
          && pat `T.isInfixOf` line

  exit <- Bus.wait dir cursorFile (acTimeout cfg) keep
  case exit of
    ExitSuccess -> do
      -- Bus.wait already printed the matched line(s) to stdout.
      case acLock cfg of
        Just path -> do
          r <- try @IOException $ TIO.writeFile path ""
          case r of
            Left err -> hPutStrLn stderr $ "muster-alert: failed to write lock: " <> show err
            Right () -> pure ()
        Nothing -> pure ()
      exitSuccess
    ExitFailure 2 -> exitWith exit
    _ -> exitWith exit
