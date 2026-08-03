{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Native Haskell mailbox engine.
--
-- Behaviour-preserving port of @~/mg/logs/board/mailbox.sh@: FIFO + append-only
-- log, cursor-based read, and pidfile-guarded watch.  The bus daemon opens the
-- FIFO read-write so it never sees EOF between messages (the @cat \<>fifo@
-- trick, without shelling out to @cat@).
module Muster.Bus
  ( BusPaths (..),
    pathsFor,
    busStart,
    busStop,
    busStatus,
    busLiveness,
    busDaemon,
    busRunning,
    post,
    readNew,
    readTail,
    wait,
    dumpLog,
    countLogLines,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, catch, finally, try)
import Control.Monad (forever, unless, void, when)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muster.Cursor qualified as Cur
import Muster.Framing (formatNow, frameMessage)
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    removeFile,
  )
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeFileName, (</>))
import System.IO
  ( BufferMode (..),
    Handle,
    IOMode (..),
    hClose,
    hFlush,
    hPutStrLn,
    hSetBuffering,
    hSetEncoding,
    openFile,
    stderr,
    stdout,
    utf8,
  )
import System.IO.Error (isEOFError)
import System.Posix.Files (createNamedPipe, stdFileMode)
import System.Posix.Process (getProcessID)
import System.Posix.Types (CPid (..), ProcessID)
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    getProcessExitCode,
    proc,
  )
import Text.Read (readMaybe)
import Prelude

-- ---------------------------------------------------------------------------
-- Paths
-- ---------------------------------------------------------------------------

-- | Standard files inside a channel directory.
data BusPaths = BusPaths
  { busDir :: FilePath,
    busFifo :: FilePath,
    busLog :: FilePath,
    busErr :: FilePath,
    busPid :: FilePath
  }
  deriving (Show, Eq)

-- | Compute standard paths from a channel directory.
pathsFor :: FilePath -> BusPaths
pathsFor dir =
  BusPaths
    { busDir = dir,
      busFifo = dir </> "bus.fifo",
      busLog = dir </> "log.md",
      busErr = dir </> "err.md",
      busPid = dir </> "bus.pid"
    }

watchPidFile :: FilePath -> FilePath -> FilePath
watchPidFile dir cursorfile =
  -- Use takeFileName (not takeBaseName): cursors are ".watch-NAME" / ".cursor-NAME"
  -- and takeBaseName would strip ".NAME" as a fake extension, collapsing every
  -- watcher onto one pidfile (".watch.pid-.watch") — multi-watcher death.
  let base = takeFileName cursorfile
      watchname = fromMaybe base (stripPrefix' ".cursor-" base)
   in dir </> ".watch.pid-" <> watchname
  where
    stripPrefix' p s
      | p `isPrefixOf` s = Just (drop (length p) s)
      | otherwise = Nothing

pidToInt :: ProcessID -> Int
pidToInt (CPid x) = fromIntegral x

-- ---------------------------------------------------------------------------
-- Log metrics
-- ---------------------------------------------------------------------------

-- | Count newline bytes in a log (match @wc -l@).
countLogLines :: FilePath -> IO Int
countLogLines dir = do
  let p = pathsFor dir
  exists <- doesFileExist (busLog p)
  if not exists
    then pure 0
    else T.count "\n" <$> TIO.readFile (busLog p)

-- ---------------------------------------------------------------------------
-- Process liveness (kill -0)
-- ---------------------------------------------------------------------------

readPidFile :: FilePath -> IO (Maybe ProcessID)
readPidFile path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      raw <- readFile path
      case readMaybe (filter (not . isSpace) raw) :: Maybe Integer of
        Nothing -> pure Nothing
        Just i -> pure $ Just (CPid (fromIntegral i))

processAlive :: ProcessID -> IO Bool
processAlive pid = do
  r <- try @IOException $ do
    (_, _, _, ph) <-
      createProcess
        (proc "kill" ["-0", show (pidToInt pid)])
          { std_in = NoStream,
            std_out = NoStream,
            std_err = NoStream
          }
    waitExit ph 50
  pure $ case r of
    Right ExitSuccess -> True
    _ -> False

waitExit :: ProcessHandle -> Int -> IO ExitCode
waitExit ph n = do
  mec <- getProcessExitCode ph
  case mec of
    Just e -> pure e
    Nothing
      | n > 0 -> threadDelay 10000 >> waitExit ph (n - 1)
      | otherwise -> pure (ExitFailure 1)

busRunning :: BusPaths -> IO (Maybe ProcessID)
busRunning p = do
  mpid <- readPidFile (busPid p)
  case mpid of
    Nothing -> pure Nothing
    Just pid -> do
      alive <- processAlive pid
      pure $ if alive then Just pid else Nothing

signalKill :: ProcessID -> IO ()
signalKill pid = void $ do
  (_, _, _, ph) <-
    createProcess
      (proc "kill" [show (pidToInt pid)])
        { std_in = NoStream,
          std_out = NoStream,
          std_err = NoStream
        }
  waitExit ph 50

-- ---------------------------------------------------------------------------
-- Daemon
-- ---------------------------------------------------------------------------

-- | Long-running bus: open FIFO RDWR (never EOF between writers) and append
-- each line to the log.
busDaemon :: FilePath -> IO ()
busDaemon dir = do
  let p = pathsFor dir
  createDirectoryIfMissing True dir
  self <- getProcessID
  writeFile (busPid p) (show (pidToInt self) <> "\n")
  -- Block on reads.  When every external writer closes, 'hGetLine'
  -- raises EOF — and GHC latches EOF on the handle, so the handle can
  -- never see later writes.  Close and re-open fresh; lines written
  -- meanwhile wait in the kernel pipe.  No hIsEOF polling: the poll
  -- was what latched.
  let relayOnce = do
        h <- openFile (busFifo p) ReadWriteMode
        hSetEncoding h utf8
        hSetBuffering h LineBuffering
        logH <- openFile (busLog p) AppendMode
        hSetEncoding logH utf8
        hSetBuffering logH NoBuffering
        (relay h logH `finally` (hClose h >> hClose logH))
          `catch` \e -> if isEOFError e then pure () else ioError e
      relay :: Handle -> Handle -> IO ()
      relay h logH = forever do
        line <- TIO.hGetLine h
        TIO.hPutStrLn logH line
        hFlush logH
  forever relayOnce

-- | Start the bus daemon by re-execing the current binary as @bus daemon@.
busStart :: FilePath -> String -> FilePath -> IO ()
busStart dir channel busRoot = do
  let p = pathsFor dir
  createDirectoryIfMissing True dir
  running <- busRunning p
  case running of
    Just pid -> putStrLn $ "bus already running (pid " <> show (pidToInt pid) <> ")"
    Nothing -> do
      mpid <- readPidFile (busPid p)
      case mpid of
        Just pid -> signalKill pid
        Nothing -> pure ()
      void $ try @IOException $ removeFile (busPid p)
      void $ try @IOException $ removeFile (busFifo p)
      createNamedPipe (busFifo p) stdFileMode
      appendFile (busLog p) ""
      appendFile (busErr p) ""
      exe <- getExecutablePath
      errH <- openFile (busErr p) AppendMode
      let cp =
            ( proc
                exe
                [ "-c",
                  channel,
                  "--bus-root",
                  busRoot,
                  "bus",
                  "daemon"
                ]
            )
              { std_in = NoStream,
                std_out = NoStream,
                std_err = UseHandle errH,
                cwd = Just dir,
                close_fds = True
              }
      (_, _, _, _ph) <- createProcess cp
      hClose errH
      threadDelay 400000
      still <- busRunning p
      case still of
        Just pid ->
          putStrLn $ "bus up (pid " <> show (pidToInt pid) <> ")  log=" <> busLog p
        Nothing -> do
          hPutStrLn stderr $ "bus FAILED to start; see " <> busErr p
          exitWith (ExitFailure 1)

busStop :: FilePath -> IO ()
busStop dir = do
  let p = pathsFor dir
  mpid <- readPidFile (busPid p)
  case mpid of
    Nothing -> putStrLn "bus not running"
    Just pid -> do
      signalKill pid
      void $ try @IOException $ removeFile (busPid p)
      putStrLn "bus stopped"

busStatus :: FilePath -> IO ()
busStatus dir = do
  let p = pathsFor dir
  running <- busRunning p
  nLines <- countLogLines dir
  case running of
    Just pid ->
      putStrLn $
        "alive (pid "
          <> show (pidToInt pid)
          <> ")  log="
          <> busLog p
          <> "  lines="
          <> show nLines
    Nothing -> do
      mpid <- readPidFile (busPid p)
      let pidS = maybe "none" (show . pidToInt) mpid
      putStrLn $
        "DOWN (pidfile="
          <> pidS
          <> "; no healthy bus on "
          <> busFifo p
          <> ")"

-- | Compact liveness for channel listings.
busLiveness :: FilePath -> IO String
busLiveness dir = do
  running <- busRunning (pathsFor dir)
  pure $ case running of
    Just pid -> "alive (pid " <> show (pidToInt pid) <> ")"
    Nothing -> "down"

-- ---------------------------------------------------------------------------
-- Post / read / wait / log
-- ---------------------------------------------------------------------------

-- | Post a framed message to the channel. Fails (as an 'IOException') if
-- the bus is down or the log does not grow — callers may catch; the CLI
-- dies with the same message and exit 1 as before.
post :: FilePath -> String -> Text -> IO ()
post dir sender body = do
  let p = pathsFor dir
  running <- busRunning p
  case running of
    Nothing -> do
      hPutStrLn stderr "post FAILED: bus is down (run: muster bus start)"
      ioError (userError "post FAILED: bus is down")
    Just _ -> do
      before <- countLogLines dir
      ts <- formatNow
      let sender' = T.pack sender
          framed = frameMessage ts sender' body <> "\n"
      bracket (openFile (busFifo p) WriteMode) hClose \h -> do
        hSetEncoding h utf8
        hSetBuffering h NoBuffering
        TIO.hPutStr h framed
        hFlush h
      ok <- checkGrew (busLog p) before 20
      unless ok do
        after <- countLogLines dir
        let msg =
              "post WARNING: log did not grow ("
                <> show before
                <> "->"
                <> show after
                <> ") — bus may be wedged"
        hPutStrLn stderr msg
        ioError (userError msg)

checkGrew :: FilePath -> Int -> Int -> IO Bool
checkGrew logPath before n = do
  after <- T.count "\n" <$> TIO.readFile logPath
  if after > before
    then pure True
    else
      if n <= 0
        then pure False
        else threadDelay 50000 >> checkGrew logPath before (n - 1)

-- | Read new lines since the cursor file and print them.
readNew :: FilePath -> FilePath -> IO ()
readNew dir cursorfile = do
  let p = pathsFor dir
  createDirectoryIfMissing True dir
  c <- Cur.newFile cursorfile
  ls <- Cur.pollFile c (busLog p)
  mapM_ TIO.putStrLn ls

-- | Read the last @n@ lines of the log, advancing the cursor to the end.
--
-- This is the default behaviour for @muster read@: it gives a newcomer (or a
-- long-polling session) a bounded window instead of dumping the entire backlog.
readTail :: FilePath -> FilePath -> Int -> IO ()
readTail dir cursorfile n = do
  let p = pathsFor dir
      logPath = busLog p
  createDirectoryIfMissing True dir
  c <- Cur.newFile cursorfile
  ls <-
    doesFileExist logPath >>= \case
      False -> pure []
      True -> T.lines <$> TIO.readFile logPath
  Cur.seekEnd c ls
  mapM_ TIO.putStrLn (takeLast n ls)

-- | Last @n@ elements of a list.
takeLast :: Int -> [a] -> [a]
takeLast n xs
  | n <= 0 = []
  | otherwise = drop (max 0 (length xs - n)) xs

-- | Block until a message matching the filter appears.
--
-- The filter predicate receives each raw log line. Return 'True' to wake
-- and print; 'False' to skip (the line is not printed and the wait
-- continues). Pass @const True@ to wake on any message.
--
-- When messages arrive but none pass the filter (e.g. self-posts), the
-- elapsed counter is reset to zero — the bus IS alive and delivering,
-- just not messages this watcher cares about.
wait ::
  FilePath ->
  FilePath ->
  Int ->
  (Text -> Bool) ->
  IO ExitCode
wait dir cursorfile timeoutSec keep = do
  let p = pathsFor dir
      wpid = watchPidFile dir cursorfile
  existing <- readPidFile wpid
  case existing of
    Just epid -> do
      alive <- processAlive epid
      when alive do
        putStrLn $
          "(wait: watcher already live pid "
            <> show (pidToInt epid)
            <> " — not stacking)"
        exitWith (ExitFailure 3)
    Nothing -> pure ()
  self <- getProcessID
  writeFile wpid (show (pidToInt self) <> "\n")
  let cleanup = void $ try @IOException $ removeFile wpid
  ( do
      c <- Cur.newFile cursorfile
      go c p 0
    )
    `finally` cleanup
  where
    go :: Cur.Cursor -> BusPaths -> Int -> IO ExitCode
    go c p elapsed = do
      ls <- Cur.pollFile c (busLog p)
      if not (null ls)
        then do
          let woke = filter keep ls
          if null woke
            then -- Messages arrived but none passed filter (e.g. self-posts).
            -- The bus is alive but not for us; count the polling interval
            -- toward the timeout rather than waiting forever.
                 threadDelay 1000000 >> go c p (elapsed + 1)
            else do
              mapM_ TIO.putStrLn woke
              hFlush stdout
              pure ExitSuccess
        else
          if elapsed >= timeoutSec
            then do
              putStrLn $ "(wait: no new messages in " <> show timeoutSec <> "s)"
              pure (ExitFailure 2)
            else do
              threadDelay 1000000
              go c p (elapsed + 1)

-- | Dump the full channel log to stdout.
dumpLog :: FilePath -> IO ()
dumpLog dir = do
  let p = pathsFor dir
  exists <- doesFileExist (busLog p)
  when exists $ TIO.readFile (busLog p) >>= TIO.putStr
