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
module Mailbox
  ( MailboxPaths (..),
    pathsFor,
    busStart,
    busStop,
    busStatus,
    busDaemon,
    post,
    readNew,
    wait,
    dumpLog,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (forever, unless, void, when)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
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
    hGetLine,
    hIsEOF,
    hPutStrLn,
    hSetBuffering,
    openFile,
    stderr,
    stdout,
  )
import System.Posix.Files (createNamedPipe, stdFileMode)
import System.Posix.IO
  ( OpenMode (..),
    defaultFileFlags,
    fdToHandle,
    openFd,
  )
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

data MailboxPaths = MailboxPaths
  { mbDir :: FilePath,
    mbFifo :: FilePath,
    mbLog :: FilePath,
    mbErr :: FilePath,
    mbPid :: FilePath
  }
  deriving (Show, Eq)

pathsFor :: FilePath -> MailboxPaths
pathsFor dir =
  MailboxPaths
    { mbDir = dir,
      mbFifo = dir </> "bus.fifo",
      mbLog = dir </> "log.md",
      mbErr = dir </> "err.md",
      mbPid = dir </> "bus.pid"
    }

watchPidFile :: FilePath -> FilePath -> FilePath
watchPidFile dir cursorfile =
  -- Use takeFileName (not takeBaseName): cursors are ".watch-NAME" / ".cursor-NAME"
  -- and takeBaseName would strip ".NAME" as a fake extension, collapsing every
  -- watcher onto one pidfile (".watch.pid-.watch") — multi-watcher death.
  let base = takeFileName cursorfile
      -- Match bash: watchname="${cursorbase#.cursor-}"
      watchname = fromMaybe base (stripPrefix' ".cursor-" base)
   in dir </> ".watch.pid-" <> watchname
  where
    stripPrefix' p s
      | p `isPrefixOf` s = Just (drop (length p) s)
      | otherwise = Nothing

pidToInt :: ProcessID -> Int
pidToInt (CPid x) = fromIntegral x

-- ---------------------------------------------------------------------------
-- Line counting (match `wc -l`: count newline bytes)
-- ---------------------------------------------------------------------------

countNewlines :: FilePath -> IO Int
countNewlines path = do
  exists <- doesFileExist path
  if not exists
    then pure 0
    else T.count "\n" <$> TIO.readFile path

readCursor :: FilePath -> IO Int
readCursor cursorfile = do
  exists <- doesFileExist cursorfile
  if not exists
    then pure 0
    else do
      raw <- readFile cursorfile
      pure $ fromMaybe 0 $ readMaybe (filter (not . isSpace) raw)

writeCursor :: FilePath -> Int -> IO ()
writeCursor cursorfile n = writeFile cursorfile (show n <> "\n")

-- | Message lines after the cursor.  Cursor stores a @wc -l@ count; with a
-- trailing newline on every framed post the listed-line count equals that.
linesAfter :: FilePath -> Int -> IO [Text]
linesAfter logPath cur = do
  exists <- doesFileExist logPath
  if not exists
    then pure []
    else drop cur . T.lines <$> TIO.readFile logPath

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

busRunning :: MailboxPaths -> IO (Maybe ProcessID)
busRunning p = do
  mpid <- readPidFile (mbPid p)
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
-- Daemon (replaces `exec cat <>fifo`)
-- ---------------------------------------------------------------------------

-- | Long-running bus: open FIFO RDWR (never EOF between writers) and append
-- each line to the log.
busDaemon :: FilePath -> IO ()
busDaemon dir = do
  let p = pathsFor dir
  createDirectoryIfMissing True dir
  -- Open FIFO read-write: keeps a write-end open so readers never see EOF
  -- between messages (posix @open(O_RDWR)@ ≡ bash @cat \<>fifo@).
  fd <- openFd (mbFifo p) ReadWrite defaultFileFlags
  h <- fdToHandle fd
  hSetBuffering h LineBuffering
  logH <- openFile (mbLog p) AppendMode
  hSetBuffering logH NoBuffering
  self <- getProcessID
  writeFile (mbPid p) (show (pidToInt self) <> "\n")
  relay h logH `finally` (hClose h >> hClose logH)
  where
    relay :: Handle -> Handle -> IO ()
    relay h logH = forever do
      eof <- hIsEOF h
      if eof
        then threadDelay 100000 -- should not happen with RDWR; back off
        else do
          line <- hGetLine h
          hPutStrLn logH line
          hFlush logH

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Start the bus daemon by re-execing this binary as @bus daemon@.
--
-- @channel@ and @busRoot@ are passed so the child resolves the same channel
-- directory the parent used.
busStart :: FilePath -> String -> FilePath -> IO ()
busStart dir channel busRoot = do
  let p = pathsFor dir
  createDirectoryIfMissing True dir
  running <- busRunning p
  case running of
    Just pid -> putStrLn $ "bus already running (pid " <> show (pidToInt pid) <> ")"
    Nothing -> do
      -- Clear stale pid / fifo (split-inode guard: always recreate fifo)
      mpid <- readPidFile (mbPid p)
      case mpid of
        Just pid -> signalKill pid
        Nothing -> pure ()
      void $ try @IOException $ removeFile (mbPid p)
      void $ try @IOException $ removeFile (mbFifo p)
      createNamedPipe (mbFifo p) stdFileMode
      appendFile (mbLog p) ""
      appendFile (mbErr p) ""
      exe <- getExecutablePath
      errH <- openFile (mbErr p) AppendMode
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
      -- errH is now owned by the child; do not close here if UseHandle transferred.
      -- With UseHandle, parent must not use it further; closing is OK after spawn
      -- only if the child dup'd — process docs say parent should close. Safe:
      hClose errH
      threadDelay 400000
      still <- busRunning p
      case still of
        Just pid ->
          putStrLn $ "bus up (pid " <> show (pidToInt pid) <> ")  log=" <> mbLog p
        Nothing -> do
          hPutStrLn stderr $ "bus FAILED to start; see " <> mbErr p
          exitWith (ExitFailure 1)

busStop :: FilePath -> IO ()
busStop dir = do
  let p = pathsFor dir
  mpid <- readPidFile (mbPid p)
  case mpid of
    Nothing -> putStrLn "bus not running"
    Just pid -> do
      signalKill pid
      void $ try @IOException $ removeFile (mbPid p)
      putStrLn "bus stopped"

busStatus :: FilePath -> IO ()
busStatus dir = do
  let p = pathsFor dir
  running <- busRunning p
  nLines <- countNewlines (mbLog p)
  case running of
    Just pid ->
      putStrLn $
        "alive (pid "
          <> show (pidToInt pid)
          <> ")  log="
          <> mbLog p
          <> "  lines="
          <> show nLines
    Nothing -> do
      mpid <- readPidFile (mbPid p)
      let pidS = maybe "none" (show . pidToInt) mpid
      putStrLn $
        "DOWN (pidfile="
          <> pidS
          <> "; no healthy bus on "
          <> mbFifo p
          <> ")"

-- ---------------------------------------------------------------------------
-- Post / read / wait / log
-- ---------------------------------------------------------------------------

post :: FilePath -> String -> Text -> IO ()
post dir sender body = do
  let p = pathsFor dir
  running <- busRunning p
  case running of
    Nothing -> do
      hPutStrLn stderr "post FAILED: bus is down (run: muster bus start)"
      exitWith (ExitFailure 1)
    Just _ -> do
      before <- countNewlines (mbLog p)
      let framed = T.pack ("[" <> sender <> "] ") <> body <> "\n"
      bracket (openFile (mbFifo p) WriteMode) hClose \h -> do
        hSetBuffering h NoBuffering
        TIO.hPutStr h framed
        hFlush h
      ok <- checkGrew (mbLog p) before 20
      unless ok do
        after <- countNewlines (mbLog p)
        hPutStrLn stderr $
          "post WARNING: log did not grow ("
            <> show before
            <> "->"
            <> show after
            <> ") — bus may be wedged"
        exitWith (ExitFailure 1)

checkGrew :: FilePath -> Int -> Int -> IO Bool
checkGrew logPath before n = do
  after <- countNewlines logPath
  if after > before
    then pure True
    else
      if n <= 0
        then pure False
        else threadDelay 50000 >> checkGrew logPath before (n - 1)

readNew :: FilePath -> FilePath -> IO ()
readNew dir cursorfile = do
  let p = pathsFor dir
  createDirectoryIfMissing True dir
  exists <- doesFileExist cursorfile
  unless exists $ writeFile cursorfile "0\n"
  cur <- readCursor cursorfile
  total <- countNewlines (mbLog p)
  when (total > cur) do
    ls <- linesAfter (mbLog p) cur
    mapM_ TIO.putStrLn ls
  writeCursor cursorfile total

-- | Block until a non-excluded message appears. Exit 0 with printed lines,
-- 2 on timeout, 3 if another watcher is already live for this cursor.
wait ::
  FilePath ->
  FilePath ->
  Int ->
  String -> -- exclude fixed-string prefix (e.g. "[grok] ")
  IO ExitCode
wait dir cursorfile timeoutSec exclude = do
  let p = pathsFor dir
      wpid = watchPidFile dir cursorfile
  -- Guard: refuse second watcher
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
      exists <- doesFileExist cursorfile
      unless exists $ writeFile cursorfile "0\n"
      cur0 <- readCursor cursorfile
      go p cur0 0
    )
    `finally` cleanup
  where
    go :: MailboxPaths -> Int -> Int -> IO ExitCode
    go p cur elapsed = do
      total <- countNewlines (mbLog p)
      if total > cur
        then do
          ls <- linesAfter (mbLog p) cur
          writeCursor cursorfile total
          let woke =
                if null exclude
                  then ls
                  else filter (not . (T.pack exclude `T.isPrefixOf`)) ls
          if null woke
            then go p total elapsed -- only own/filtered traffic
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
              go p cur (elapsed + 1)

dumpLog :: FilePath -> IO ()
dumpLog dir = do
  let p = pathsFor dir
  exists <- doesFileExist (mbLog p)
  when exists $ TIO.readFile (mbLog p) >>= TIO.putStr
