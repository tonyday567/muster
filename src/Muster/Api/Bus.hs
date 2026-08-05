{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Central bus daemon for the rebuilt muster API.
--
-- Phase 5: one daemon owns one global FIFO and one global append-only log
-- under the 'BusRoot'.  Channel identity is carried in the @to@ field of each
-- post; readers filter the global log by channel.  Cursors are root-level files.
module Muster.Api.Bus
  ( -- * Central daemon
    BusHandle,
    centralDaemon,
    withBus,
    isRunning,
    startCentral,
    stopCentral,
    statusCentral,

    -- * Paths and channel discovery
    busRootPath,
    cursorPath,
    listChannels,
    isReservedChannel,

    -- * Bus root
    ensureBusRoot,

    -- * Channel lifecycle
    ensureChannel,
    waitForChannel,

    -- * Messaging
    post,
    postWithThread,
    readNext,
    readTail,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (IOException, SomeException, bracket, catch, finally, try)
import Control.Monad (forever, unless, void, when)
import Data.Char (isSpace)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)
import Circuit.Agent (Post (..), PostId)
import Circuit.Agent.Framing (Stamped (..), parseLineAt)
import Muster.Api.Types (BusRoot (..), Channel (..), Nick (..))
import Muster.Bus qualified as Bus
import Muster.Bus (matchesChannel)
import Muster.Cursor qualified as Cur
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    removePathForcibly,
  )
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO
  ( BufferMode (..),
    IOMode (..),
    hClose,
    hSetBuffering,
    hSetEncoding,
    openFile,
    utf8,
  )
import System.IO.Error (isEOFError)
import System.Posix.Files (createNamedPipe, stdFileMode)
import System.Posix.IO (OpenFileFlags (..), OpenMode (..), closeFd, defaultFileFlags, openFd)
import System.Posix.IO.ByteString (fdWrite)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigTERM, signalProcess)
import System.Posix.Types (CPid (..), Fd, ProcessID)
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

-- $setup
-- >>> :set -XOverloadedStrings

-- | Handle to a central bus daemon running in the current process.
--
-- This is primarily useful for tests and in-process orchestration. A
-- production CLI daemon runs 'centralDaemon' in its own process instead.
data BusHandle = BusHandle BusRoot (Async ())

-- | Filesystem path of a bus root.
--
-- >>> busRootPath (BusRoot "/tmp/muster")
-- "/tmp/muster"
busRootPath :: BusRoot -> FilePath
busRootPath = unBusRoot

-- | Filesystem path of a participant's join cursor inside the bus root.
--
-- Phase 5: cursors are root-level.
--
-- >>> cursorPath (BusRoot "/tmp/muster") (Channel "bus") (Nick "kimi")
-- "/tmp/muster/.cursor-kimi"
cursorPath :: BusRoot -> Channel -> Nick -> FilePath
cursorPath root _chan (Nick n) = busRootPath root </> ".cursor-" <> T.unpack n

-- | Channels reserved for muster internals. These are never treated as bus
-- channels by the central daemon.
--
-- >>> isReservedChannel (Channel "agents")
-- True
--
-- >>> isReservedChannel (Channel "bus")
-- False
reservedChannels :: Set Channel
reservedChannels = Set.fromList ["agents", "sessions"]

isReservedChannel :: Channel -> Bool
isReservedChannel = (`Set.member` reservedChannels)

-- | List channels that have traffic in the global log, excluding reserved
-- names.
--
-- Phase 5: channels are views into the global log, not directories.  This
-- reads the global log and collects channel names from the @to@ field.
--
-- >>> let root = BusRoot "/tmp/muster-doctest-list"
-- >>> createDirectoryIfMissing True (busRootPath root)
-- >>> listChannels root
-- []
listChannels :: BusRoot -> IO [Channel]
listChannels root = do
  let logPath = Bus.busLog (Bus.pathsFor (busRootPath root))
  exists <- doesFileExist logPath
  if not exists
    then pure []
    else do
      ls <- T.lines <$> TIO.readFile logPath
      pure
        $ Set.toAscList
        $ Set.filter (not . isReservedChannel)
        $ Set.fromList
        $ map Channel $ concat [to p | l <- ls, Just p <- [parseStored l]]
  where
    parseStored l = stamped <$> parseLineAt 0 l

-- | Create a bus root directory if it does not exist.
ensureBusRoot :: BusRoot -> IO ()
ensureBusRoot = createDirectoryIfMissing True . busRootPath

-- | Path to the central daemon's pid file.
rootPidPath :: BusRoot -> FilePath
rootPidPath root = busRootPath root </> "muster.pid"

pidToInt :: ProcessID -> Int
pidToInt (CPid x) = fromIntegral x

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
  where
    waitExit :: ProcessHandle -> Int -> IO ExitCode
    waitExit ph n = do
      mec <- getProcessExitCode ph
      case mec of
        Just e -> pure e
        Nothing
          | n > 0 -> threadDelay 10000 >> waitExit ph (n - 1)
          | otherwise -> pure (ExitFailure 1)

writeRootPid :: BusRoot -> IO ()
writeRootPid root = do
  self <- getProcessID
  writeFile (rootPidPath root) (show (pidToInt self) <> "\n")

-- | Check whether the central daemon for this root appears to be alive.
isRunning :: BusRoot -> IO Bool
isRunning root = do
  mpid <- readPidFile (rootPidPath root)
  case mpid of
    Nothing -> pure False
    Just pid -> processAlive pid

-- | Start the central daemon as a background process.
--
-- Returns 'True' if the daemon is running when the call returns.
startCentral :: BusRoot -> IO Bool
startCentral root = do
  ensureBusRoot root
  running <- isRunning root
  if running
    then pure True
    else do
      exe <- getExecutablePath
      void $
        createProcess
          (proc exe ["--bus-root", busRootPath root, "internal-daemon"])
            { std_in = NoStream,
              std_out = NoStream,
              std_err = NoStream,
              close_fds = True
            }
      wait (30 :: Int)
  where
    wait 0 = pure False
    wait n = do
      threadDelay 100000
      running <- isRunning root
      if running then pure True else wait (n - 1)

-- | Stop the central daemon.
stopCentral :: BusRoot -> IO ()
stopCentral root = do
  mpid <- readPidFile (rootPidPath root)
  case mpid of
    Nothing -> pure ()
    Just pid -> do
      alive <- processAlive pid
      when alive $ do
        void $ try @IOException $ signalProcess sigTERM pid
        waitExit pid (30 :: Int)
  void $ try @IOException $ removePathForcibly (rootPidPath root)
  where
    waitExit _ 0 = pure ()
    waitExit pid n = do
      alive <- processAlive pid
      when alive $ threadDelay 100000 >> waitExit pid (n - 1)

-- | Print a one-line status report for the central daemon.
statusCentral :: BusRoot -> IO ()
statusCentral root = do
  running <- isRunning root
  putStrLn $ if running then "bus running" else "bus down"

-- | Ensure the bus root, global FIFO, and global log files exist.
--
-- Phase 5: this is channel-agnostic.  Returns the bus root path.
ensureChannel :: BusRoot -> Channel -> IO FilePath
ensureChannel root _chan = do
  let rootPath = busRootPath root
      p = Bus.pathsFor rootPath
  createDirectoryIfMissing True rootPath
  fifoExists <- doesFileExist (Bus.busFifo p)
  unless fifoExists $
    void $
      try @SomeException $
        createNamedPipe (Bus.busFifo p) stdFileMode
  -- Touch the log files so readers and post's growth check do not race
  -- with the relay creating them.  Use a raw POSIX open/close, not GHC's
  -- 'appendFile': the relay holds these files open as raw fds, and two
  -- concurrent GHC 'openFile' calls can collide on the RTS lock table.
  touchFile (Bus.busLog p)
  touchFile (Bus.busErr p)
  pure rootPath
  where
    touchFile path =
      void $
        bracket
          (openFd path WriteOnly defaultFileFlags {append = True, creat = Just stdFileMode})
          closeFd
          (const (pure ()))
          `catch` \(_ :: IOException) -> pure ()

-- | Wait up to two seconds for the global daemon to come alive.
waitForChannel :: BusRoot -> Channel -> IO Bool
waitForChannel root _chan = go (20 :: Int)
  where
    go 0 = pure False
    go n = do
      _ <- ensureChannel root (Channel "bus")
      alive <- Bus.busRunning (Bus.pathsFor (busRootPath root))
      case alive of
        Just _ -> pure True
        Nothing -> threadDelay 100000 >> go (n - 1)

-- | Global relay: read the single root FIFO and append stamped lines to the
-- single root log.  Exceptions are caught so a transient FIFO issue does not
-- crash the daemon; the loop reopens everything and continues.
globalRelay :: BusRoot -> IO ()
globalRelay root = do
  self <- getProcessID
  -- Log appends go through a raw POSIX fd, held open for the relay's life.
  -- GHC 'openFile' takes an advisory lock (exclusive for AppendMode) in the
  -- RTS lock table: in-process readers (post's growth check, the ws pump)
  -- then intermittently collide with a per-line open, and the collision
  -- used to cost the consumed FIFO line.  A raw fd takes no GHC lock at
  -- all, so readers never see us and no line is lost.  The exception log
  -- gets the same treatment.
  _ <- ensureChannel root (Channel "bus")
  writeFile (Bus.busPid p) (show (pidToInt self) <> "\n")
  bracket openLog closeFd $ \logFd ->
    bracket openErr closeFd $ \errFd ->
      forever (relayOnce logFd errFd)
  where
    rootPath = busRootPath root
    p = Bus.pathsFor rootPath
    openLog =
      openFd
        (Bus.busLog p)
        WriteOnly
        defaultFileFlags {append = True, creat = Just stdFileMode}
    openErr =
      openFd
        (Bus.busErr p)
        WriteOnly
        defaultFileFlags {append = True, creat = Just stdFileMode}
    relayOnce logFd errFd =
      bracket (openFifo p) hClose (\h -> forever (TIO.hGetLine h >>= appendLine logFd))
        `catch` \(e :: IOException) -> dbg errFd e
    dbg errFd e = do
      ts <- take 19 . show <$> getCurrentTime
      let tag = if isEOFError e then "eof-reopen" else "relay-exception"
      fdWriteAll errFd (encodeUtf8 (T.pack (ts <> " " <> tag <> " " <> show e <> "\n")))
        `catch` \(_ :: IOException) -> pure ()
    openFifo p' = do
      h <- openFile (Bus.busFifo p') ReadWriteMode
      hSetEncoding h utf8
      hSetBuffering h LineBuffering
      pure h
    appendLine logFd line = do
      stampedLine <- Bus.stampLine (Bus.busLog p) line
      fdWriteAll logFd (encodeUtf8 (stampedLine <> "\n"))
    fdWriteAll :: Fd -> BS.ByteString -> IO ()
    fdWriteAll fd bs
      | BS.null bs = pure ()
      | otherwise = do
          n <- fdWrite fd bs
          fdWriteAll fd (BS.drop (fromIntegral n) bs)

-- | Run the central bus daemon forever.
--
-- This is the entry point for the background daemon process. It owns the bus
-- root, writes its pid to @muster.pid@, and runs the single global relay.
centralDaemon :: BusRoot -> IO ()
centralDaemon root = do
  ensureBusRoot root
  writeRootPid root
  let cleanup = pure ()
  (globalRelay root) `finally` cleanup

-- | Run the central daemon in-process for the duration of an action.
--
-- This is convenient for tests and local scripts. The daemon and its global
-- relay are cancelled when the action finishes.
withBus :: BusRoot -> (BusHandle -> IO a) -> IO a
withBus root action = do
  ensureBusRoot root
  a <- async (centralDaemon root)
  action (BusHandle root a) `finally` cancel a

-- | Post a message to a channel as a named participant.
post :: BusRoot -> Channel -> Nick -> Text -> IO ()
post root chan nick body = do
  ok <- waitForChannel root chan
  unless ok $ fail $ "muster: bus not running for channel " <> T.unpack (unChannel chan)
  let rootPath = busRootPath root
  Bus.post rootPath (unNick nick) [unChannel chan] [] body

-- | Like 'post' but also records thread ancestry in the stamped log line.
postWithThread :: BusRoot -> Channel -> Nick -> [PostId] -> Text -> IO ()
postWithThread root chan nick thread body = do
  ok <- waitForChannel root chan
  unless ok $ fail $ "muster: bus not running for channel " <> T.unpack (unChannel chan)
  let rootPath = busRootPath root
  Bus.post rootPath (unNick nick) [unChannel chan] thread body

-- | Read all unread raw log lines for a participant on a channel, advancing
-- the cursor.  Callers that want human display should render with
-- 'Circuit.Agent.Framing.renderStored'.
readNext :: BusRoot -> Channel -> Nick -> IO [Text]
readNext root chan nick = do
  let rootPath = busRootPath root
      cursor = cursorPath root chan nick
      logPath = Bus.busLog (Bus.pathsFor rootPath)
  c <- Cur.newFile cursor
  ls <- Cur.pollFile c logPath
  pure (filter (matchesChannel (unChannel chan)) ls)

-- | Read the last @n@ raw log lines of a channel, advancing the cursor to the
-- end of the global log.
readTail :: BusRoot -> Channel -> Nick -> Int -> IO [Text]
readTail root chan nick n = do
  let rootPath = busRootPath root
      cursor = cursorPath root chan nick
      logPath = Bus.busLog (Bus.pathsFor rootPath)
  exists <- doesFileExist logPath
  ls <-
    if exists
      then T.lines <$> TIO.readFile logPath
      else pure []
  c <- Cur.newFile cursor
  Cur.seekEnd c ls
  pure (takeLast n (filter (matchesChannel (unChannel chan)) ls))
  where
    takeLast m xs
      | m <= 0 = []
      | otherwise = drop (max 0 (length xs - m)) xs
