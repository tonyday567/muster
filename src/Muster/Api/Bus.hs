{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Central bus daemon for the rebuilt muster API.
--
-- One daemon owns every channel under a 'BusRoot'. It multiplexes channels on
-- demand: new channel directories are picked up by a scan loop and each gets a
-- lightweight relay thread running 'Muster.Bus.busDaemon'. Per-channel FIFOs,
-- logs and cursors live in the channel directory exactly as before; only the
-- ownership model changes from one daemon per channel to one daemon for all.
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
    channelPath,
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
import Control.Concurrent.Async (Async, async, cancel, pollSTM)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Exception (IOException, SomeException, bracket, catch, finally, try)
import Control.Monad (forever, forM, unless, void, when)
import Circuit.Parser.Json (Json (..), encodeJson)
import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Vector qualified as V
import Muster.Api.Types (BusRoot (..), Channel (..), Nick (..))
import Muster.Bus qualified as Bus
import Muster.Cursor qualified as Cur
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
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

-- | Filesystem path of a channel directory.
--
-- >>> channelPath (BusRoot "/tmp/muster") (Channel "bus")
-- "/tmp/muster/bus"
channelPath :: BusRoot -> Channel -> FilePath
channelPath root (Channel c) = busRootPath root </> T.unpack c

-- | Filesystem path of a participant's join cursor inside a channel.
--
-- >>> cursorPath (BusRoot "/tmp/muster") (Channel "bus") (Nick "kimi")
-- "/tmp/muster/bus/.cursor-kimi"
cursorPath :: BusRoot -> Channel -> Nick -> FilePath
cursorPath root chan (Nick n) = channelPath root chan </> ".cursor-" <> T.unpack n

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

-- | List channel directories under a bus root, excluding reserved names.
--
-- >>> let root = BusRoot "/tmp/muster-doctest-list"
-- >>> createDirectoryIfMissing True (busRootPath root </> "bus")
-- >>> listChannels root
-- [Channel {unChannel = "bus"}]
listChannels :: BusRoot -> IO [Channel]
listChannels root = do
  let dir = busRootPath root
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- listDirectory dir
      chans <-
        forM entries $ \e -> do
          let name = T.pack e
              c = Channel name
          isDir <- doesDirectoryExist (dir </> e)
          pure
            $ if isDir && not (isReservedChannel c) && not ("_" `T.isPrefixOf` name) && not ("." `T.isPrefixOf` name)
              then Just c
              else Nothing
      pure $ Set.toAscList $ Set.fromList $ catMaybes chans

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

-- | Create a channel directory and FIFO if they do not already exist.
ensureChannel :: BusRoot -> Channel -> IO FilePath
ensureChannel root chan = do
  let dir = channelPath root chan
      p = Bus.pathsFor dir
  createDirectoryIfMissing True dir
  fifoExists <- doesFileExist (Bus.busFifo p)
  unless fifoExists $
    void $
      try @SomeException $
        createNamedPipe (Bus.busFifo p) stdFileMode
  -- Touch the log files so readers and post's growth check do not race with
  -- the relay creating them.
  logExists <- doesFileExist (Bus.busLog p)
  unless logExists $ appendFile (Bus.busLog p) ""
  errExists <- doesFileExist (Bus.busErr p)
  unless errExists $ appendFile (Bus.busErr p) ""
  pure dir

-- | Wait up to two seconds for a channel relay to come alive.
--
-- This smooths the race between creating a channel and the central daemon's
-- scan loop picking it up.
waitForChannel :: BusRoot -> Channel -> IO Bool
waitForChannel root chan = go (20 :: Int)
  where
    go 0 = pure False
    go n = do
      dir <- ensureChannel root chan
      alive <- Bus.busRunning (Bus.pathsFor dir)
      case alive of
        Just _ -> pure True
        Nothing -> threadDelay 100000 >> go (n - 1)

-- | Relay a single channel. Exceptions are caught so one channel cannot crash
-- the central daemon; the scan loop will restart it on the next pass.
channelRelay :: BusRoot -> Channel -> IO ()
channelRelay root chan = do
  self <- getProcessID
  _dir <- ensureChannel root chan
  writeFile (Bus.busPid p) (show (pidToInt self) <> "\n")
  -- Log appends go through a raw POSIX fd, held open for the relay's life.
  -- GHC 'openFile' takes an advisory lock (exclusive for AppendMode) in the
  -- RTS lock table: in-process readers (post's growth check, the ws pump)
  -- then intermittently collide with a per-line open, and the collision
  -- used to cost the line already consumed from the FIFO.  A raw fd takes
  -- no GHC lock at all, so readers never see us and no line is lost.
  bracket openLog closeFd $ \logFd ->
    forever (relayOnce logFd)
  where
    p = Bus.pathsFor (channelPath root chan)
    openLog =
      openFd
        (Bus.busLog p)
        WriteOnly
        defaultFileFlags {append = True, creat = Just stdFileMode}
    relayOnce logFd =
      bracket (openFifo p) hClose (\h -> forever (TIO.hGetLine h >>= appendLine logFd))
        `catch` \(e :: IOException) -> dbg e
    dbg :: IOException -> IO ()
    dbg e = do
      ts <- take 19 . show <$> getCurrentTime
      let tag = if isEOFError e then "eof-reopen" else "relay-exception"
      appendFile (Bus.busErr p) (ts <> " " <> tag <> " " <> show e <> "\n")
        `catch` \(_ :: IOException) -> pure ()
    openFifo p' = do
      h <- openFile (Bus.busFifo p') ReadWriteMode
      hSetEncoding h utf8
      hSetBuffering h LineBuffering
      pure h
    appendLine logFd line = go (encodeUtf8 (line <> "\n"))
      where
        go bs
          | BS.null bs = pure ()
          | otherwise = do
              n <- fdWrite logFd bs
              go (BS.drop (fromIntegral n) bs)

-- | One scan pass: make sure every existing channel has a live relay thread,
-- and drop relays for channels that have disappeared.
scanChannels :: BusRoot -> TVar (Map Channel (Async ())) -> IO ()
scanChannels root active = do
  chans <- listChannels root
  let wanted = Set.fromList chans
  -- Start or keep relays for wanted channels.
  mapM_
    ( \chan -> do
        need <- atomically $ do
          m <- readTVar active
          case Map.lookup chan m of
            Nothing -> pure True
            Just a -> do
              mp <- pollSTM a
              pure $ case mp of
                Nothing -> False
                Just _ -> True
        when need $ do
          a <- async (channelRelay root chan)
          atomically $ modifyTVar' active (Map.insert chan a)
    )
    chans
  -- Cancel relays for removed channels.
  gone <-
    atomically $ do
      m <- readTVar active
      let (keep, remove) = Map.partitionWithKey (\k _ -> k `Set.member` wanted) m
      writeTVar active keep
      pure remove
  mapM_ cancel gone

-- | Run the central bus daemon forever.
--
-- This is the entry point for the background daemon process. It owns the bus
-- root, writes its pid to @muster.pid@, and keeps relay threads alive for every
-- channel under the root.
centralDaemon :: BusRoot -> IO ()
centralDaemon root = do
  ensureBusRoot root
  writeRootPid root
  active <- newTVarIO Map.empty
  let cleanup = atomically (readTVar active) >>= mapM_ cancel
  (forever $ scanChannels root active >> threadDelay 1_000_000) `finally` cleanup

-- | Run the central daemon in-process for the duration of an action.
--
-- This is convenient for tests and local scripts. The daemon and all of its
-- channel relays are cancelled when the action finishes.
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
  let dir = channelPath root chan
  Bus.post dir (T.unpack (unNick nick)) body

-- | Like 'post' (same waited commit, same failure behaviour), but records
-- the thread ancestry in a @dag.md@ sidecar: after the log grows, one JSON
-- line @{"from":sender,"thread":[names],"body":body}@ is appended to
-- @\<chanDir\>/dag.md@.
--
-- The bus log itself carries only @[ts] sender: body@ — ancestry is lost
-- there, so @dag.md@ is the honest wiring record.  Plain 'post' stays
-- untouched: dag coverage may be partial, and readers must treat a missing
-- entry as "no wiring recorded", never as an invented merge.
--
-- The append goes through a raw POSIX fd opened in append mode for the
-- call (O_APPEND), never GHC 'openFile' AppendMode — the RTS lock table
-- collision that cost lines in the relay applies here too.
--
-- dag.md is written /after/ the log commit, so for a single orchestrated
-- writer its lines are order-aligned with log.md.  Concurrent dag writers
-- can misalign (two posts interleave log-vs-dag appends); the stage-L4
-- runner owns that discipline.
postWithThread :: BusRoot -> Channel -> Nick -> [Text] -> Text -> IO ()
postWithThread root chan nick thread body = do
  post root chan nick body
  let entry =
        JObject
          [ ("from", JString (unNick nick)),
            ("thread", JArray (V.fromList (map JString thread))),
            ("body", JString body)
          ]
      line = encodeJson entry <> "\n"
  bracket openDag closeFd (writeAll line)
  where
    openDag =
      openFd
        (channelPath root chan </> "dag.md")
        WriteOnly
        defaultFileFlags {append = True, creat = Just stdFileMode}
    writeAll bs fd
      | BS.null bs = pure ()
      | otherwise = do
          n <- fdWrite fd bs
          writeAll (BS.drop (fromIntegral n) bs) fd

-- | Read all unread lines for a participant on a channel, advancing the cursor.
readNext :: BusRoot -> Channel -> Nick -> IO [Text]
readNext root chan nick = do
  let dir = channelPath root chan
      cursor = cursorPath root chan nick
      logPath = Bus.busLog (Bus.pathsFor dir)
  c <- Cur.newFile cursor
  Cur.pollFile c logPath

-- | Read the last @n@ lines of a channel log, advancing the cursor to the end.
readTail :: BusRoot -> Channel -> Nick -> Int -> IO [Text]
readTail root chan nick n = do
  let dir = channelPath root chan
      cursor = cursorPath root chan nick
      logPath = Bus.busLog (Bus.pathsFor dir)
  exists <- doesFileExist logPath
  ls <-
    if exists
      then T.lines <$> TIO.readFile logPath
      else pure []
  c <- Cur.newFile cursor
  Cur.seekEnd c ls
  pure (takeLast n ls)
  where
    takeLast m xs
      | m <= 0 = []
      | otherwise = drop (max 0 (length xs - m)) xs
