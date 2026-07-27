{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | High-level muster channel handle.
--
-- Product-side channel transport over the native muster bus: layout + daemon
-- ownership here; framing and cursor core from 'circuits-agent'. Does not use
-- 'Circuit.Agent.Comm' (cat-FIFO absorb leftover).
module Muster.Channel
  ( ChannelConfig (..),
    Channel,
    channelOpen,
    channelAttach,
    channelClose,
    channelSend,
    channelRecv,
    channelRecvRaw,
    channelRecvBlocking,
    defaultChannelConfig,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Cursor qualified as Cur
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muster.Bus qualified as Bus
import Muster.Framing (formatNow, frameMessage, parseMessage)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO
  ( BufferMode (NoBuffering),
    Handle,
    IOMode (WriteMode),
    hClose,
    hFlush,
    hSetBuffering,
    hSetEncoding,
    openFile,
    utf8,
  )
import Prelude

-- | Channel configuration.
data ChannelConfig = ChannelConfig
  { chName :: Text,
    chChannel :: String,
    chBusRoot :: FilePath
  }
  deriving (Show, Eq)

-- | A connected channel handle.
--
-- Persistent FIFO write end + file cursor on the channel log. Optional
-- ownership of the per-channel bus start/stop lifecycle.
data Channel = Channel
  { chCfg :: ChannelConfig,
    chDir :: FilePath,
    chWriteH :: Handle,
    chCursor :: Cur.Cursor,
    chOwnsBus :: Bool
  }

-- | Defaults for a local channel.
defaultChannelConfig :: Text -> ChannelConfig
defaultChannelConfig name =
  ChannelConfig
    { chName = name,
      chChannel = "bus",
      chBusRoot = ""
    }

-- | Resolve the bus root directory.
resolveRoot :: FilePath -> IO FilePath
resolveRoot explicit
  | not (null explicit) = pure explicit
  | otherwise = (</> ".config/muster") <$> getEnv "HOME"

-- | Directory for a specific channel.
channelDir :: ChannelConfig -> IO FilePath
channelDir cfg = do
  root <- resolveRoot (chBusRoot cfg)
  pure (root </> chChannel cfg)

cursorPath :: ChannelConfig -> FilePath -> FilePath
cursorPath cfg dir = dir </> ".cursor-" <> T.unpack (chName cfg)

logPath :: FilePath -> FilePath
logPath dir = dir </> "log.md"

fifoPath :: FilePath -> FilePath
fifoPath dir = dir </> "bus.fifo"

-- | Open a channel, starting the bus if necessary.
--
-- The returned handle owns the bus daemon. Use 'channelClose' to tear down.
channelOpen :: ChannelConfig -> IO Channel
channelOpen cfg = do
  dir <- channelDir cfg
  Bus.busStart dir (chChannel cfg) (chBusRoot cfg)
  ch <- attachDir cfg dir
  pure ch {chOwnsBus = True}

-- | Attach to an existing channel without starting the bus.
--
-- Fresh cursors (position 0) seek to the current end of the log so the next
-- recv only sees future traffic. Existing cursor files resume.
channelAttach :: ChannelConfig -> IO Channel
channelAttach cfg = do
  dir <- channelDir cfg
  createDirectoryIfMissing True dir
  attachDir cfg dir

attachDir :: ChannelConfig -> FilePath -> IO Channel
attachDir cfg dir = do
  let logFile = logPath dir
      curP = cursorPath cfg dir
  -- Ensure log exists so poll/seek do not race a missing file.
  exists <- doesFileExist logFile
  when (not exists) $ appendFile logFile ""
  c <- Cur.newFile curP
  pos <- Cur.get c
  when (pos == 0) $ Cur.seekEndFile c logFile
  writeH <- openFile (fifoPath dir) WriteMode
  hSetEncoding writeH utf8
  hSetBuffering writeH NoBuffering
  pure
    Channel
      { chCfg = cfg,
        chDir = dir,
        chWriteH = writeH,
        chCursor = c,
        chOwnsBus = False
      }

-- | Close a channel handle.
channelClose :: Channel -> IO ()
channelClose ch = do
  hClose (chWriteH ch)
  when (chOwnsBus ch) $ Bus.busStop (chDir ch)

-- | Send a framed message to the channel.
channelSend :: Channel -> Text -> IO ()
channelSend ch body = do
  ts <- formatNow
  TIO.hPutStrLn (chWriteH ch) (frameMessage ts (chName (chCfg ch)) body)
  hFlush (chWriteH ch)

-- | Receive all new messages since the last poll.
channelRecv :: Channel -> IO [(Text, Text)]
channelRecv ch = mapMaybe parseMessage <$> channelRecvRaw ch

-- | Receive all new raw log lines since the last poll.
channelRecvRaw :: Channel -> IO [Text]
channelRecvRaw ch = Cur.pollFile (chCursor ch) (logPath (chDir ch))

-- | Block until new messages arrive, or the timeout fires (microseconds).
channelRecvBlocking :: Channel -> Int -> IO (Maybe [(Text, Text)])
channelRecvBlocking ch timeoutUs = go 0 10000
  where
    go elapsed delay = do
      msgs <- channelRecv ch
      if not (null msgs)
        then pure (Just msgs)
        else do
          let elapsed' = elapsed + delay
          if elapsed' >= timeoutUs
            then pure Nothing
            else do
              threadDelay delay
              let delay' = min 500000 (floor (fromIntegral delay * 1.5 :: Double))
              go elapsed' delay'
