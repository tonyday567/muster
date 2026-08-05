{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | High-level muster channel handle.
--
-- Product-side channel transport over the native muster bus: layout + daemon
-- ownership here; framing and cursor core from 'circuits-agent'. Does not use
-- 'Circuit.Agent.Comm' (cat-FIFO absorb leftover).
--
-- Phase 5: paths are root-level.  The channel label is still carried in
-- 'ChannelConfig' but the directory is the bus root; readers filter the global
-- log by channel name.
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

import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (framePost, parseMessage)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Cursor qualified as Cur
import Data.Text.IO qualified as TIO
import Muster.Bus qualified as Bus
import Muster.Bus (matchesChannel)
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
-- Persistent FIFO write end + file cursor on the global log. Optional
-- ownership of the bus start/stop lifecycle.
data Channel = Channel
  { chCfg :: ChannelConfig,
    chRoot :: FilePath,
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
--
-- Phase 5: the channel no longer has its own directory; this returns the bus
-- root.
channelDir :: ChannelConfig -> IO FilePath
channelDir cfg = resolveRoot (chBusRoot cfg)

cursorPath :: ChannelConfig -> FilePath -> FilePath
cursorPath cfg dir = dir </> ".cursor-" <> T.unpack (chName cfg)

logPath :: FilePath -> FilePath
logPath root = Bus.busLog (Bus.pathsFor root)

fifoPath :: FilePath -> FilePath
fifoPath root = Bus.busFifo (Bus.pathsFor root)

-- | Open a channel, starting the bus if necessary.
--
-- The returned handle owns the bus daemon. Use 'channelClose' to tear down.
channelOpen :: ChannelConfig -> IO Channel
channelOpen cfg = do
  root <- channelDir cfg
  Bus.busStart root
  ch <- attachRoot cfg root
  pure ch {chOwnsBus = True}

-- | Attach to an existing channel without starting the bus.
--
-- Fresh cursors (position 0) seek to the current end of the log so the next
-- recv only sees future traffic. Existing cursor files resume.
channelAttach :: ChannelConfig -> IO Channel
channelAttach cfg = do
  root <- channelDir cfg
  createDirectoryIfMissing True root
  attachRoot cfg root

attachRoot :: ChannelConfig -> FilePath -> IO Channel
attachRoot cfg root = do
  let logFile = logPath root
      curP = cursorPath cfg root
  -- Ensure log exists so poll/seek do not race a missing file.
  exists <- doesFileExist logFile
  when (not exists) $ appendFile logFile ""
  c <- Cur.newFile curP
  pos <- Cur.get c
  when (pos == 0) $ Cur.seekEndFile c logFile
  writeH <- openFile (fifoPath root) WriteMode
  hSetEncoding writeH utf8
  hSetBuffering writeH NoBuffering
  pure
    Channel
      { chCfg = cfg,
        chRoot = root,
        chWriteH = writeH,
        chCursor = c,
        chOwnsBus = False
      }

-- | Close a channel handle.
channelClose :: Channel -> IO ()
channelClose ch = do
  hClose (chWriteH ch)
  when (chOwnsBus ch) $ Bus.busStop (chRoot ch)

-- | Send a framed message to the channel.
channelSend :: Channel -> Text -> IO ()
channelSend ch body = do
  let name = chName (chCfg ch)
      chan = T.pack (chChannel (chCfg ch))
      payload = framePost (Post name [chan] [] body)
  TIO.hPutStrLn (chWriteH ch) payload
  hFlush (chWriteH ch)

-- | Receive all new messages since the last poll, restricted to this channel.
channelRecv :: Channel -> IO [(Text, Text)]
channelRecv ch = mapMaybe parseMessage <$> channelRecvRaw ch

-- | Receive all new raw log lines since the last poll, filtered to this
-- channel.
channelRecvRaw :: Channel -> IO [Text]
channelRecvRaw ch =
  filter (matchesChannel (T.pack (chChannel (chCfg ch))))
    <$> Cur.pollFile (chCursor ch) (logPath (chRoot ch))

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
