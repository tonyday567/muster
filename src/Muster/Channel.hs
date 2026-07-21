{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | High-level muster channel handle.
--
-- A 'Channel' wraps a persistent FIFO write handle and a cursor over the
-- append-only log. Keeping the write handle open prevents the bus daemon from
-- seeing EOF between messages.
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
import Control.Exception (onException)
import Control.Monad (unless, when)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muster.Bus qualified as Bus
import Muster.Cursor qualified as Cur
import Muster.Framing (formatNow, frameMessage, parseMessage)
import System.Directory (doesFileExist)
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
data Channel = Channel
  { chCfg :: ChannelConfig,
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

-- | Open a channel, starting the bus if necessary.
--
-- The returned handle owns the bus daemon. Use 'channelClose' to tear down.
channelOpen :: ChannelConfig -> IO Channel
channelOpen cfg = do
  dir <- channelDir cfg
  Bus.busStart dir (chChannel cfg) (chBusRoot cfg)
  openHandles cfg dir True

-- | Attach to an existing channel without starting the bus.
channelAttach :: ChannelConfig -> IO Channel
channelAttach cfg = do
  dir <- channelDir cfg
  openHandles cfg dir False

openHandles :: ChannelConfig -> FilePath -> Bool -> IO Channel
openHandles cfg dir ownsBus = do
  let p = Bus.pathsFor dir
      logPath = Bus.busLog p
      fifoPath = Bus.busFifo p
      cursorFile = dir </> ".cursor-" <> T.unpack (chName cfg)
  fifoExists <- doesFileExist fifoPath
  unless fifoExists $
    fail $ "bus.fifo missing at " <> fifoPath <> " — run: muster bus start -c " <> chChannel cfg
  -- Use the persistent participant cursor when it exists so that external
  -- watchers such as muster-alert/muster-watch see the same read position.
  -- Otherwise start an in-memory cursor at the current end of the log.
  cursorExists <- doesFileExist cursorFile
  cursor <-
    if cursorExists
      then Cur.newFile cursorFile
      else do
        c <- Cur.newMem 0
        content <-
          doesFileExist logPath >>= \case
            True -> TIO.readFile logPath
            False -> pure ""
        Cur.seekEnd c (completeLines content)
        pure c
  writeH <-
    openFile fifoPath WriteMode
      `onException` unless ownsBus (pure ())
  hSetEncoding writeH utf8
  hSetBuffering writeH NoBuffering
  pure $ Channel cfg writeH cursor ownsBus
  where
    completeLines :: Text -> [Text]
    completeLines t
      | T.null t = []
      | T.isSuffixOf "\n" t = T.lines t
      | otherwise =
          let parts = T.splitOn "\n" t
           in if null parts then [] else init parts

-- | Close a channel handle.
channelClose :: Channel -> IO ()
channelClose ch = do
  hClose (chWriteH ch)
  when (chOwnsBus ch) $ do
    dir <- channelDir (chCfg ch)
    Bus.busStop dir

-- | Send a framed message to the channel.
channelSend :: Channel -> Text -> IO ()
channelSend ch body = do
  ts <- formatNow
  TIO.hPutStrLn (chWriteH ch) (frameMessage ts (chName (chCfg ch)) body)
  hFlush (chWriteH ch)

-- | Receive all new messages since the last poll.
--
-- Returns @(sender, body)@ pairs. Lines that do not parse as framed messages
-- are silently dropped. The optional timestamp is discarded — use the raw
-- log if you need it.
channelRecv :: Channel -> IO [(Text, Text)]
channelRecv ch = do
  dir <- channelDir (chCfg ch)
  let logPath = Bus.busLog (Bus.pathsFor dir)
  ls <- Cur.pollFile (chCursor ch) logPath
  pure $ mapMaybe (fmap dropTs . parseMessage) ls
  where
    dropTs (s, b, _) = (s, b)

-- | Receive all new raw log lines since the last poll.
--
-- Returns the complete framed lines (including timestamps). Use this when
-- you need the original log format rather than parsed sender/body pairs.
channelRecvRaw :: Channel -> IO [Text]
channelRecvRaw ch = do
  dir <- channelDir (chCfg ch)
  let logPath = Bus.busLog (Bus.pathsFor dir)
  Cur.pollFile (chCursor ch) logPath

-- | Block until new messages arrive, or the timeout fires.
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
