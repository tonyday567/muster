{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | High-level muster channel handle.
--
-- A thin wrapper around 'Circuit.Comm' that adds the native muster bus daemon
-- lifecycle and muster-specific path/layout conventions. All FIFO, cursor,
-- framing, and send/recv logic is delegated to 'Circuit.Comm'.
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

import Circuit.Comm qualified as Comm
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Muster.Bus qualified as Bus
import System.Environment (getEnv)
import System.FilePath ((</>))
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
-- Wraps a 'Circuit.Comm.Channel' plus ownership of the native muster bus
-- daemon. Keeping the write-end open prevents the bus daemon from seeing EOF
-- between messages.
data Channel = Channel
  { chCfg :: ChannelConfig,
    chComm :: Comm.Channel,
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

-- | Convert a muster channel config to a 'Circuit.Comm.ChannelConfig'.
toCommConfig :: ChannelConfig -> IO Comm.ChannelConfig
toCommConfig cfg = do
  dir <- channelDir cfg
  pure
    Comm.ChannelConfig
      { Comm.chStdinPath = dir </> "bus.fifo",
        Comm.chStdoutPath = dir </> "log.md",
        Comm.chStderrPath = dir </> "err.md",
        Comm.chCursorPath = Just (dir </> ".cursor-" <> T.unpack (chName cfg)),
        Comm.chName = chName cfg,
        Comm.chWorkingDir = dir
      }

-- | Open a channel, starting the bus if necessary.
--
-- The returned handle owns the bus daemon. Use 'channelClose' to tear down.
channelOpen :: ChannelConfig -> IO Channel
channelOpen cfg = do
  dir <- channelDir cfg
  Bus.busStart dir (chChannel cfg) (chBusRoot cfg)
  commCfg <- toCommConfig cfg
  ch <- Comm.channelAttach commCfg
  pure $ Channel cfg ch True

-- | Attach to an existing channel without starting the bus.
channelAttach :: ChannelConfig -> IO Channel
channelAttach cfg = do
  commCfg <- toCommConfig cfg
  ch <- Comm.channelAttach commCfg
  pure $ Channel cfg ch False

-- | Close a channel handle.
channelClose :: Channel -> IO ()
channelClose ch = do
  Comm.channelClose (chComm ch)
  when (chOwnsBus ch) $ do
    dir <- channelDir (chCfg ch)
    Bus.busStop dir

-- | Send a framed message to the channel.
channelSend :: Channel -> Text -> IO ()
channelSend ch = Comm.channelSend (chComm ch)

-- | Receive all new messages since the last poll.
channelRecv :: Channel -> IO [(Text, Text)]
channelRecv ch = Comm.channelRecv (chComm ch)

-- | Receive all new raw log lines since the last poll.
channelRecvRaw :: Channel -> IO [Text]
channelRecvRaw ch = Comm.channelRecvRaw (chComm ch)

-- | Block until new messages arrive, or the timeout fires.
channelRecvBlocking :: Channel -> Int -> IO (Maybe [(Text, Text)])
channelRecvBlocking ch = Comm.channelRecvBlocking (chComm ch)
