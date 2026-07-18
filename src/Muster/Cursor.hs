{-# LANGUAGE OverloadedStrings #-}

-- | Cursor over an append-only log.
--
-- Thin wrapper around the @Cursor@ package so muster consumers do not depend
-- on it directly. A cursor tracks a read position in a file; polling returns
-- only new complete lines since the last poll.
module Muster.Cursor
  ( Cursor,
    newFile,
    newMem,
    set,
    seekEnd,
    pollFile,
    pollLines,
  )
where

import Cursor qualified as Cur
import Data.Text (Text)
import Prelude

-- | File-backed cursor.
type Cursor = Cur.Cursor

-- | Create a cursor backed by @path@. The file stores the current offset.
newFile :: FilePath -> IO Cursor
newFile = Cur.newFile

-- | Create an in-memory cursor at position @n@.
newMem :: Int -> IO Cursor
newMem = Cur.newMem

-- | Set cursor position.
set :: Cursor -> Int -> IO ()
set = Cur.set

-- | Seek to the end of the given complete content.
seekEnd :: Cursor -> [Text] -> IO ()
seekEnd = Cur.seekEnd

-- | Poll a log file for new complete lines since the cursor position.
pollFile :: Cursor -> FilePath -> IO [Text]
pollFile = Cur.pollFile

-- | Poll already-read content for new complete lines.
pollLines :: Cursor -> [Text] -> IO [Text]
pollLines = Cur.pollLines
