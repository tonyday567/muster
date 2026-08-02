{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Product seats for the muster-agent pipeline.
--
-- Each seat is a 'Circuit.Agent.Shard' over 'Post' lists. They compose with
-- '>:>' so that 'muster-agent' is a thin wrapper: wake → pipeline → bus + bucket.
--
-- The seats are intentionally product-specific (muster addressing, bus layout,
-- bucket path). They do not grow a parallel ontology; they instantiate the
-- 'Shard' shape from 'circuits-agent'.
module Muster.Agent.Seat
  ( -- * Meta actions
    MetaAction (..),
    parseMetaAction,

    -- * Seats
    filterShard,
    metaShard,
    busSink,
    bucketShard,
    diagShard,
  )
where

import Circuit.Agent (Post (..), Shard, shard)
import Control.Concurrent.STM (TChan, TQueue, atomically, writeTChan, writeTQueue)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void)
import Data.Either (partitionEithers)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muster.Agent (addressedTo, stripAddress)
import Prelude

-- ---------------------------------------------------------------------------
-- Meta actions
-- ---------------------------------------------------------------------------

-- | Side effects that the main loop must perform on behalf of the meta seat.
--
-- Channel-map mutation lives outside the pipeline so the seat ontology stays
-- pure-ish; the seat only emits the request.
data MetaAction
  = JoinChannel String
  | LeaveChannel String
  deriving (Eq, Show)

-- | Parse a stripped body for join/leave commands.
--
-- Matches @join #chan@ or @leave #chan@ (hash optional).
parseMetaAction :: Text -> Maybe MetaAction
parseMetaAction t =
  let low = T.toLower $ T.strip t
      stripHash c = fromMaybe c (T.stripPrefix "#" c)
   in case T.words low of
        ["join", c] -> Just (JoinChannel (T.unpack (stripHash c)))
        ["leave", c] -> Just (LeaveChannel (T.unpack (stripHash c)))
        _ -> Nothing

-- ---------------------------------------------------------------------------
-- Seats
-- ---------------------------------------------------------------------------

-- | Keep only posts addressed to @who@, stripping the addressing prefix.
--
-- Self-posts and non-addressed posts become quiet (empty emit).
filterShard :: Text -> IO (Shard IO [Post Text] [Post Text])
filterShard who = do
  ref <- newIORef []
  pure $
    shard
      (writeIORef ref . mapMaybe keep)
      (readIORef ref <* writeIORef ref [])
  where
    keep p
      | from p == who = Nothing
      | addressedTo who (body p) =
          Just p {body = stripAddress who (body p)}
      | otherwise = Nothing

-- | Intercept join/leave meta commands and pass everything else through.
--
-- Meta actions are written to the supplied 'TChan' and logged to the diag
-- queue; they do not propagate downstream.
metaShard :: TChan MetaAction -> TQueue Text -> IO (Shard IO [Post Text] [Post Text])
metaShard acts diag = do
  ref <- newIORef []
  pure $
    shard
      (writeIORef ref)
      ( do
          ins <- readIORef ref
          writeIORef ref []
          let (metas, rest) = partitionEithers (map classify ins)
          forM_ metas $ \a -> do
            atomically (writeTChan acts a)
            atomically (writeTQueue diag (metaDiag a))
          pure rest
      )
  where
    classify p = case parseMetaAction (body p) of
      Just a -> Left a
      Nothing -> Right p
    metaDiag = \case
      JoinChannel c -> "meta: join #" <> T.pack c
      LeaveChannel c -> "meta: leave #" <> T.pack c

-- | Send emitted post bodies to per-channel sinks.
--
-- The map key is a channel / wire name; the value is the send action. A post
-- is sent to every sink whose name appears in its 'to' list. Posts are passed
-- through so downstream seats (bucket, diag) can still see them.
busSink :: Map Text (Text -> IO ()) -> IO (Shard IO [Post Text] [Post Text])
busSink sinks = do
  ref <- newIORef []
  pure $
    shard
      (writeIORef ref)
      ( do
          outs <- readIORef ref
          writeIORef ref []
          forM_ outs $ \o ->
            forM_ (to o) $ \name ->
              case Map.lookup name sinks of
                Just send -> void $ try @SomeException (send (body o))
                Nothing -> pure ()
          pure outs
      )

-- | Append emitted post bodies to a file.
--
-- Passes posts through so a downstream 'diagShard' can log them.
bucketShard :: FilePath -> IO (Shard IO [Post Text] [Post Text])
bucketShard outPath = do
  ref <- newIORef []
  pure $
    shard
      (writeIORef ref)
      ( do
          outs <- readIORef ref
          writeIORef ref []
          forM_ outs $ \o -> TIO.appendFile outPath (body o <> "\n")
          pure outs
      )

-- | Terminal seat: log every emitted post to the diag queue.
diagShard :: TQueue Text -> IO (Shard IO [Post Text] [Post Text])
diagShard diag = do
  ref <- newIORef []
  pure $
    shard
      (writeIORef ref)
      ( do
          outs <- readIORef ref
          writeIORef ref []
          forM_ outs $ \o ->
            atomically $
              writeTQueue diag $
                "emit on " <> T.pack (show (to o)) <> ": " <> T.take 100 (body o)
          pure []
      )
