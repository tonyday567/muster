{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Smoke test for the composed muster-agent seat pipeline.
--
-- No live bus daemon, no hermes. We mock the bus with 'IORef' sinks and
-- exercise filter → meta → main (echo) → bus → bucket → diag.
module Main (main) where

import Circuit.Agent ((>:>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
  ( TChan,
    TQueue,
    atomically,
    newTChanIO,
    newTQueueIO,
    readTQueue,
    tryReadTChan,
  )
import Control.Monad (forever, unless, void)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muster.Agent (Post (..), Shard, echoShard, runShard)
import Muster.Agent.Seat
  ( MetaAction (..),
    bucketShard,
    busSink,
    diagShard,
    filterShard,
    metaShard,
  )
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Prelude

assert :: String -> Bool -> IO ()
assert msg ok =
  unless ok $ do
    putStrLn $ "  FAIL " <> msg
    exitFailure

mkPost :: Text -> Text -> Text -> Text -> Post
mkPost a d c b = Post a d c b

main :: IO ()
main = do
  putStrLn "muster-agent-pipeline-smoke: composed seats"

  -- Mock bus sinks: channel name -> action that records the sent body.
  busLog <- newIORef ([] :: [Text])
  testLog <- newIORef ([] :: [Text])
  let sinks :: Map Text (Text -> IO ())
      sinks =
        Map.fromList
          [ ("bus", \t -> modifyIORef' busLog (<> [t])),
            ("test", \t -> modifyIORef' testLog (<> [t]))
          ]

  -- Bucket file.
  let bucketDir = "/tmp/muster-agent-pipeline-smoke"
      bucketPath = bucketDir </> "output.md"
  removePathForcibly bucketDir
  createDirectoryIfMissing True bucketDir

  -- Diagnostic sink: collect to a list so we can check it.
  diagQ <- newTQueueIO
  diags <- newIORef ([] :: [Text])
  void $ async $ forever $ do
    d <- atomically $ readTQueue diagQ
    modifyIORef' diags (<> [d])

  -- Meta action channel.
  metaChan <- newTChanIO

  -- Main seat: echo.
  mainSeat <- echoShard "echo"

  -- Build initial pipeline.
  pipeline <- buildPipeline mainSeat sinks bucketPath diagQ metaChan

  -- 1. Addressed post from human on #bus.
  putStrLn "addressed post produces reply"
  _ <- runShard pipeline [mkPost "human" "echo" "bus" "@echo hello"]
  bus1 <- readIORef busLog
  assert "reply body is stripped prompt" $ bus1 == ["hello"]

  -- 2. Self-post is quiet.
  putStrLn "self post is quiet"
  _ <- runShard pipeline [mkPost "echo" "echo" "bus" "@echo hi"]
  bus2 <- readIORef busLog
  assert "no reply to self post" $ bus2 == ["hello"]

  -- 3. Non-addressed post is quiet.
  putStrLn "non-addressed post is quiet"
  _ <- runShard pipeline [mkPost "human" "echo" "bus" "just chatting"]
  bus3 <- readIORef busLog
  assert "no reply to chatter" $ bus3 == ["hello"]

  -- 4. Meta join command produces a MetaAction.
  putStrLn "meta join emits action"
  _ <- runShard pipeline [mkPost "human" "echo" "bus" "@echo join #test"]
  mAct <- atomically $ tryReadTChan metaChan
  assert "meta channel received JoinChannel test" $ mAct == Just (JoinChannel "test")

  -- 5. Simulate main-loop channel refresh (test channel already in sinks)
  --    and post on the newly joined channel.
  putStrLn "post on refreshed channel produces reply"
  pipeline' <- buildPipeline mainSeat sinks bucketPath diagQ metaChan
  _ <- runShard pipeline' [mkPost "human" "echo" "test" "@echo ping"]
  test1 <- readIORef testLog
  assert "reply on #test" $ test1 == ["ping"]

  -- 6. Bucket contains both replies.
  putStrLn "bucket accumulates replies"
  bucketContent <- TIO.readFile bucketPath
  assert "bucket has hello and ping" $
    T.lines bucketContent == ["hello", "ping"]

  -- Give the diag collector a moment to drain.
  threadDelay 50_000
  ds <- readIORef diags
  assert "diag logged emitted replies" $ length ds >= 2

  putStrLn "muster-agent-pipeline-smoke: PASS"

-- | Assemble the pipeline for a given bus-sink map.
buildPipeline ::
  Shard IO [Post] ->
  Map Text (Text -> IO ()) ->
  FilePath ->
  TQueue Text ->
  TChan MetaAction ->
  IO (Shard IO [Post])
buildPipeline mainSeat sinks outPath diagQ metaChan = do
  f <- filterShard "echo"
  m <- metaShard metaChan diagQ
  b <- busSink sinks
  buck <- bucketShard outPath
  d <- diagShard diagQ
  pure (f >:> m >:> mainSeat >:> b >:> buck >:> d)
