{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Panel topology smoke test.
--
-- Demonstrates the "5a × 3 +" meeting pattern in miniature:
--   * spawn N agents with different prompts (panel)
--   * run R rounds of turn (each round every agent sees the previous round's posts)
--   * collect / synthesize the final responses
--
-- This is a topology over the shared log: visibility is which posts each agent
-- is committed, and the public bus records every step.
module Main (main) where

import Control.Monad (foldM, forM, forM_, unless, void)
import Data.Text (Text)
import Data.Text qualified as T
import Muster.Agent
  ( Post (..),
    Shard,
    queryShard,
    runShard,
  )
import Muster.Api.Bus (ensureChannel, post, readTail, withBus)
import Muster.Api.Types (BusRoot (..), Channel (..), Nick (..))
import Muster.Framing (parseMessage)
import System.Directory (removePathForcibly)
import System.Exit (exitFailure)
import Prelude

assert :: String -> Bool -> IO ()
assert msg ok =
  unless ok $ do
    putStrLn $ "  FAIL " <> msg
    exitFailure

mkPost :: Text -> Text -> Text -> Text -> Post
mkPost a d c b = Post a d c b

pureShard :: Text -> (Text -> Text) -> IO (Shard IO [Post])
pureShard who f = queryShard who (pure . f)

-- | Run one round: every agent in the panel sees the same input stream and
-- emits replies.  Returns the concatenated replies (the public record of the
-- round).
runRound :: [Shard IO [Post]] -> [Post] -> IO [Post]
runRound panel inputs = do
  roundOuts <- forM panel (\seat -> runShard seat inputs)
  pure (concat roundOuts)

main :: IO ()
main = do
  putStrLn "muster-panel-smoke: panel × rounds + synthesis"

  let root = BusRoot "/tmp/muster-panel-smoke"
      panelChan = Channel "panel"
      human = Nick "human"
      synth = Nick "synth"
      nAgents = 3
      nRounds = 2

  removePathForcibly (unBusRoot root)
  withBus root $ \_ -> do
    void $ ensureChannel root panelChan

    -- Seed: the single problem every expert sees in round 1.
    let seed = mkPost "human" "panel" "panel" "Q: what should we do?"
    post root panelChan human (body seed)

    -- Panel: n agents with distinct response functions.
    panel <- forM [1 .. nAgents] $ \i -> do
      let name = "agent-" <> T.pack (show (i :: Int))
      pureShard name (\_ -> name <> " notes: factor " <> T.singleton (['A' ..] !! (i - 1)))

    assert "panel has expected size" $ length panel == nAgents

    -- Run rounds: round 1 sees the seed; later rounds see the previous round's posts.
    -- Track every post emitted so we can post the public record and verify totals.
    (allRoundPosts, finalRoundPosts) <-
      foldM
        ( \(acc, _inputs) _round -> do
            outs <- runRound panel _inputs
            pure (acc <> outs, outs)
        )
        ([], [seed])
        (replicate nRounds ())

    assert "each agent emitted once per round"
      $ length allRoundPosts == nAgents * nRounds

    -- Post every round's replies to the public bus.
    forM_ allRoundPosts $ \o -> post root panelChan (Nick (author o)) (body o)

    -- Synthesizer sees all final-round replies.
    synthSeat <-
      queryShard
        "synth"
        (\prompt -> pure ("synthesis: " <> T.replace "\n" ", " prompt))
    sOuts <- runShard synthSeat finalRoundPosts

    assert "synth emits one synthesis" $ length sOuts == 1

    -- Post synthesis to the bus.
    forM_ sOuts $ \o -> post root panelChan (Nick (author o)) (body o)

    -- Verify the public log contains the synthesis.
    final <- readTail root panelChan synth 10
    assert "synthesis is on the bus" $ not (null final)
    let synthesis =
          case parseMessage (last final) of
            Just (_, b) -> b
            Nothing -> last final

    forM_ [1 .. nAgents] $ \i ->
      let name = "agent-" <> T.pack (show (i :: Int))
       in assert (T.unpack name <> " referenced in synthesis") (name `T.isInfixOf` synthesis)

    putStrLn $ "  synthesis: " <> T.unpack (T.take 120 synthesis)
    putStrLn "muster-panel-smoke: PASS"
