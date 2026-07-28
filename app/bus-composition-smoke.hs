{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bus-as-topology smoke test.
--
-- Demonstrates that any meeting topology can be defunctionalized onto the
-- shared log: visibility = which posts you commit to each agent; the public
-- bus records everything.
--
-- Topology here: one seed → three experts (each sees only the seed) → one
-- synthesizer (sees all three expert replies) → final synthesis posted back.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, void)
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

main :: IO ()
main = do
  putStrLn "muster-bus-composition-smoke: topology via the log"

  let root = BusRoot "/tmp/muster-bus-composition-smoke"
      panel = Channel "panel"
      human = Nick "human"
      synth = Nick "synth"

  removePathForcibly (unBusRoot root)
  withBus root $ \_ -> do
    void $ ensureChannel root panel

    -- Seed: the single prompt every expert sees.
    let seed = mkPost "human" "panel" "panel" "Q: what should we do?"
    post root panel human (body seed)
    threadDelay 100_000

    -- Three independent experts, each fed only the seed.
    alphaSeat <- pureShard "alpha" (\_ -> "alpha notes: factor A")
    betaSeat <- pureShard "beta" (\_ -> "beta notes: factor B")
    gammaSeat <- pureShard "gamma" (\_ -> "gamma notes: factor C")

    aOuts <- runShard alphaSeat [seed]
    bOuts <- runShard betaSeat [seed]
    gOuts <- runShard gammaSeat [seed]

    assert "alpha emits one reply" $ length aOuts == 1
    assert "beta emits one reply" $ length bOuts == 1
    assert "gamma emits one reply" $ length gOuts == 1

    -- Public record: every expert reply is posted to the bus.
    forM_ (aOuts <> bOuts <> gOuts) $ \o ->
      post root panel (Nick (author o)) (body o)
    threadDelay 100_000

    -- Synthesizer sees the three replies, not the seed.
    let replies = aOuts <> bOuts <> gOuts
    synthSeat <-
      queryShard
        "synth"
        (\prompt -> pure ("synthesis: " <> T.replace "\n" ", " prompt))
    sOuts <- runShard synthSeat replies

    assert "synth emits one synthesis" $ length sOuts == 1

    -- Post synthesis to the bus.
    forM_ sOuts $ \o -> post root panel (Nick (author o)) (body o)
    threadDelay 100_000

    -- Verify the public log contains the synthesis.
    final <- readTail root panel synth 10
    assert "synthesis is on the bus" $ not (null final)
    let synthesis =
          case parseMessage (last final) of
            Just (_, body) -> body
            Nothing -> last final
    assert "synthesis references alpha" $ "alpha" `T.isInfixOf` synthesis
    assert "synthesis references beta" $ "beta" `T.isInfixOf` synthesis
    assert "synthesis references gamma" $ "gamma" `T.isInfixOf` synthesis

    putStrLn $ "  synthesis: " <> T.unpack (T.take 120 synthesis)
    putStrLn "muster-bus-composition-smoke: PASS"
