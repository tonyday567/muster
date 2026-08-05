{-# LANGUAGE OverloadedStrings #-}

-- | Mock-echo demonstration of the living-agent Shard seat (no hermes).
--
-- Pure seat + live-bus dogfood:
--
-- @
--   cabal run muster-seat-verify
-- @
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, void)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Muster.Agent
  ( Post (..),
    echoShard,
    replyPosts,
    runShard,
    sessionPrompt,
  )
import Muster.Api.Bus (ensureChannel, post, readNext, withBus)
import Muster.Api.Types (BusRoot (..), Channel (..), Nick (..), unBusRoot, unChannel)
import Circuit.Agent.Framing (parseMessage)
import System.Directory (removePathForcibly)
import System.Exit (exitFailure)

assert :: String -> Bool -> IO ()
assert msg ok =
  if ok
    then putStrLn ("  PASS " ++ msg)
    else do
      putStrLn ("  FAIL " ++ msg)
      exitFailure

mk :: Text -> Text -> Text -> Post Text
mk a d = Post a [d] []

-- | Same decode as circuits-agent-observe: channel is the address.
decodePost :: Channel -> Text -> Maybe (Post Text)
decodePost chan line = do
  (author', body') <- parseMessage line
  pure
    Post
      { from = author',
        to = [unChannel chan],
        thread = [],
        body = body'
      }

main :: IO ()
main = do
  putStrLn "muster-seat-verify: living agent seat (mock echo)"

  putStrLn "session / reply helpers"
  do
    let p1 = mk "human" "echo" "hi"
        p2 = mk "human" "echo" "there"
    assert "sessionPrompt joins bodies" $
      sessionPrompt [p1, p2] == "hi\nthere"
    assert "replyPosts addresses last author and preserves wire" $
      case replyPosts "echo" [p1] [] "ack" of
        [o] ->
          from o == "echo"
            && to o == ["human"]
            && body o == "ack"
        _ -> False
    assert "replyPosts quiet on empty reply" $
      null (replyPosts "echo" [p1] [] "  ")
    assert "replyPosts quiet on empty session" $
      null (replyPosts "echo" [] [] "x")

  putStrLn "echo Shard IO [Post] [Post]"
  do
    seat <- echoShard "echo"
    let pIn = mk "human" "echo" "ping"
    outs <- runShard seat [pIn]
    assert "one reply post" $ length outs == 1
    assert "body is session prompt (echo)" $ map body outs == ["ping"]
    assert "from is seat nick" $ all ((== "echo") . from) outs
    assert "to is [human]" $ all ((== ["human"]) . to) outs

    outs2 <- runShard seat [mk "k" "echo" "hi\nline2"]
    assert "multi-line session echoes" $
      map body outs2 == ["hi\nline2"]

    quiet <- runShard seat []
    assert "empty commit is quiet" $ null quiet

  putStrLn "live bus dogfood (echo seat, no hermes)"
  do
    let root = BusRoot "/tmp/muster-seat-verify-bus"
        echoChan = Channel "echo"
        humanChan = Channel "human"
        human = Nick "human"
        echo = Nick "echo"
    removePathForcibly (unBusRoot root)
    withBus root $ \_ -> do
      void $ ensureChannel root echoChan
      void $ ensureChannel root humanChan
      -- give the central scan a tick to attach relays
      threadDelay 200_000

      -- human → echo channel
      post root echoChan human "ping"
      threadDelay 100_000

      raw <- readNext root echoChan echo
      let news = mapMaybe (decodePost echoChan) raw
      assert "echo agent watches human ping" $
        map body news == ["ping"]

      seat <- echoShard "echo"
      outs <- runShard seat news
      assert "seat emits echo reply" $ map body outs == ["ping"]

      -- seat emit → tape (post as echo to human's channel via Post.to)
      forM_ outs $ \o ->
        forM_ (listToMaybe (to o)) $ \name ->
          post root (Channel name) (Nick (from o)) (body o)
      threadDelay 100_000

      humanSeen <- readNext root humanChan human
      let humanPosts = mapMaybe (decodePost humanChan) humanSeen
      assert "human receives framed echo on bus" $
        map body humanPosts == ["ping"]

      again <- readNext root humanChan human
      assert "no redelivery to human" $ null again

  putStrLn "All seat oracles passed"
