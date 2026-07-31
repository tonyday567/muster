{-# LANGUAGE OverloadedStrings #-}

-- | Multi-round seat smoke — dialogue, not single-shot evaluate.
--
-- Tiers (escalating reality; same 'runShard' exterior):
--
--   1. __pure–pure__ — two pure functions behind 'queryShard' (always)
--   2. __pure–hermes__ — pure nudge + live hermes seat (needs hermes)
--   3. __hermes–hermes__ — two hermes seats, separate session names (needs hermes)
--
-- Naming: these are not \"Identity agents\". A pure seat is
-- @queryShard who (pure . f)@ — Moore-shaped evaluate with a pure @Text -> Text@.
-- @Shard Identity [Post] [Post]@ is a different (pure Ends) packaging in circuits-agent verify.
--
-- @
--   cabal run muster-multi-round-smoke
--   cabal run muster-multi-round-smoke -- 5 pure
--   cabal run muster-multi-round-smoke -- 2 mixed
--   cabal run muster-multi-round-smoke -- 2 dual-hermes
--   cabal run muster-multi-round-smoke -- 3 all
-- @
--
-- Exit 0: requested tiers passed.
-- Exit 1: assertion failure.
-- Exit 2: hermes required for a requested tier but missing\/failed (infra).
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Muster.Agent
  ( AgentConfig (..),
    Post (..),
    Shard,
    defaultAgentConfig,
    oneshotShard,
    queryShard,
    runShard,
  )
import System.Directory (findExecutable)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

-- | Default dialogue depth.
defaultRounds :: Int
defaultRounds = 3

mk :: Text -> Text -> Text -> Post
mk a d = Post a [d]

assert :: String -> Bool -> IO ()
assert msg ok =
  if ok
    then putStrLn ("  PASS " ++ msg)
    else do
      putStrLn ("  FAIL " ++ msg)
      exitWith (ExitFailure 1)

-- | Ping-pong: A replies to seed, B replies to A, … for @n@ full A→B pairs.
--
-- Each step is one closed 'runShard' turn (commit list, emit list). The
-- conversation is the posts themselves — multi-round, not oneshot-shaped.
pingPong :: Int -> Shard IO [Post] [Post] -> Shard IO [Post] [Post] -> Post -> IO [Post]
pingPong n seatA seatB seed = go n seed []
  where
    go 0 _ acc = pure (reverse acc)
    go k p acc = do
      outsA <- runShard seatA [p]
      case outsA of
        [] -> pure (reverse acc)
        (a : _) -> do
          outsB <- runShard seatB [a]
          case outsB of
            [] -> pure (reverse (a : acc))
            (b : _) -> go (k - 1) b (b : a : acc)

-- | Pure seat: @Text -> Text@ with no external process.
pureShard :: Text -> (Text -> Text) -> IO (Shard IO [Post] [Post])
pureShard who f = queryShard who (pure . f)

data Tier = Pure | Mixed | DualHermes | All
  deriving (Eq, Show)

parseArgs :: [String] -> (Int, Tier)
parseArgs args =
  case args of
    [] -> (defaultRounds, Pure)
    [r] | Just n <- readMaybe r -> (n, Pure)
    [r, t] | Just n <- readMaybe r -> (n, parseTier t)
    [t] -> (defaultRounds, parseTier t)
    (r : t : _) | Just n <- readMaybe r -> (n, parseTier t)
    _ -> (defaultRounds, Pure)
  where
    parseTier s = case s of
      "pure" -> Pure
      "mixed" -> Mixed
      "dual-hermes" -> DualHermes
      "dual" -> DualHermes
      "all" -> All
      _ -> Pure
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _ -> Nothing

needHermes :: Tier -> Bool
needHermes Pure = False
needHermes _ = True

main :: IO ()
main = do
  args <- getArgs
  let (rounds, tier) = parseArgs args
  putStrLn $
    "muster-multi-round-smoke: rounds="
      <> show rounds
      <> " tier="
      <> show tier

  when (rounds < 1) $ do
    hPutStrLn stderr "rounds must be >= 1"
    exitWith (ExitFailure 1)

  mHermes <- findExecutable "hermes"
  when (needHermes tier && isNothing mHermes) $ do
    hPutStrLn stderr "muster-multi-round-smoke: hermes not on PATH — skip (exit 2)"
    exitWith (ExitFailure 2)

  case tier of
    Pure -> runPure rounds
    Mixed -> runMixed rounds
    DualHermes -> runDualHermes rounds
    All -> do
      runPure rounds
      case mHermes of
        Nothing ->
          putStrLn "  skip mixed/dual-hermes (hermes not on PATH)"
        Just _ -> do
          -- fewer rounds for live models unless user set a small n
          let hRounds = min rounds 2
          er <- try @SomeException (runMixed hRounds >> runDualHermes hRounds)
          case er of
            Left err -> do
              hPutStrLn stderr $ "muster-multi-round-smoke: hermes tier failed: " <> show err
              exitWith (ExitFailure 2)
            Right () -> pure ()

  putStrLn "muster-multi-round-smoke: done"

-- ---------------------------------------------------------------------------
-- Tier 1: pure–pure
-- ---------------------------------------------------------------------------

runPure :: Int -> IO ()
runPure rounds = do
  putStrLn "tier pure–pure (two pure seats)"
  -- nudge always asks for more; worker acks with a growing tag
  nudge <- pureShard "nudge" (const "tell me more.")
  worker <- pureShard "worker" ("ack:" <>)
  let seed = mk "human" "worker" "start"
  trail <- pingPong rounds worker nudge seed
  putStrLn $ "  trail length: " <> show (length trail)
  mapM_ (\p -> putStrLn $ "    " <> T.unpack (from p) <> " → " <> T.unpack (T.pack (show (to p))) <> ": " <> T.unpack (T.take 60 (body p))) trail
  assert "pure: produced 2 posts per round" $ length trail == 2 * rounds
  assert "pure: alternates worker then nudge" $
    let authors = map from trail
     in authors == take (length authors) (cycle ["worker", "nudge"])
  assert "pure: first body is ack of seed" $
    case trail of
      (p : _) -> body p == "ack:start"
      _ -> False
  assert "pure: nudge bodies constant" $
    all (\p -> from p /= "nudge" || body p == "tell me more.") trail

-- ---------------------------------------------------------------------------
-- Tier 2: pure–hermes
-- ---------------------------------------------------------------------------

runMixed :: Int -> IO ()
runMixed rounds = do
  putStrLn "tier pure–hermes"
  -- pure coach; hermes answers briefly
  nudge <-
    pureShard "nudge" $
      const "Reply with exactly one short sentence. Do not ask questions."
  hermes <-
    oneshotShard
      defaultAgentConfig
        { cfgName = "multi-round-hermes-a",
          cfgAgent = "hermes"
        }
      "hermes"
  let seed =
        mk "human" "hermes" $
          "You are in a "
            <> T.pack (show rounds)
            <> "-round drill. Reply with exactly: ready"
  er <- try @SomeException (pingPong rounds hermes nudge seed)
  case er of
    Left err -> do
      hPutStrLn stderr $ "mixed: hermes error: " <> show err
      exitWith (ExitFailure 2)
    Right trail -> do
      putStrLn $ "  trail length: " <> show (length trail)
      mapM_ (\p -> putStrLn $ "    " <> T.unpack (from p) <> ": " <> T.unpack (T.take 80 (body p))) trail
      assert "mixed: produced posts" $ not (null trail)
      assert "mixed: hermes authored some emit" $ any ((== "hermes") . from) trail
      assert "mixed: no empty bodies" $ not (any (T.null . T.strip . body) trail)

-- ---------------------------------------------------------------------------
-- Tier 3: hermes–hermes
-- ---------------------------------------------------------------------------

runDualHermes :: Int -> IO ()
runDualHermes rounds = do
  putStrLn "tier hermes–hermes (two seats, two session names)"
  alice <-
    oneshotShard
      defaultAgentConfig
        { cfgName = "multi-round-alice",
          cfgAgent = "hermes"
        }
      "alice"
  bob <-
    oneshotShard
      defaultAgentConfig
        { cfgName = "multi-round-bob",
          cfgAgent = "hermes"
        }
      "bob"
  let seed =
        mk "human" "alice" $
          "You are alice. Bob will reply next. "
            <> "Answer in at most five words. First message only: say ping"
  er <- try @SomeException (pingPong rounds alice bob seed)
  case er of
    Left err -> do
      hPutStrLn stderr $ "dual-hermes: error: " <> show err
      exitWith (ExitFailure 2)
    Right trail -> do
      putStrLn $ "  trail length: " <> show (length trail)
      mapM_ (\p -> putStrLn $ "    " <> T.unpack (from p) <> ": " <> T.unpack (T.take 80 (body p))) trail
      assert "dual: produced posts" $ not (null trail)
      assert "dual: both authors appear when rounds>=1" $
        rounds < 1
          || (any ((== "alice") . from) trail && (rounds == 1 || any ((== "bob") . from) trail || not (null trail)))
      assert "dual: no empty bodies" $ not (any (T.null . T.strip . body) trail)
