{-# LANGUAGE OverloadedStrings #-}

-- | Bespoke live hermes smoke for the oneshot Shard seat.
--
-- Manual only — not part of observe pipelines, CI, or muster-seat-verify.
-- Needs @hermes@ on PATH, network, and provider credentials.
--
-- @
--   cabal run muster-hermes-seat-smoke
--   # or with model/provider env already set for hermes
-- @
--
-- Exit 0: seat returned a non-empty reply body.
-- Exit 2: hermes missing or query failed (infrastructure, not a model bug).
module Main (main) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Muster.Agent
  ( AgentConfig (..),
    Post (..),
    defaultAgentConfig,
    oneshotShard,
    runShard,
  )
import System.Directory (findExecutable)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

mk :: Text -> Text -> Text -> Post
mk a d = Post a [d]

main :: IO ()
main = do
  args <- getArgs
  mHermes <- findExecutable "hermes"
  case mHermes of
    Nothing -> do
      hPutStrLn stderr "muster-hermes-seat-smoke: hermes not on PATH — skip (exit 2)"
      exitWith (ExitFailure 2)
    Just _ -> pure ()

  let who = "smoke"
      prompt =
        case args of
          (p : _) -> T.pack p
          _ -> "Reply with exactly one word: pong"
      cfg =
        defaultAgentConfig
          { cfgName = "hermes-seat-smoke",
            cfgAgent = "hermes"
          }
  seat <- oneshotShard cfg who
  let pIn = mk "human" who prompt
  er <- try @SomeException (runShard seat [pIn])
  case er of
    Left err -> do
      hPutStrLn stderr $ "muster-hermes-seat-smoke: query failed: " <> show err
      exitWith (ExitFailure 2)
    Right outs -> do
      putStrLn $ "commit: " <> T.unpack prompt
      putStrLn $ "emit count: " <> show (length outs)
      mapM_ (\o -> putStrLn $ "  body: " <> T.unpack (T.take 200 (body o))) outs
      if null outs || all (T.null . T.strip . body) outs
        then do
          hPutStrLn stderr "muster-hermes-seat-smoke: quiet emit (unexpected)"
          exitWith (ExitFailure 1)
        else do
          putStrLn "muster-hermes-seat-smoke: PASS (non-empty seat reply)"
          -- optional model hint from env (diagnostic only)
          mModel <- lookupEnv "HERMES_MODEL"
          mProv <- lookupEnv "HERMES_PROVIDER"
          putStrLn $ "  model/provider env: " <> show mModel <> " / " <> show mProv
