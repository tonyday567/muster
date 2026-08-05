{-# LANGUAGE OverloadedStrings #-}

-- | L4a smoke: an in-process meeting runner over the live bus.
--
-- Drives a deterministic roster of seats ('Free.Agent.Meeting.quoter'),
-- posts every message with thread ancestry via 'postWithThread', and proves
-- the live drawing is hyper-equivalent to the pure 'meetLog' of the same
-- roster. Also starts 'muster-ws' and checks that the deck endpoint renders
-- the meeting live.
--
-- Optional: install the local @kimi@ CLI (or set @KIMI_API_KEY@, optionally
-- with @KIMI_BASE_URL@ / @KIMI_MODEL@) to attach a real LLM seat to a second
-- channel and prove the deck draws it too.
--
-- curl(1) is used as the HTTP client to keep the executable's dependency
-- footprint small.
module Main (main) where

import Circuit.Agent (Post (..))
import Circuit.Poly.StringDiagram.Hyper (hyperEquiv)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Monad (forM_, unless)
import Data.ByteString qualified as BS
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Maybe (fromMaybe)
import Control.Exception (SomeException, try)
import Free.Agent.Diagram (meetingSkeleton)
import Free.Agent.Host (HostConfig (..), apiHost, hostRun, kimiHost)
import Free.Agent.Meeting (AgentBox (..), meetLog, quoter)
import Muster.Api.Bus
  ( ensureChannel,
    postWithThread,
    withBus,
  )
import Muster.Api.Types (BusRoot (..), Channel (..), Nick (..))
import Muster.Diagram (readMeetingPosts, renderSkeletonSvg)
import Muster.Ws (WsConfig (..), mkApp)
import Network.Wai.Handler.Warp (defaultSettings, openFreePort, runSettingsSocket)
import System.Directory (createDirectoryIfMissing, findExecutable, removePathForcibly)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Process (readProcess)
import Prelude

assert :: String -> Bool -> IO ()
assert msg ok =
  unless ok $ do
    putStrLn ("  FAIL " <> msg)
    exitFailure

pass :: String -> IO ()
pass msg = putStrLn ("  PASS " <> msg)

-- | GET via curl: @(status, content-type, body)@. No http-client dep.
httpGet :: Int -> String -> IO (Int, String, String)
httpGet port path = do
  out <- readProcess "curl" ["-sS", "-i", "http://127.0.0.1:" <> show port <> path] ""
  let (hdrs, body) = splitHeaders out
      statusLine = takeWhile (/= '\r') hdrs
      st = case words statusLine of
        (_ : code : _) -> reads code :: [(Int, String)]
        _ -> []
      ct =
        [ v
          | l <- lines hdrs,
            let (k, v0) = break (== ':') l,
            map toLowerAscii k == "content-type",
            let v = dropWhile (== ' ') (drop 1 v0)
        ]
  pure (case st of [(n, _)] -> n; _ -> -1, concat (take 1 ct), body)
  where
    toLowerAscii c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
    splitHeaders s = go "" s
      where
        go acc rest
          | "\r\n\r\n" `isPrefixOf` rest = (reverse acc, drop 4 rest)
          | otherwise = case rest of
              [] -> (reverse acc, "")
              (c : cs) -> go (c : acc) cs

-- | Post one reconstructed 'Post' to the bus, ancestry included.
postPost :: BusRoot -> Channel -> Post Text -> IO ()
postPost root chan p =
  postWithThread root chan (Nick (from p)) (thread p) (body p)

-- | Run a tiny real-agent demo on a fresh channel if a live agent is available.
--
-- Prefer the local @kimi@ CLI; fall back to the OpenAI-compatible API if
-- @KIMI_API_KEY@ is set; otherwise skip.  The deterministic structural oracle
-- has already passed; this just proves a live LLM seat can post with ancestry
-- and the deck draws it.
runLiveSeatDemo :: BusRoot -> Int -> IO ()
runLiveSeatDemo root port = do
  mKimi <- findExecutable "kimi"
  case mKimi of
    Just _ -> runKimiCli
    Nothing -> do
      mKey <- lookupEnv "KIMI_API_KEY"
      case mKey of
        Nothing -> putStrLn "  SKIP live seat demo (install kimi CLI or set KIMI_API_KEY to run it)"
        Just key -> runApi key
  where
    liveChan = Channel "live"
    seedBody = "What is one concise thought about composition?"

    setup = do
      _ <- ensureChannel root liveChan
      postWithThread root liveChan (Nick "human") [] seedBody

    finish rsp note = do
      postWithThread root liveChan (Nick "kimi") [0] rsp
      -- A deterministic synthesiser folds the room.
      postWithThread root liveChan (Nick "synth") [0, 1] "synth saw the seed and the reply."
      (st, ct, body) <- httpGet port "/api/diagram.svg?channel=live"
      assert ("live deck endpoint status 200, got " <> show st) $ st == 200
      assert "live deck content-type is svg" $ "image/svg+xml" `isPrefixOf` ct
      assert ("live deck SVG labels kimi" <> note) $ "kimi" `isInfixOf` body
      pass ("live seat demo: kimi posted" <> note)

    runKimiCli = do
      setup
      let sessionFile = unBusRoot root </> "kimi-session"
      mModelEnv <- lookupEnv "KIMI_MODEL"
      let model = T.pack <$> mModelEnv
      er <- try @SomeException (hostRun (kimiHost "kimi" model sessionFile) [seedBody])
      rsp <- case er of
        Left e -> pure ("🔴 " <> T.pack (show e))
        Right (r : _) -> pure r
        Right [] -> pure "🔴 empty host response"
      let note = if T.isPrefixOf "🔴" rsp then " (error response)" else " (kimi CLI)"
      finish rsp note

    runApi key = do
      setup
      base <- fromMaybe "https://api.openai.com/v1" <$> lookupEnv "KIMI_BASE_URL"
      apiModel <- fromMaybe "gpt-4o-mini" <$> lookupEnv "KIMI_MODEL"
      let cfg =
            HostConfig
              { agentName = "kimi",
                baseUrl = T.pack base,
                model = T.pack apiModel,
                key = T.pack key
              }
      rs <- hostRun (apiHost cfg "You are a terse assistant.") [seedBody]
      rsp <- case rs of (r : _) -> pure r; [] -> pure "🔴 empty host response"
      let note = if T.isPrefixOf "🔴" rsp then " (error response)" else " (API)"
      finish rsp note

main :: IO ()
main = do
  putStrLn "muster-meeting-runner: L4a live participation frame"
  let root = BusRoot "/tmp/muster-meeting-runner"
      chan = Channel "panel"
      chanDir = unBusRoot root
      seed = [Post "human" [] [] "seed question"]
      tagFor i = "agent-" <> T.pack (show (i :: Int))
      roster = [AgentBox ([], [], [], []) (quoter (tagFor i) ("tag-" <> tagFor i)) | i <- [1 .. 3 :: Int]]
      rounds = 2
      purePosts = meetLog rounds roster seed

  removePathForcibly (unBusRoot root)
  createDirectoryIfMissing True chanDir

  withBus root $ \_ -> do
    _ <- ensureChannel root chan
    -- Give the scan loop a beat to attach the relay before the first post.
    threadDelay 1_500_000

    -- Drive the live bus with the exact posts the pure semantics produced.
    forM_ purePosts $ \p -> do
      postPost root chan p
      -- Tiny gap so the relay fd and growth check don't race to a deadlock
      -- under stress; 50ms is far below the check timeout.
      threadDelay 50_000

    -- Start the deck server over the same bus.
    app <-
      mkApp
        WsConfig
          { wcName = "runner",
            wcChannel = "panel",
            wcBusRoot = unBusRoot root,
            wcHistory = 20,
            wcBoard = chanDir </> "board.md",
            wcDeckHtml = "<html><body><div id=\"log\"></div></body></html>",
            wcDevPath = Nothing
          }
    (port, sock) <- openFreePort
    server <- async (runSettingsSocket defaultSettings sock app)
    threadDelay 300_000

    -- Read the meeting back from the global log, filtered to the channel.
    livePosts <- readMeetingPosts chanDir (unChannel chan) (length purePosts)
    pass ("pure posts: " <> show (length purePosts) <> ", live posts: " <> show (length livePosts))
    assert "live post count matches pure" $ length livePosts == length purePosts

    -- The core oracle: the live drawing is hyper-equivalent to the pure one.
    let skelPure = meetingSkeleton purePosts
        skelLive = meetingSkeleton livePosts
    assert "live skeleton hyper-equivalent to pure" $ skelPure `hyperEquiv` skelLive
    pass "live drawing matches pure meeting semantics"

    -- SVG sanity: render the live skeleton directly.
    let svg = renderSkeletonSvg skelLive
    assert "SVG is non-empty" $ not (BS.null svg)
    let svgText = TE.decodeUtf8 svg
    assert "SVG contains a fork/merge spider" $ "circle" `T.isInfixOf` svgText
    assert "SVG labels every seat" $ all (`T.isInfixOf` svgText) (map from purePosts)
    pass "SVG renders the meeting"

    -- Deck endpoint: the live server serves the same drawing.
    (st, ct, body) <- httpGet port "/api/diagram.svg?channel=panel"
    assert ("deck endpoint status 200, got " <> show st) $ st == 200
    assert "deck content-type is svg" $ "image/svg+xml" `isPrefixOf` ct
    assert "deck SVG labels a seat" $ "agent-1" `isInfixOf` body
    pass "deck endpoint renders the meeting live"

    -- Optional real-agent demo.
    runLiveSeatDemo root port

    cancel server

  putStrLn "muster-meeting-runner: PASS"
