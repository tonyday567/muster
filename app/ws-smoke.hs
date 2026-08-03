{-# LANGUAGE OverloadedStrings #-}

-- | Structural oracle for the muster-ws refactor (launch L1).
--
-- Boots a real in-process central daemon + muster-ws server on a temp
-- bus, drives two websocket clients, and pins:
--
--   * history arrives as numbered post frames (parse-once protocol);
--   * a post from one client reaches both clients exactly once, framed
--     with the server identity;
--   * a burst arrives contiguous and distinct (no gap, no duplicates);
--   * multi-line bodies are rejected with a visible error frame;
--   * the channel query param works in any position;
--   * a dead bus turns the next post into a visible error frame
--     (the "deck posts stop appearing" bug, now loud).
--
-- Launch L3 adds the diagram stage, on a fresh channel after the daemon
-- is restarted: a wired meeting seeded via 'postWithThread', the
-- @\/api\/diagram@ and @\/api\/diagram.svg@ endpoints (HTTP via curl —
-- the smoke has no http-client dep, curl is the simplest client), and
-- the live-growth @{type:"diagram"}@ nudge frame after a live post.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, race, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Monad (forM_, forever)
import Data.Aeson (Value (..), decode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Muster.Api.Bus (centralDaemon, ensureChannel, post, postWithThread)
import Muster.Api.Types (BusRoot (..), Channel (..), Nick (..))
import Muster.Ws (WsConfig (..), mkApp)
import Network.Wai.Handler.Warp (defaultSettings, openFreePort, runSettingsSocket)
import Network.WebSockets.Client (runClient)
import Network.WebSockets (receiveData, sendTextData)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Process (readProcess)
import System.Timeout (timeout)
import Prelude

assert :: String -> Bool -> IO ()
assert msg ok =
  if ok
    then putStrLn ("  PASS " ++ msg)
    else do
      putStrLn ("  FAIL " ++ msg)
      exitFailure

-- | A decoded wire frame (only what the oracle inspects):
-- history flag, line number, sender, body.
data Msg
  = MsgPost Bool Int (Maybe Text) (Maybe Text)
  | MsgError Text
  | MsgDiagram Text
  | MsgOther

parseMsg :: Text -> Msg
parseMsg t = case decode (LBS.fromStrict (encodeUtf8 t)) of
  Just (Object o) ->
    case KM.lookup "type" o of
      Just (String "post") ->
        MsgPost
          (KM.lookup "history" o == Just (Bool True))
          (case KM.lookup "n" o of
             Just (Number n) -> floor n
             _ -> -1)
          (case KM.lookup "sender" o of
             Just (String s) -> Just s
             _ -> Nothing)
          (case KM.lookup "body" o of
             Just (String b) -> Just b
             _ -> Nothing)
      Just (String "error") ->
        MsgError (case KM.lookup "message" o of
                    Just (String m) -> m
                    _ -> "")
      Just (String "diagram") ->
        MsgDiagram (case KM.lookup "channel" o of
                      Just (String c) -> c
                      _ -> "")
      _ -> MsgOther
  _ -> MsgOther

-- | A test websocket client: bounded recv, plain send, cancellable.
data WsClient = WsClient
  { cRecv :: IO (Maybe Text),
    cSend :: Text -> IO (),
    cClose :: IO ()
  }

connectWs :: Int -> String -> IO WsClient
connectWs port path = do
  inbox <- newTChanIO
  outbox <- newTChanIO
  ready <- newEmptyMVar
  tid <- async $ runClient "127.0.0.1" port path $ \conn -> do
    putMVar ready ()
    _ <- async $ forever $ do
      m <- receiveData conn
      atomically $ writeTChan inbox m
    forever $ do
      m <- atomically $ readTChan outbox
      sendTextData conn (m :: Text)
  r <- race (takeMVar ready) (waitCatch tid)
  case r of
    Left _ ->
      pure
        WsClient
          { cRecv = timeout 5_000_000 (atomically $ readTChan inbox),
            cSend = atomically . writeTChan outbox,
            cClose = cancel tid
          }
    Right (Left e) -> fail ("ws client died: " <> show e)
    Right (Right _) -> fail "ws connection closed before ready"

-- | Poll a predicate until it holds or the patience runs out.
waitFor :: String -> IO Bool -> IO ()
waitFor what p = go (50 :: Int)
  where
    go 0 = do
      ok <- p
      assert what ok
    go n = do
      ok <- p
      if ok then pure () else threadDelay 100_000 >> go (n - 1)

-- | Receive one frame and decode it, failing on timeout.
expect :: String -> WsClient -> IO Msg
expect what c = do
  m <- cRecv c
  case m of
    Nothing -> do
      putStrLn ("  FAIL timeout waiting for " ++ what)
      exitFailure
    Just t -> pure (parseMsg t)

-- | Receive frames until the next post frame, skipping diagram nudges
-- (launch L3: every live post is followed by a @{type:"diagram"}@ frame).
-- Error frames surface to the caller.
expectPost :: String -> WsClient -> IO Msg
expectPost what c = go (20 :: Int)
  where
    go 0 = do
      putStrLn ("  FAIL too many non-post frames waiting for " ++ what)
      exitFailure
    go k = do
      m <- expect what c
      case m of
        MsgDiagram _ -> go (k - 1)
        _ -> pure m

-- | GET via curl: @(status, content-type, body)@.  The smoke has no
-- http-client dep; curl -i is the simplest possible client here.
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

main :: IO ()
main = do
  putStrLn "muster-ws-smoke: two clients, exactly-once, no gap, loud failure"
  let root = BusRoot "/tmp/muster-ws-smoke"
      chan = Channel "panel"
      chanDir = unBusRoot root </> "panel"
      logf = chanDir </> "log.md"
  removePathForcibly (unBusRoot root)
  createDirectoryIfMissing True chanDir

  daemon <- async (centralDaemon root)
  _ <- ensureChannel root chan
  -- Give the scan loop a beat to attach the relay before the first post.
  threadDelay 1_500_000

  -- Seed history through the real bus path.
  forM_ [1 .. 3 :: Int] $ \i ->
    post root chan (Nick "human") ("seed " <> T.pack (show i))
  waitFor "seeds committed to the log" $ do
    raw <- TIO.readFile logf
    pure (length (T.lines raw) >= 3)

  -- The server under test.
  app <-
    mkApp
      WsConfig
        { wcName = "smoke",
          wcChannel = "panel",
          wcBusRoot = unBusRoot root,
          wcHistory = 10,
          wcBoard = unBusRoot root </> "board.md",
          wcDeckHtml = "<html>stub</html>",
          wcDevPath = Nothing
        }
  (port, sock) <- openFreePort
  server <- async (runSettingsSocket defaultSettings sock app)
  threadDelay 300_000

  clientA <- connectWs port "/?channel=panel"
  clientB <- connectWs port "/?channel=panel"

  -- 1. History: three numbered frames, parsed fields, on both clients.
  forM_ [(clientA, "A"), (clientB, "B")] $ \(c, who) ->
    forM_ [1 .. 3 :: Int] $ \i -> do
      m <- expect ("history " <> show i <> " on " <> who) c
      case m of
        MsgPost h n s b ->
          assert ("history frame " <> show i <> " on " <> who) $
            h && n == i && s == Just "human" && b == Just ("seed " <> T.pack (show i))
        _ -> assert ("history frame " <> show i <> " on " <> who) False

  -- 2. A posts; both clients receive it once, framed as the server identity.
  cSend clientA "hello from A"
  forM_ [(clientA, "A"), (clientB, "B")] $ \(c, who) -> do
    m <- expect ("live post on " <> who) c
    case m of
      MsgPost h n s b ->
        assert ("live post on " <> who <> " is numbered, identified, single") $
          not h && n == 4 && s == Just "smoke" && b == Just "hello from A"
      _ -> assert ("live post on " <> who) False

  -- 3. Burst from another participant: contiguous, distinct — no gap, no dup.
  forM_ [1 .. 5 :: Int] $ \i ->
    post root chan (Nick "bot") ("burst " <> T.pack (show i))
  ns <-
    mapM
      ( \_ -> do
          m <- expectPost "burst frame on B" clientB
          case m of
            MsgPost _ n s _ -> do
              assert "burst sender is bot" (s == Just "bot")
              pure n
            _ -> assert "burst frame is a post" False >> pure (-1)
      )
      [5 .. 9 :: Int]
  assert "burst is contiguous and distinct (exactly-once, no gap)" $
    ns == [5, 6, 7, 8, 9]
  -- A heard the same burst; drain it so the error frame below is next up.
  nsA <-
    mapM
      ( \_ -> do
          m <- expectPost "burst frame on A" clientA
          case m of
            MsgPost _ n _ _ -> pure n
            _ -> assert "burst frame on A is a post" False >> pure (-1)
      )
      [5 .. 9 :: Int]
  assert "burst on A matches B" $ nsA == [5, 6, 7, 8, 9]

  -- 4. Multi-line body rejected with a visible error; nothing phantom follows.
  cSend clientA "line1\nline2"
  m <- expectPost "multi-line rejection" clientA
  case m of
    MsgError e -> assert "multi-line body rejected with error frame" ("multi-line" `T.isInfixOf` e)
    _ -> assert "multi-line body rejected with error frame" False
  post root chan (Nick "bot") "after rejection"
  m2 <- expectPost "next real post on B" clientB
  case m2 of
    MsgPost _ n _ b ->
      assert "rejected body produced no phantom post" $
        n == 10 && b == Just "after rejection"
    _ -> assert "next real post on B" False

  -- 5. Query param in a non-first position still selects the channel.
  clientC <- connectWs port "/?foo=1&channel=panel"
  mC <- expect "history on C (query param second)" clientC
  case mC of
    MsgPost h n s _ ->
      assert "channel param works in non-first position" $
        h && n == 1 && s == Just "human"
    _ -> assert "channel param works in non-first position" False
  cClose clientC

  -- 6. Reconnect: history replays as history, no duplicate live frames.
  cClose clientA
  clientA2 <- connectWs port "/?channel=panel"
  mA2 <- expect "history after reconnect" clientA2
  case mA2 of
    MsgPost h n _ _ -> assert "reconnect replays history as history" (h && n == 1)
    _ -> assert "reconnect replays history as history" False
  post root chan (Nick "bot") "post reconnect"
  let drainLive :: Int -> IO (Maybe Text)
      drainLive 0 = pure Nothing
      drainLive k = do
        fr <- expect "frame after reconnect" clientA2
        case fr of
          MsgPost False _ _ (Just b) -> pure (Just b)
          _ -> drainLive (k - 1)
  mb <- drainLive 15
  assert "post-reconnect live frame arrives once, after history replay" $
    mb == Just "post reconnect"

  -- 7. Dead bus: the next post comes back as a visible error frame.
  cancel daemon
  threadDelay 300_000
  cSend clientA2 "doomed"
  mF <- expectPost "post failure error frame" clientA2
  case mF of
    MsgError e -> assert "dead bus surfaces a post failure" ("post failed" `T.isInfixOf` e)
    _ -> assert "dead bus surfaces a post failure" False

  mapM_ cClose [clientA2, clientB]

  -- 8. Diagram stage (launch L3), on a fresh channel with the daemon
  -- restarted: seed a wired meeting via postWithThread — human root, two
  -- replies citing human, one synthesis citing both agents.
  daemon2 <- async (centralDaemon root)
  let dchan = Channel "diagrams"
  _ <- ensureChannel root dchan
  threadDelay 1_500_000
  postWithThread root dchan (Nick "human") [] "root: what should we do?"
  postWithThread root dchan (Nick "agent-1") ["human"] "agent-1: factor A"
  postWithThread root dchan (Nick "agent-2") ["human"] "agent-2: factor B"
  postWithThread root dchan (Nick "synth") ["agent-1", "agent-2"] "synthesis of both"

  -- (a) the skeleton JSON: a visible merge spider, all four senders.
  (stD, _ctD, bodyD) <- httpGet port "/api/diagram?channel=diagrams"
  assert "GET /api/diagram is 200" (stD == 200)
  assert "diagram JSON mentions a spider (the synthesis merge)" ("\"spider\"" `isInfixOf` bodyD)
  forM_ ["human", "agent-1", "agent-2", "synth"] $ \s ->
    assert ("diagram JSON mentions sender " <> s) (s `isInfixOf` bodyD)

  -- (b) the SVG render: svg bytes, no NaN, the synthesis sender label.
  (stS, ctS, bodyS) <- httpGet port "/api/diagram.svg?channel=diagrams"
  assert "GET /api/diagram.svg is 200" (stS == 200)
  assert "diagram.svg is image/svg+xml" ("image/svg+xml" `isInfixOf` ctS)
  assert "diagram.svg contains <svg" ("<svg" `isInfixOf` bodyS)
  assert "diagram.svg has no NaN" (not ("NaN" `isInfixOf` bodyS))
  assert "diagram.svg labels the synthesis sender" ("synth" `isInfixOf` bodyS)

  -- (c) live growth: history frames, then a postWithThread live post,
  -- then the diagram nudge — after the post frame.
  clientD <- connectWs port "/?channel=diagrams"
  forM_ [1 .. 4 :: Int] $ \i -> do
    mh <- expect ("diagrams history " <> show i) clientD
    case mh of
      MsgPost h n _ _ ->
        assert ("diagrams history frame " <> show i) (h && n == i)
      _ -> assert ("diagrams history frame " <> show i) False
  postWithThread root dchan (Nick "agent-1") ["synth"] "agent-1: live follow-up"
  mP <- expect "live post frame on diagrams" clientD
  case mP of
    MsgPost h n s b ->
      assert "live post on diagrams is numbered and identified" $
        not h && n == 5 && s == Just "agent-1" && b == Just "agent-1: live follow-up"
    _ -> assert "live post on diagrams" False
  mDg <- expect "diagram nudge after the live post" clientD
  case mDg of
    MsgDiagram c -> assert "diagram nudge names the channel" (c == "diagrams")
    _ -> assert "diagram nudge after the live post" False
  cClose clientD
  cancel daemon2

  cancel server
  putStrLn "muster-ws-smoke: PASS"
