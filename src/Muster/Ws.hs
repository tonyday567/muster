{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The muster-ws server, as a testable library: 'mkApp' builds the WAI
-- application; @app\/MusterWs.hs@ is a thin Main over it.
--
-- Structural refactor (launch L1). The fan is one explicit box per
-- channel, not four ad-hoc asyncs per client:
--
--   * a 'ChannelManager' (MVar) owns channel entries — the check-then-act
--     race that leaked pumps and dual-polled the file cursor is gone;
--   * the pump uses a private in-memory 'Cursor' with
--     'Cur.pollNumberedFile' — complete lines only, absolute line
--     numbers, no shared file cursor, no stale-replay on restart;
--   * clients subscribe ('dupTChan') /before/ reading history and dedup
--     by line number — one source (the log), no gap, no duplicates;
--   * per-client outbound is a bounded 'TBQueue', drop-oldest;
--   * the commit path is waited: FIFO failures come back as error frames;
--   * bodies containing newlines are rejected (the frame is one line per
--     post — newline injection is log forgery);
--   * the emit port parses once: clients receive structured JSON
--     (@postJson@) — the deck's regex parser is dead.
module Muster.Ws
  ( WsConfig (..),
    mkApp,
    BusLine (..),
    postJson,
    errorJson,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, waitEither)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, displayException, finally, try)
import Control.Monad (filterM, forever, void, when)
import Cursor qualified as Cur
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as TIO
import Free.Agent.Diagram (meetingSkeleton)
import Muster.Channel
  ( Channel,
    ChannelConfig (..),
    channelAttach,
    channelSend,
    defaultChannelConfig,
  )
import Muster.Diagram (diagramJson, readMeetingPosts, renderSkeletonSvg)
import Muster.Framing qualified as Framing
import Network.HTTP.Types (parseQuery, status200, status400, status404)
import Network.Wai
  ( Application,
    Request,
    Response,
    pathInfo,
    rawQueryString,
    responseLBS,
  )
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
  ( Connection,
    PendingConnection (..),
    RequestHead (..),
    ServerApp,
    acceptRequest,
    defaultConnectionOptions,
    receiveData,
    sendTextData,
  )
import Numeric.Natural (Natural)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (CPid (..))
import System.Process (createProcess, proc)
import System.Timeout (timeout)
import Prelude

-- | Server configuration. Deck HTML is injected so the library has no
-- file-embed dependency; Main embeds, the smoke passes a stub.
data WsConfig = WsConfig
  { wcName :: Text,
    wcChannel :: String,
    wcBusRoot :: FilePath,
    wcHistory :: Int,
    wcBoard :: FilePath,
    wcDeckHtml :: LBS.ByteString,
    wcDevPath :: Maybe FilePath
  }

-- | A bus line: absolute 1-based line number + raw framed text.
data BusLine = BusLine
  { blN :: Int,
    blRaw :: Text
  }

-- ---------------------------------------------------------------------------
-- Channel manager — one owner per channel
-- ---------------------------------------------------------------------------

data ChannelEntry = ChannelEntry
  { ceChannel :: Channel,
    ceFan :: TChan BusLine,
    -- | The manager owns the pump for the process lifetime (channels
    -- never close); kept for a future teardown pass.
    _cePump :: Async ()
  }

-- | A cache of active channels, keyed by channel name.  All
-- check-then-act happens inside 'modifyMVar': at most one pump, one
-- cursor, one FIFO write handle per channel, ever.
newtype ChannelManager = ChannelManager (MVar (Map.Map String ChannelEntry))

newChannelManager :: IO ChannelManager
newChannelManager = ChannelManager <$> newMVar Map.empty

-- | How long channel attach may block on the FIFO open before we call
-- the bus dead.  A FIFO opened for writing blocks until a reader (the
-- central daemon's relay) exists; without a bound this wedges a warp
-- handler forever.
attachTimeoutUs :: Int
attachTimeoutUs = 5_000_000

-- | Get or create a channel entry by name.
getOrCreateChannel :: FilePath -> Text -> ChannelManager -> String -> IO ChannelEntry
getOrCreateChannel root identity (ChannelManager mv) chanName =
  modifyMVar mv $ \m ->
    case Map.lookup chanName m of
      Just entry -> pure (m, entry)
      Nothing -> do
        entry <- newChannelEntry root identity chanName
        pure (Map.insert chanName entry m, entry)

newChannelEntry :: FilePath -> Text -> String -> IO ChannelEntry
newChannelEntry root identity chanName = do
  let cfg =
        (defaultChannelConfig identity)
          { chChannel = chanName,
            chBusRoot = root
          }
      logf = root </> chanName </> "log.md"
  mch <- timeout attachTimeoutUs (channelAttach cfg)
  ch <- case mch of
    Just c -> pure c
    Nothing ->
      ioError (userError ("muster-ws: bus not answering for channel " <> chanName <> " (FIFO open blocked — is the central daemon running?)"))
  fan <- newBroadcastTChanIO
  -- Private in-memory cursor, complete lines only, absolute numbers.
  -- The Channel's file cursor (.cursor-deck) is deliberately unused:
  -- restart must resume at "now", not mid-log.
  cur <- Cur.newMem 0
  Cur.seekEndFile cur logf
  pump <- async $ forever $ do
    ls <- map (uncurry BusLine) <$> Cur.pollNumberedFile cur logf
    atomically $ mapM_ (writeTChan fan) ls
    when (null ls) $ threadDelay 50_000
  pure (ChannelEntry ch fan pump)

-- ---------------------------------------------------------------------------
-- Wire format — the emit port parses once
-- ---------------------------------------------------------------------------

-- | A post frame: parsed fields plus the raw line (the cheap regression
-- view).  Unparseable lines carry null sender/body.
postJson :: Bool -> BusLine -> Text
postJson hist (BusLine n raw) =
  case Framing.parseMessage raw of
    Just (sender, body) ->
      T.concat
        [ "{\"type\":\"post\",\"history\":",
          if hist then "true" else "false",
          ",\"n\":",
          T.pack (show n),
          ",\"raw\":",
          jsonStr raw,
          ",\"sender\":",
          jsonStr sender,
          ",\"body\":",
          jsonStr body,
          "}"
        ]
    Nothing ->
      T.concat
        [ "{\"type\":\"post\",\"history\":",
          if hist then "true" else "false",
          ",\"n\":",
          T.pack (show n),
          ",\"raw\":",
          jsonStr raw,
          ",\"sender\":null,\"body\":null}"
        ]

-- | An error frame: post failures and validation rejections, made
-- visible to the client (the "deck posts stop appearing" bug was silent).
errorJson :: Text -> Text
errorJson msg = T.concat ["{\"type\":\"error\",\"message\":", jsonStr msg, "}"]

jsonStr :: Text -> Text
jsonStr t =
  T.concat ["\"", T.concatMap esc t, "\""]
  where
    esc c
      | c == '"' = "\\\""
      | c == '\\' = "\\\\"
      | c == '\n' = "\\n"
      | c == '\r' = "\\r"
      | c == '\t' = "\\t"
      | otherwise = T.singleton c

-- ---------------------------------------------------------------------------
-- Application
-- ---------------------------------------------------------------------------

mkApp :: WsConfig -> IO Application
mkApp cfg = do
  manager <- newChannelManager
  -- Pre-attach the default channel: fail fast (bounded) if the bus is dead.
  _ <- getOrCreateChannel (wcBusRoot cfg) (wcName cfg) manager (wcChannel cfg)
  pure $ websocketsOr defaultConnectionOptions (clientApp cfg manager) (staticApp cfg)

-- ---------------------------------------------------------------------------
-- WebSocket session
-- ---------------------------------------------------------------------------

-- | Per-client outbound bound. A slow client drops the oldest lines; it
-- never backpressures the meeting.
outBound :: Natural
outBound = 1000

data OutMsg = OutPost Bool BusLine | OutError Text | OutDiagram Text

clientApp :: WsConfig -> ChannelManager -> ServerApp
clientApp cfg manager pending = do
  conn <- acceptRequest pending
  let chan = extractChannelFromWs pending (wcChannel cfg)
  entry <- getOrCreateChannel (wcBusRoot cfg) (wcName cfg) manager chan
  let logf = wcBusRoot cfg </> chan </> "log.md"
  clientSession entry (T.pack chan) logf (wcHistory cfg) conn

clientSession :: ChannelEntry -> Text -> FilePath -> Int -> Connection -> IO ()
clientSession entry chan logf hist conn = do
  -- Subscribe BEFORE reading history: the TChan buffers everything from
  -- here on, and the line-number filter below removes the overlap. One
  -- source (the log), no gap, no duplicates.
  myFan <- atomically $ dupTChan (ceFan entry)
  histLines <- readHistoryNumbered logf hist
  let histMax = foldl' (\acc (n, _) -> max acc n) 0 histLines
  outQ <- newTBQueueIO outBound
  inQ <- newTQueueIO
  mapM_ (atomically . writeDropOldest outQ . OutPost True . uncurry BusLine) histLines
  tFan <- async $ forever $ do
    bl <- atomically $ readTChan myFan
    when (blN bl > histMax) $
      atomically $ do
        writeDropOldest outQ (OutPost False bl)
        -- Live growth nudge (launch L3): the deck refetches the SVG on
        -- this frame.  Cheap on purpose — no SVG in the frame, and a
        -- full skeleton re-send per post, no incremental patching.
        writeDropOldest outQ (OutDiagram chan)
  tPost <- async $ forever $ do
    t <- atomically $ readTQueue inQ
    r <- try @SomeException (handlePost entry outQ t)
    case r of
      Left err ->
        atomically $
          writeDropOldest outQ (OutError ("post failed: " <> T.pack (displayException err)))
      Right _ -> pure ()
  tOut <- async $ forever $ do
    m <- atomically $ readTBQueue outQ
    sendTextData conn (encodeOut m)
  tIn <- async $ forever $ do
    msg <- receiveData conn
    atomically $ writeTQueue inQ (msg :: Text)
  void $
    finally
      (waitEitherCancel tIn tOut)
      (mapM_ cancel [tFan, tPost, tIn, tOut])

encodeOut :: OutMsg -> Text
encodeOut (OutPost hist bl) = postJson hist bl
encodeOut (OutError e) = errorJson e
encodeOut (OutDiagram chan) = T.concat ["{\"type\":\"diagram\",\"channel\":", jsonStr chan, "}"]

-- | The commit path, validated. Multi-line bodies are rejected: the bus
-- frame is one line per post, so a newline is log forgery.
handlePost :: ChannelEntry -> TBQueue OutMsg -> Text -> IO ()
handlePost entry outQ t
  | T.null body = pure ()
  | T.any (== '\n') body =
      atomically $
        writeDropOldest outQ (OutError "multi-line bodies are not supported on the bus")
  | otherwise = channelSend (ceChannel entry) body
  where
    body = T.strip t

-- | Write to a bounded queue, dropping the oldest element when full.
writeDropOldest :: TBQueue a -> a -> STM ()
writeDropOldest q x = do
  full <- isFullTBQueue q
  when full $ void (tryReadTBQueue q)
  writeTBQueue q x

-- | Read the last @n@ complete lines of a log, with absolute 1-based
-- line numbers.
readHistoryNumbered :: FilePath -> Int -> IO [(Int, Text)]
readHistoryNumbered path n = do
  ls <- Cur.readLogLinesComplete path
  let numbered = zip [1 ..] ls
  pure $
    if n <= 0
      then []
      else drop (max 0 (length ls - n)) numbered

-- | Extract the channel name from the WebSocket request's query string
-- (any position, not just first). Returns the default channel if absent.
extractChannelFromWs :: PendingConnection -> String -> String
extractChannelFromWs pending defaultChannel =
  let pathText = decodeUtf8 (requestPath (pendingRequest pending))
      (_, q) = T.break (== '?') pathText
      qs = parseQuery (encodeUtf8 q)
   in case lookup "channel" qs of
        Just (Just c) | not (T.null (decodeUtf8 c)) -> T.unpack (decodeUtf8 c)
        _ -> defaultChannel

waitEitherCancel :: Async a -> Async b -> IO ()
waitEitherCancel a b = do
  void $ waitEither a b
  cancel a
  cancel b

-- ---------------------------------------------------------------------------
-- HTTP
-- ---------------------------------------------------------------------------

staticApp :: WsConfig -> Application
staticApp cfg req respond =
  case pathInfo req of
    [] -> serveHtml cfg req respond
    ["api", "state"] -> do
      let qs = parseQuery (rawQueryString req)
          chan = case lookup "channel" qs of
            Just (Just c) -> decodeUtf8 c
            _ -> T.pack (wcChannel cfg)
          chanLogf = wcBusRoot cfg </> T.unpack chan </> "log.md"
      body <- buildStateJson (wcBoard cfg) (wcBusRoot cfg </> "agents") chanLogf chan (wcName cfg)
      respond $
        responseLBS
          status200
          [ ("Content-Type", "application/json; charset=utf-8"),
            ("Cache-Control", "no-store")
          ]
          (LBS.fromStrict (encodeUtf8 body))
    ["api", "board"] -> do
      t <- readFileUtf8 (wcBoard cfg)
      respond $
        responseLBS
          status200
          [("Content-Type", "text/plain; charset=utf-8"), ("Cache-Control", "no-store")]
          (LBS.fromStrict (encodeUtf8 t))
    -- The meeting skeleton of the log tail as JSON (launch L3): the dag.md
    -- wiring record aligned over the log, drawn by 'meetingSkeleton'.
    ["api", "diagram"] -> do
      let chan = queryChannel cfg req
      posts <- readMeetingPosts (wcBusRoot cfg </> T.unpack chan) (wcHistory cfg)
      let body =
            encode
              ( object
                  [ "channel" .= chan,
                    "lines" .= length posts,
                    "skeleton" .= diagramJson (meetingSkeleton posts)
                  ]
              )
      respond $
        responseLBS
          status200
          [ ("Content-Type", "application/json; charset=utf-8"),
            ("Cache-Control", "no-store")
          ]
          body
    -- The same skeleton rendered to SVG (strings-svg -> chart-svg).
    ["api", "diagram.svg"] -> do
      let chan = queryChannel cfg req
      posts <- readMeetingPosts (wcBusRoot cfg </> T.unpack chan) (wcHistory cfg)
      respond $
        responseLBS
          status200
          [ ("Content-Type", "image/svg+xml"),
            ("Cache-Control", "no-store")
          ]
          (LBS.fromStrict (renderSkeletonSvg (meetingSkeleton posts)))
    ["api", "open"] -> do
      let loomDir = takeDirectory (wcBoard cfg)
          qs = parseQuery (rawQueryString req)
          mpath = case lookup "path" qs of
            Just (Just p) -> Just (loomDir </> T.unpack (decodeUtf8 p))
            _ -> Nothing
      case mpath of
        Just fullPath -> do
          void $ try @IOException $ createProcess (proc "open" [fullPath])
          respond $ responseLBS status200 [("Content-Type", "text/plain")] "ok"
        Nothing ->
          respond $ responseLBS status400 [("Content-Type", "text/plain")] "missing path"
    ["channels"] -> do
      dirs <- listChannels (wcBusRoot cfg)
      let json =
            "[" <> T.intercalate "," (map jsonStr (map T.pack dirs)) <> "]"
      respond $
        responseLBS
          status200
          [("Content-Type", "application/json; charset=utf-8"), ("Cache-Control", "no-store")]
          (LBS.fromStrict (encodeUtf8 json))
    _ ->
      respond $
        responseLBS status404 [("Content-Type", "text/plain")] "not found"

-- | The @?channel=@ query parameter, or the server's default channel.
queryChannel :: WsConfig -> Request -> Text
queryChannel cfg req =
  case lookup "channel" (parseQuery (rawQueryString req)) of
    Just (Just c) -> decodeUtf8 c
    _ -> T.pack (wcChannel cfg)

-- | List available channel directories under the bus root.
listChannels :: FilePath -> IO [String]
listChannels root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      ents <- listDirectory root
      let isChannelDir name = do
            let full = root </> name
                tname = T.pack name
            isDir <- doesDirectoryExist full
            pure (isDir && name /= "." && name /= ".." && name /= "agents" && not ("_" `T.isPrefixOf` tname) && not ("." `T.isPrefixOf` tname))
      filterM isChannelDir ents

serveHtml :: WsConfig -> Request -> (Response -> IO a) -> IO a
serveHtml cfg req respond = do
  html <- case wcDevPath cfg of
    Nothing -> pure (wcDeckHtml cfg)
    Just devPath -> do
      res <- try @IOException (LBS.readFile devPath)
      case res of
        Left _ -> pure (wcDeckHtml cfg)
        Right bytes -> pure bytes
  let qs = parseQuery (rawQueryString req)
      chan = case lookup "channel" qs of
        Just (Just c) -> decodeUtf8 c
        _ -> T.pack (wcChannel cfg)
      escape t = T.replace "\\" "\\\\" $ T.replace "\"" "\\\"" t
      injected =
        LBS.fromStrict
          ( encodeUtf8 $
              T.concat
                [ "<script>",
                  "window.DEFAULT_CHANNEL=\"",
                  escape chan,
                  "\";",
                  "window.DEFAULT_NAME=\"",
                  escape (wcName cfg),
                  "\";",
                  "window.DEFAULT_BUS_ROOT=\"",
                  escape (T.pack (wcBusRoot cfg)),
                  "\";",
                  "</script>\n"
                ]
          )
          <> html
  respond $
    responseLBS
      status200
      [ ("Content-Type", "text/html; charset=utf-8"),
        ("Cache-Control", "no-store, no-cache, must-revalidate")
      ]
      injected

buildStateJson :: FilePath -> FilePath -> FilePath -> Text -> Text -> IO Text
buildStateJson boardPath agentsDir logf channel identity = do
  board <- readFileUtf8 boardPath
  agents <- listAgents agentsDir
  lastBy <- lastLinesBySender logf 30
  msession <- readSessionInfo (takeDirectory logf)
  let hermesRow = agentJson (AgentInfo "hermes" "active" Nothing "") (Map.lookup "hermes" lastBy)
      allAgentRows = hermesRow : [ agentJson a (Map.lookup (agName a) lastBy) | a <- agents, agStatus a /= "down" ]
      projects = projectLines board
      busNames = Map.keys lastBy
      sessionJson = case msession of
        Nothing -> "null"
        Just s ->
          T.concat
            [ "{\"status\":",
              jsonStr (siStatus s),
              ",\"opened\":",
              jsonStr (siOpened s),
              ",\"participants\":[",
              T.intercalate "," (map jsonStr (siParticipants s)),
              "],\"logStart\":",
              T.pack (show (siLogStart s)),
              "}"
            ]
  pure $
    T.concat
      [ "{\"channel\":",
        jsonStr channel,
        ",\"identity\":",
        jsonStr identity,
        ",\"boardPath\":",
        jsonStr (T.pack boardPath),
        ",\"board\":",
        jsonStr board,
        ",\"projects\":[",
        T.intercalate "," (map jsonStr projects),
        "],\"agents\":[",
        T.intercalate "," allAgentRows,
        "],\"bus\":[",
        T.intercalate "," (map jsonStr busNames),
        "],\"session\":",
        sessionJson,
        "}"
      ]

data SessionInfo = SessionInfo
  { siStatus :: Text,
    siOpened :: Text,
    siParticipants :: [Text],
    siLogStart :: Int
  }

readSessionInfo :: FilePath -> IO (Maybe SessionInfo)
readSessionInfo dir = do
  let path = dir </> "session.md"
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      raw <- TIO.readFile path
      let kv = map (T.breakOn ":") (T.lines raw)
          lookupKey k =
            listToMaybe
              [ T.strip (T.drop 1 v)
                | (k', v) <- kv,
                  T.strip k' == k
              ]
          participants =
            maybe
              []
              (filter (not . T.null) . map T.strip . T.words)
              (lookupKey "participants")
          logStart =
            maybe
              0
              (\v -> case reads (T.unpack (T.strip v)) of [(n, _)] -> n; _ -> 0)
              (lookupKey "log-start")
      case lookupKey "status" of
        Nothing -> pure Nothing
        Just st ->
          pure $
            Just
              SessionInfo
                { siStatus = st,
                  siOpened = fromMaybe "" (lookupKey "opened"),
                  siParticipants = participants,
                  siLogStart = logStart
                }

data AgentInfo = AgentInfo
  { agName :: Text,
    agStatus :: Text,
    agPid :: Maybe Int,
    agModel :: Text
  }

listAgents :: FilePath -> IO [AgentInfo]
listAgents dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      ents <- listDirectory dir
      let names = filter (\n -> n /= "_coord" && n /= "." && n /= "..") ents
      mapM (readAgent dir) names

readAgent :: FilePath -> FilePath -> IO AgentInfo
readAgent dir name = do
  let base = dir </> name
      pidPath = base </> "agent.pid"
      cfgPath = base </> "config"
  mpid <- readPid pidPath
  st <- case mpid of
    Nothing -> pure "down"
    Just p -> do
      alive <- pidAlive p
      pure $ if alive then "alive" else "stale"
  model <- readModel cfgPath
  pure $
    AgentInfo
      { agName = T.pack name,
        agStatus = st,
        agPid = mpid,
        agModel = model
      }

readPid :: FilePath -> IO (Maybe Int)
readPid path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      t <- T.strip <$> TIO.readFile path
      pure $
        case reads (T.unpack (T.takeWhile (/= '\n') t)) of
          [(n, _)] -> Just n
          _ -> Nothing

pidAlive :: Int -> IO Bool
pidAlive p = do
  r <- try @IOException $ signalProcess nullSignal (CPid (fromIntegral p))
  pure $ case r of
    Left _ -> False
    Right _ -> True

readModel :: FilePath -> IO Text
readModel path = do
  exists <- doesFileExist path
  if not exists
    then pure ""
    else do
      t <- TIO.readFile path
      let ms =
            [ T.strip (T.drop 1 rest)
              | l <- T.lines t,
                let (k, rest) = T.breakOn "=" l,
                T.strip k == "model",
                T.isPrefixOf "=" rest
            ]
      pure $ case ms of
        (m : _) -> m
        [] -> ""

lastLinesBySender :: FilePath -> Int -> IO (Map.Map Text Text)
lastLinesBySender path n = do
  exists <- doesFileExist path
  if not exists
    then pure Map.empty
    else do
      raw <- TIO.readFile path
      let ls = reverse $ take n $ reverse $ filter (not . T.null) $ T.lines raw
          step m line =
            case Framing.parseMessage line of
              Nothing -> m
              Just (sender, _) -> Map.insert sender line m
      pure $ foldl' step Map.empty ls

projectLines :: Text -> [Text]
projectLines board =
  [ T.strip l
    | l <- T.lines board,
      let s = T.strip l,
      T.isInfixOf "bus-deck" s
  ]

agentJson :: AgentInfo -> Maybe Text -> Text
agentJson a mlast =
  T.concat
    [ "{\"name\":",
      jsonStr (agName a),
      ",\"status\":",
      jsonStr (agStatus a),
      ",\"pid\":",
      maybe "null" (T.pack . show) (agPid a),
      ",\"model\":",
      jsonStr (agModel a),
      ",\"last\":",
      jsonStr (maybe "" id mlast),
      "}"
    ]

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = do
  exists <- doesFileExist path
  if not exists then pure "" else TIO.readFile path
