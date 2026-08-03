{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The live deck diagram (launch L3): read a channel's log + @dag.md@
-- wiring record back as a 'SDiagram' skeleton, serialise it as JSON for
-- @GET \/api\/diagram@, and render it to SVG bytes for
-- @GET \/api\/diagram.svg@.
--
-- The log carries only @[ts] sender: body@ — thread ancestry lives in the
-- @dag.md@ sidecar written by 'Muster.Api.Bus.postWithThread'.  Coverage
-- may be partial (plain 'Muster.Api.Bus.post' writes no dag entry), so a
-- log line without a matching dag entry is honestly a root: "no wiring
-- recorded", never an invented merge.  With no @dag.md@ at all, every
-- line is a root.
module Muster.Diagram
  ( readMeetingPosts,
    diagramJson,
    renderSkeletonSvg,
  )
where

import Chart (encodeChartOptions)
import Circuit.Agent (Post (..))
import Circuit.Parser.Json (Json (..), decodeJson)
import Circuit.Poly.StringDiagram (SDiagram (..))
import Cursor qualified as Cur
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Muster.Framing qualified as Framing
import Strings.Svg.Render (renderSDiagram)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Prelude

-- | One @dag.md@ line: the wiring record of a single post.
data DagEntry = DagEntry
  { deFrom :: Text,
    deThread :: [Text],
    deBody :: Text
  }

-- | Parse one @dag.md@ JSON line. Anything malformed is dropped — a
-- half-written or corrupted line must not kill the whole diagram.
parseDagEntry :: Text -> Maybe DagEntry
parseDagEntry line = case decodeJson (encodeUtf8 line) of
  Right (JObject o) -> do
    JString f <- lookup "from" o
    JString b <- lookup "body" o
    ts <- case lookup "thread" o of
      Just (JArray a) -> traverse (\case JString t -> Just t; _ -> Nothing) (V.toList a)
      _ -> Nothing
    pure (DagEntry f ts b)
  _ -> Nothing

-- | Read every parseable @dag.md@ entry, in file order. Missing file is
-- not an error: it means "no wiring recorded".
readDag :: FilePath -> IO [DagEntry]
readDag path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      raw <- TIO.readFile path
      pure [e | l <- T.lines raw, Just e <- [parseDagEntry l]]

-- | Align dag entries to parsed log lines greedily, in order: the first
-- dag entry whose from+body matches the line is consumed and supplies the
-- thread ancestry; a line without a matching entry gets @thread=[]@.
alignPosts :: [DagEntry] -> [(Text, Text)] -> [Post Text]
alignPosts dags = go dags
  where
    go _ [] = []
    go ds ((sender, body) : rest) = case ds of
      (d : ds')
        | deFrom d == sender && deBody d == body ->
            Post sender [] (deThread d) body : go ds' rest
      _ -> Post sender [] [] body : go ds rest

-- | Reconstruct the meeting from a channel directory: parse the log into
-- @(sender, body)@ pairs, align the @dag.md@ wiring record over the full
-- log (alignment is positional, so the tail alone would mismatch), then
-- keep the last @n@ posts — the same tail the ws history sends.
readMeetingPosts :: FilePath -> Int -> IO [Post Text]
readMeetingPosts chanDir histN = do
  ls <- Cur.readLogLinesComplete (chanDir </> "log.md")
  dags <- readDag (chanDir </> "dag.md")
  let parsed =
        [ (sender, body)
          | l <- ls,
            Just (sender, body) <- [Framing.parseMessage l]
        ]
      posts = alignPosts dags parsed
  pure (takeLast histN posts)
  where
    takeLast m xs
      | m <= 0 = []
      | otherwise = drop (max 0 (length xs - m)) xs

-- | A drawing skeleton as plain JSON, lowercase constructor tags:
-- @{"tag":"box","label":..,"ins":..,"outs":..}@,
-- @{"tag":"spider","ins":..,"outs":..}@,
-- @{"tag":"beside","left":..,"right":..}@, and so on.  A plain function,
-- deliberately not an instance — the skeleton type belongs to
-- string-diagrams and the wire format belongs here.
diagramJson :: SDiagram -> Json
diagramJson = \case
  SWire -> tag "wire" []
  SBox l i o -> tag "box" [("label", JString (T.pack l)), ("ins", jint i), ("outs", jint o)]
  SSpider i o -> tag "spider" [("ins", jint i), ("outs", jint o)]
  SPrismBox -> tag "prism" []
  SBeside f g -> tag "beside" [("left", diagramJson f), ("right", diagramJson g)]
  SThenD f g -> tag "then" [("first", diagramJson f), ("second", diagramJson g)]
  SBend -> tag "cup" []
  SBend' -> tag "cap" []
  STurn f -> tag "turn" [("diagram", diagramJson f)]
  SUnitL -> tag "unitl" []
  SUnitL' -> tag "unitl'" []
  SUnitR -> tag "unitr" []
  SUnitR' -> tag "unitr'" []
  SAssoc -> tag "assoc" []
  SAssoc' -> tag "assoc'" []
  SSwap -> tag "swap" []
  where
    tag :: Text -> [(Text, Json)] -> Json
    tag t fs = JObject (("tag", JString t) : fs)
    jint :: Int -> Json
    jint = JNumber . fromIntegral

-- | Render a skeleton to SVG bytes via strings-svg's 'renderSDiagram'
-- and chart-svg's 'encodeChartOptions' — the same render path as
-- @free-agent/app/render-meeting.hs@, minus the file write.
renderSkeletonSvg :: SDiagram -> ByteString
renderSkeletonSvg = encodeChartOptions . renderSDiagram
