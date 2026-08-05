{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The live deck diagram (launch L3): read a channel's stamped log back as
-- a 'SDiagram' skeleton, serialise it as JSON for @GET \/api\/diagram@, and
-- render it to SVG bytes for @GET \/api\/diagram.svg@.
--
-- The log carries JSON Lines of stamped 'Post Text'
-- (@{"id":..., "ts":..., "from":..., "to":..., "thread":..., "body":...}@).
-- Thread ancestry is now part of every log line; there is no sidecar.
module Muster.Diagram
  ( readMeetingPosts,
    diagramJson,
    renderSkeletonSvg,
  )
where

import Chart (encodeChartOptions)
import Circuit.Agent (Post (..))
import Circuit.Agent.Framing (parseLineAt, stamped)
import Circuit.Parser.Json (Json (..))
import Circuit.Poly.StringDiagram (SDiagram (..))
import Cursor qualified as Cur
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Strings.Svg.Render (renderSDiagram)
import System.FilePath ((</>))
import Prelude

-- | Reconstruct the meeting for a channel from the global log: parse the
-- stamped log into 'Post Text' values, keep only posts addressed to the
-- channel, and keep the last @n@ posts — the same tail the ws history sends.
readMeetingPosts :: FilePath -> Text -> Int -> IO [Post Text]
readMeetingPosts root channel histN = do
  ls <- Cur.readLogLinesComplete (root </> "log.jsonl")
  let posts = [stamped s | l <- ls, Just s <- [parseLineAt 0 l], channel `elem` to (stamped s)]
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
