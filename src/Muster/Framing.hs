{-# LANGUAGE OverloadedStrings #-}

-- | Bus message framing.
--
-- V2 format (default): @[timestamp] sender: body@
-- V1 format (legacy): @[sender] body@
--
-- Parsing is backward-compatible with V1. The prefix is treated as a
-- timestamp when it looks like ISO-8601 (starts with a digit and contains
-- @-@ separators); otherwise it's a legacy sender-only frame.
module Muster.Framing
  ( frameMessage,
    parseMessage,
    formatNow,
  )
where

import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Prelude

-- | Get current time as ISO-8601 text.
formatNow :: IO Text
formatNow = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

-- | Frame a message with timestamp and sender prefix (V2 format).
--
-- >>> frameMessage "2026-07-21T14:30:00" "hermes" "status check"
-- "[2026-07-21T14:30:00] hermes: status check"
frameMessage :: Text -> Text -> Text -> Text
frameMessage ts sender body = "[" <> ts <> "] " <> sender <> ": " <> body

-- | Parse a framed message into @(sender, body, maybeTimestamp)@.
--
-- Parses V2: @[timestamp] sender: body@
-- Fallback V1: @[sender] body@
--
-- >>> parseMessage "[2026-07-21T14:30:00] hermes: status check"
-- Just ("hermes","status check",Just "2026-07-21T14:30:00")
--
-- >>> parseMessage "[hermes] status check"
-- Just ("hermes","status check",Nothing)
--
-- >>> parseMessage "unframed text"
-- Nothing
parseMessage :: Text -> Maybe (Text, Text, Maybe Text)
parseMessage t =
  case T.stripPrefix "[" t of
    Nothing -> Nothing
    Just rest ->
      case T.breakOn "] " rest of
        (prefix, afterBracket)
          | T.null prefix -> Nothing
          | T.null afterBracket -> Nothing
          | otherwise ->
              let after = T.drop 2 afterBracket
               in if T.null after
                    then Nothing
                    else
                      if isTimestampish prefix
                        then parseV2 prefix after
                        else parseV1 prefix after

-- | V2: @[timestamp] sender: body@
parseV2 :: Text -> Text -> Maybe (Text, Text, Maybe Text)
parseV2 ts after =
  case T.breakOn ": " after of
    (sender, body)
      | T.null sender -> Nothing
      | T.null body -> Nothing
      | otherwise ->
          let body' = T.drop 2 body
           in if T.null body'
                then Nothing
                else Just (sender, body', Just ts)

-- | V1: @[sender] body@
parseV1 :: Text -> Text -> Maybe (Text, Text, Maybe Text)
parseV1 sender body
  | T.null body = Nothing
  | otherwise = Just (sender, body, Nothing)

-- | Heuristic: does the prefix look like an ISO-8601 timestamp?
-- Starts with a digit and contains at least one @-@.
isTimestampish :: Text -> Bool
isTimestampish t =
  not (T.null t)
    && isDigit (T.head t)
    && "-" `T.isInfixOf` t
