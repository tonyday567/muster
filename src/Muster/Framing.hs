{-# LANGUAGE OverloadedStrings #-}

-- | Bus message framing.
--
-- One message per line: @[sender] body@. This module owns the format so every
-- muster consumer speaks the same framing.
module Muster.Framing
  ( frameMessage,
    parseMessage,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Prelude

-- | Frame a message with sender prefix.
--
-- >>> frameMessage "hermes" "status check"
-- "[hermes] status check"
frameMessage :: Text -> Text -> Text
frameMessage sender body = "[" <> sender <> "] " <> body

-- | Parse a framed message into @(sender, body)@.
--
-- >>> parseMessage "[llm] found a type error"
-- Just ("llm","found a type error")
--
-- >>> parseMessage "unframed text"
-- Nothing
parseMessage :: Text -> Maybe (Text, Text)
parseMessage t =
  case T.stripPrefix "[" t of
    Nothing -> Nothing
    Just rest ->
      case T.breakOn "] " rest of
        (sender, body)
          | T.null sender -> Nothing
          | T.null body -> Nothing
          | otherwise ->
              let body' = T.drop 2 body
               in if T.null body'
                    then Nothing
                    else Just (sender, body')
