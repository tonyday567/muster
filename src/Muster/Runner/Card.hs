{-# LANGUAGE OverloadedStrings #-}

-- | Minimal agent-card parser for the runner protocol.
--
-- A card is a markdown file whose body is split into sections under @##@
-- headings.  The runner harness extracts the five standard sections
-- (harness, protocol, readings, task, output) and folds them into a system
-- prompt.  Unknown sections are ignored.
module Muster.Runner.Card
  ( Card (..),
    parseCard,
    cardSystemPrompt,
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Prelude

-- | The five standard sections of an agent card.
data Card = Card
  { cardHarness :: Text,
    cardProtocol :: Text,
    cardReadings :: Text,
    cardTask :: Text,
    cardOutput :: Text
  }
  deriving (Show, Eq)

-- | Parse a markdown card into its sections.
--
-- Sections are introduced by @## <name>@ lines; the section name is compared
-- case-insensitively.  Everything before the first heading is treated as a
-- preamble and ignored.
parseCard :: Text -> Card
parseCard raw =
  let (_, secs) = gather (T.lines raw) [] []
      m = Map.fromList secs
      get k = fromMaybe "" (Map.lookup k m)
   in Card
        { cardHarness = get "harness",
          cardProtocol = get "protocol",
          cardReadings = get "readings",
          cardTask = get "task",
          cardOutput = get "output"
        }
  where
    gather [] pre acc = (T.unlines (reverse pre), reverse acc)
    gather (l : ls) pre acc
      | "## " `T.isPrefixOf` l =
          let name = T.toLower . T.strip $ T.drop 3 l
           in gather ls [] ((name, T.unlines (reverse pre)) : acc)
      | otherwise = gather ls (l : pre) acc

-- | Build a system prompt from the card sections.
cardSystemPrompt :: Card -> Text
cardSystemPrompt c =
  T.unlines
    [ "You are participating in a runner-protocol session.",
      "",
      "## Response rule (strict)",
      "Reply with exactly ONE line.",
      "The VERY FIRST TOKEN of that line must be one of these runner marks:",
      T.unwords validMarks <> ".",
      "Then give a card reference and a short sentence.",
      "No preamble, no bullets, no markdown fences, no explanation before the mark.",
      "",
      "## Harness",
      cardHarness c,
      "",
      "## Protocol",
      cardProtocol c,
      "",
      "## Task",
      cardTask c,
      "",
      "## Output",
      cardOutput c,
      "",
      "## Readings",
      cardReadings c,
      "",
      "## Response rule reminder",
      "Your next output must be ONE line starting with a runner mark."
    ]
  where
    validMarks = ["🟣", "🟡", "🟢", "🔴", "🔵", "✓", "↩"]
