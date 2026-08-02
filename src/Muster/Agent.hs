{-# LANGUAGE OverloadedStrings #-}

-- | Agent utilities: address matching and living 'Shard' adapters.
--
-- The evaluate seat has moved down to circuits-agent ('Circuit.Agent.Cli'):
-- invocation recipes are data ('Cli'), sessions are scraped and resumed with
-- stale fallback, and prompts travel via argv — no shell quoting, so
-- multi-line bodies survive.  This module keeps muster's addressing helpers
-- and the hermes-flavoured 'AgentConfig', and re-exports the shard adapters.
module Muster.Agent
  ( AgentMode (..),
    AgentConfig (..),
    defaultAgentConfig,
    addressedTo,
    stripAddress,
    agentCli,
    agentQuery,

    -- * Living agent as Shard (from 'Circuit.Agent.Cli')
    queryShard,
    synthShard,
    oneshotShard,
    echoShard,
    runShard,
    sessionPrompt,
    replyPosts,

    -- * Re-exports (seat types)
    Post (..),
    Shard,
  )
where

import Circuit.Agent (Post (..), Shard)
import Circuit.Agent.Cli
  ( Cli,
    cliQuery,
    cliShard,
    echoShard,
    hermesCli,
    queryShard,
    replyPosts,
    runShardIO,
    sessionPrompt,
    synthShard,
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Muster.Config qualified as Config
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Prelude

-- | Agent runtime mode.
data AgentMode
  = -- | Fresh agent process per message.
    OneShot
  | -- | One persistent process, fed addressed messages.
    Persistent
  deriving (Eq, Show)

-- | Configuration for an agent.
data AgentConfig = AgentConfig
  { cfgName :: String,
    cfgAgent :: String,
    cfgModel :: Maybe String,
    cfgProvider :: Maybe String
  }
  deriving (Show)

-- | Defaults for a hermes-based oneshot agent.
defaultAgentConfig :: AgentConfig
defaultAgentConfig =
  AgentConfig
    { cfgName = "agent",
      cfgAgent = "hermes",
      cfgModel = Nothing,
      cfgProvider = Nothing
    }

-- | Does the body address the named agent?
--
-- Matches @name:@ prefix, @ name:@ infix, @\@name@ prefix, or @ @name@ infix.
addressedTo :: Text -> Text -> Bool
addressedTo name body =
  let low = T.toLower body
      n = T.toLower name
   in T.isPrefixOf (n <> ":") low
        || (" " <> n <> ":") `T.isInfixOf` low
        || T.isPrefixOf ("@" <> n) low
        || (" @" <> n) `T.isInfixOf` low

-- | Strip the addressing prefix from a message body.
stripAddress :: Text -> Text -> Text
stripAddress name body =
  let n = T.toLower name
      low = T.toLower body
      stripAt i = T.strip $ T.dropWhile (== ' ') $ T.drop i body
      tryPrefix prefix =
        let p = prefix <> ":"
         in case T.breakOn p low of
              (_, r)
                | not (T.null r) ->
                    let i = T.length body - T.length r + T.length p
                     in Just (stripAt i)
              _ -> Nothing
      tryAt =
        let at = "@" <> n
         in case T.breakOn at low of
              (_, r)
                | not (T.null r) ->
                    let i = T.length body - T.length r + T.length at
                     in Just (T.strip $ T.drop i body)
              _ -> Nothing
   in case tryPrefix n of
        Just rest -> rest
        Nothing -> case tryAt of
          Just rest -> rest
          Nothing -> T.strip body

-- | Build the hermes 'Cli' recipe for an agent config.
--
-- Model/provider resolution: config field, else @HERMES_MODEL@ /
-- @HERMES_PROVIDER@ environment, else deepseek defaults.  The session file
-- lives in the agent's config dir.
agentCli :: AgentConfig -> IO Cli
agentCli cfg = do
  homeModel <- lookupEnv "HERMES_MODEL"
  homeProv <- lookupEnv "HERMES_PROVIDER"
  let model = fromMaybe (fromMaybe "deepseek-v4-pro" homeModel) (cfgModel cfg)
      provider = fromMaybe (fromMaybe "deepseek" homeProv) (cfgProvider cfg)
  dir <- Config.agentSessionDir (cfgName cfg)
  pure (hermesCli (Just (T.pack model)) (Just (T.pack provider)) (dir </> "session"))

-- | Run the configured oneshot agent against a prompt.
--
-- First call creates a session; subsequent calls resume it by session id so
-- context persists across dispatches.  If the stored session disappears, we
-- fall back to a fresh session and record the new id.
agentQuery :: AgentConfig -> Text -> IO Text
agentQuery cfg prompt = do
  cli <- agentCli cfg
  cliQuery cli prompt

-- | Oneshot CLI agent (hermes by default) as a list 'Shard'.
--
-- Session file and process stay inside @IO@ — apply-only at this boundary.
-- @who@ is the agent nick (from on emitted posts).
oneshotShard :: AgentConfig -> Text -> IO (Shard IO [Post Text] [Post Text])
oneshotShard cfg who = do
  cli <- agentCli cfg
  cliShard who cli

-- | One closed shard turn: commit @ins@, emit replies.
runShard :: Shard IO [Post Text] [Post Text] -> [Post Text] -> IO [Post Text]
runShard = runShardIO
