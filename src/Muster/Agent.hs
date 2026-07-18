{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Agent utilities: address matching and oneshot external-agent queries.
module Muster.Agent
  ( AgentMode (..),
    AgentConfig (..),
    defaultAgentConfig,
    addressedTo,
    stripAddress,
    agentQuery,
  )
where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (readCreateProcessWithExitCode, shell)
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
              (_, r) | not (T.null r) ->
                let i = T.length body - T.length r + T.length p
                 in Just (stripAt i)
              _ -> Nothing
      tryAt =
        let at = "@" <> n
         in case T.breakOn at low of
              (_, r) | not (T.null r) ->
                let i = T.length body - T.length r + T.length at
                 in Just (T.strip $ T.drop i body)
              _ -> Nothing
   in case tryPrefix n of
        Just rest -> rest
        Nothing -> case tryAt of
          Just rest -> rest
          Nothing -> T.strip body

-- | Run the configured oneshot agent command against a prompt.
--
-- Invoked as @\<agent\> chat -q \<prompt\>@, attempting session continuation
-- first and falling back to a fresh session.
agentQuery :: AgentConfig -> Text -> IO Text
agentQuery cfg prompt = do
  homeModel <- lookupEnv "HERMES_MODEL"
  homeProv <- lookupEnv "HERMES_PROVIDER"
  let model = fromMaybe (fromMaybe "deepseek-v4-pro" homeModel) (cfgModel cfg)
      provider = fromMaybe (fromMaybe "deepseek" homeProv) (cfgProvider cfg)
      session = cfgName cfg <> "-session"
      q = shellQuote (T.unpack prompt)
      baseArgs =
        [ cfgAgent cfg <> " chat -q",
          q,
          "-m", shellQuote model,
          "--provider", shellQuote provider,
          "--yolo -Q --max-turns 90"
        ]
      continueArgs = baseArgs <> ["--continue", shellQuote session]
      runAgent args = do
        let cmd = unwords ("export PATH=\"$HOME/.local/bin:$PATH\";" : args)
        (code, out, err) <- readCreateProcessWithExitCode (shell cmd) ""
        pure (code, T.pack out <> T.pack err)
  (code1, out1) <- runAgent continueArgs
  let missing =
        "No session found matching" `T.isInfixOf` out1
          || code1 /= ExitSuccess
  if missing
    then do
      (code2, out2) <- runAgent baseArgs
      when (code2 /= ExitSuccess) $
        fail $ "agent failed (code " <> show code2 <> "): " <> T.unpack (T.take 200 out2)
      pure $ cleanAgentOut out2
    else pure $ cleanAgentOut out1

shellQuote :: String -> String
shellQuote s = "'" <> concatMap esc s <> "'"
  where
    esc '\'' = "'\\''"
    esc c = [c]

cleanAgentOut :: Text -> Text
cleanAgentOut =
  T.unlines
    . filter keep
    . map T.strip
    . T.lines
  where
    keep l
      | T.null l = False
      | "session_id:" `T.isPrefixOf` l = False
      | "Warning:" `T.isPrefixOf` l = False
      | "Resumed session" `T.isInfixOf` l = False
      | "Reached maximum" `T.isInfixOf` l = False
      | "Requesting summary" `T.isInfixOf` l = False
      | "No session found matching" `T.isInfixOf` l = False
      | "Use 'hermes sessions list'" `T.isInfixOf` l = False
      | otherwise = True
