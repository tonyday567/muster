{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Agent utilities: address matching and living 'Shard' adapters.
--
-- Evaluate is a circuits-agent 'Shard': commit @[Post]@, emit @[Post]@.
-- @oneshotShard@ is one adapter (CLI per eval + session file opacity).
-- Multi-round dialogue is repeated 'runShard', not a different seat type.
-- This is not a rewrite of the bus — only the evaluate step is seated.
module Muster.Agent
  ( AgentMode (..),
    AgentConfig (..),
    defaultAgentConfig,
    addressedTo,
    stripAddress,
    agentQuery,

    -- * Living agent as Shard
    queryShard,
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

import Circuit.Agent (Ends (..), Post (..), Shard, close, shard)
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.IORef (atomicModifyIORef', newIORef, writeIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muster.Config qualified as Config
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
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

-- | Run the configured oneshot agent command against a prompt.
--
-- Invoked as @\<agent\> chat -q \<prompt\>@.  The first call creates a Hermes
-- session; subsequent calls resume it by session id so context persists across
-- dispatches.  If the stored session disappears, we fall back to a fresh
-- session and record the new id.
agentQuery :: AgentConfig -> Text -> IO Text
agentQuery cfg prompt = do
  homeModel <- lookupEnv "HERMES_MODEL"
  homeProv <- lookupEnv "HERMES_PROVIDER"
  sessionFile <- agentSessionFile cfg
  let model = fromMaybe (fromMaybe "deepseek-v4-pro" homeModel) (cfgModel cfg)
      provider = fromMaybe (fromMaybe "deepseek" homeProv) (cfgProvider cfg)
      q = shellQuote (T.unpack prompt)
      baseArgs =
        [ cfgAgent cfg <> " chat -q",
          q,
          "-m",
          shellQuote model,
          "--provider",
          shellQuote provider,
          "--yolo -Q --max-turns 90"
        ]
      resumeArgs sid = baseArgs <> ["--resume", shellQuote sid]
      runAgent args = do
        let cmd = unwords ("export PATH=\"$HOME/.local/bin:$PATH\";" : args)
        (code, out, err) <- readCreateProcessWithExitCode (shell cmd) ""
        pure (code, T.pack out <> T.pack err)
  mSid <- readStoredSession sessionFile
  case mSid of
    Nothing -> runFresh sessionFile runAgent baseArgs
    Just sid -> do
      (code, out) <- runAgent (resumeArgs sid)
      let stale =
            code /= ExitSuccess
              || "No session found matching" `T.isInfixOf` out
              || "Session not found" `T.isInfixOf` out
      if stale
        then runFresh sessionFile runAgent baseArgs
        else do
          updateSessionFile sessionFile out
          pure $ cleanAgentOut out

runFresh :: FilePath -> ([String] -> IO (ExitCode, Text)) -> [String] -> IO Text
runFresh sessionFile runAgent baseArgs = do
  (code, out) <- runAgent baseArgs
  when (code /= ExitSuccess) $
    fail $
      "agent failed (code " <> show code <> "): " <> T.unpack (T.take 200 out)
  updateSessionFile sessionFile out
  pure $ cleanAgentOut out

updateSessionFile :: FilePath -> Text -> IO ()
updateSessionFile path out =
  case parseSessionId out of
    Nothing -> pure ()
    Just sid -> writeStoredSession path sid

agentSessionFile :: AgentConfig -> IO FilePath
agentSessionFile cfg = do
  dir <- Config.agentSessionDir (cfgName cfg)
  pure (dir </> "session")

readStoredSession :: FilePath -> IO (Maybe String)
readStoredSession path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      res <- try @SomeException (TIO.readFile path)
      pure case res of
        Left _ -> Nothing
        Right t ->
          let sid = T.unpack (T.strip t)
           in if null sid then Nothing else Just sid

writeStoredSession :: FilePath -> String -> IO ()
writeStoredSession path sid = do
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path (T.pack sid)

parseSessionId :: Text -> Maybe String
parseSessionId out =
  case filter ("session_id:" `T.isPrefixOf`) (T.lines out) of
    (line : _) ->
      let sid = T.strip (T.drop (T.length "session_id:") line)
       in if T.null sid then Nothing else Just (T.unpack sid)
    _ -> Nothing

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
      | "Resume this session with:" `T.isInfixOf` l = False
      | "Shutting down" `T.isInfixOf` l = False
      | "Session:" `T.isPrefixOf` l = False
      | "Duration:" `T.isPrefixOf` l = False
      | "Messages:" `T.isPrefixOf` l = False
      | "⚕" `T.isPrefixOf` l = False
      | "❯" `T.isPrefixOf` l = False
      | T.any (== '\x1b') l = False
      | isDecorative l = False
      | otherwise = True
    isDecorative t =
      let ok c =
            c == ' '
              || c == '\r'
              || c == '─'
              || c == '│'
              || c == '┌'
              || c == '┐'
              || c == '└'
              || c == '┘'
       in T.all ok t

-- ---------------------------------------------------------------------------
-- Living agent as Shard IO [Post] (seat, not rewrite)
-- ---------------------------------------------------------------------------

-- | Session assembly for the opaque seat: bodies, oldest-first, one per line.
--
-- This is the discoverable side of the boundary (data).  How hermes folds it
-- is not.
sessionPrompt :: [Post] -> Text
sessionPrompt = T.intercalate "\n" . map body

-- | Build reply posts from a cleaned agent response.
--
-- Addresses the last input's sender and preserves any other names on the
-- original wire (e.g. the bus channel).  Empty reply → no posts (quiet).
replyPosts :: Text -> [Post] -> Text -> [Post]
replyPosts who ins reply =
  case (listToMaybe (reverse ins), T.strip reply) of
    (_, r) | T.null r -> []
    (Nothing, _) -> []
    (Just lastIn, r) ->
      [ Post
          { from = who,
            to = from lastIn : filter (/= who) (to lastIn),
            body = r
          }
      ]

-- | Opaque evaluate seat: any @Text -> IO Text@ behind list ends.
--
-- Commit assembles a session prompt from the input posts; emit is
-- 'replyPosts' of the query result (empty = quiet).  Used by 'oneshotShard'
-- (hermes) and 'echoShard' (mock).
queryShard :: Text -> (Text -> IO Text) -> IO (Shard IO [Post])
queryShard who query = do
  outbox <- newIORef []
  pure $
    shard
      ( \ins ->
          if null ins
            then writeIORef outbox []
            else do
              reply <- query (sessionPrompt ins)
              writeIORef outbox (replyPosts who ins reply)
      )
      (atomicModifyIORef' outbox (\os -> ([], os)))

-- | Oneshot CLI agent (hermes by default) as a list 'Shard'.
--
-- Session file and process stay inside @IO@ — apply-only at this boundary.
-- @who@ is the agent nick (from on emitted posts).
oneshotShard :: AgentConfig -> Text -> IO (Shard IO [Post])
oneshotShard cfg who = queryShard who (agentQuery cfg)

-- | Mock seat: reply body is the session prompt (echo).
--
-- Demonstrates the living-agent path without hermes.
echoShard :: Text -> IO (Shard IO [Post])
echoShard who = queryShard who pure

-- | One closed shard turn: commit @ins@, emit replies.
runShard :: Shard IO [Post] -> [Post] -> IO [Post]
runShard sh ins =
  runKleisli (close (conjoint sh) (companion sh)) ins
