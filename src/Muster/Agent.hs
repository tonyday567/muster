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

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getEnv, lookupEnv)
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
          "-m", shellQuote model,
          "--provider", shellQuote provider,
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
    fail $ "agent failed (code " <> show code <> "): " <> T.unpack (T.take 200 out)
  updateSessionFile sessionFile out
  pure $ cleanAgentOut out

updateSessionFile :: FilePath -> Text -> IO ()
updateSessionFile path out =
  case parseSessionId out of
    Nothing -> pure ()
    Just sid -> writeStoredSession path sid

agentSessionFile :: AgentConfig -> IO FilePath
agentSessionFile cfg = do
  home <- getEnv "HOME"
  pure (home </> ".config/muster" </> "agents" </> cfgName cfg </> "session")

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
