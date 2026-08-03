{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | L4b smoke: runner-protocol harness on the live bus.
--
-- Reads an agent card (harness/protocol/task/output/readings), drives a
-- lead/coder pair on a fresh channel, and asserts the dialogue follows the
-- runner oneliner grammar.  Default agents are deterministic mocks; pass
-- @--agent kimi@ (or @grok@ / @hermes@) to use a real CLI seat if it is
-- installed.
module Main (main) where

import Circuit.Agent.Cli (grokCli, hermesCli)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Free.Agent.Host (Host, cliHost, hostRun, kimiHost)
import Muster.Api.Bus
  ( ensureChannel,
    post,
    readNext,
    withBus,
  )
import Muster.Api.Types (BusRoot (..), Channel (..), Nick (..))
import Muster.Runner.Card (cardSystemPrompt, parseCard)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, findExecutable, removePathForcibly)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Prelude

-- ---------------------------------------------------------------------------
-- CLI
-- ---------------------------------------------------------------------------

data Opts = Opts
  { optCard :: FilePath,
    optChannel :: Text,
    optRoot :: FilePath,
    optAlpha :: Text,
    optBeta :: Text,
    optAgent :: Text,
    optRounds :: Int,
    optOutput :: Maybe FilePath
  }
  deriving (Show)

optsParser :: Parser Opts
optsParser =
  Opts
    <$> strOption
      ( long "card"
          <> short 'c'
          <> metavar "FILE"
          <> value "buff/runner-protocol.md"
          <> showDefault
          <> help "Agent card (markdown with harness/protocol/task/output/readings sections)"
      )
    <*> strOption
      ( long "channel"
          <> metavar "NAME"
          <> value "runner-smoke"
          <> showDefault
          <> help "Muster channel for the session"
      )
    <*> strOption
      ( long "root"
          <> metavar "DIR"
          <> value "/tmp/muster-runner-smoke"
          <> showDefault
          <> help "Bus root directory"
      )
    <*> strOption
      ( long "alpha"
          <> metavar "NAME"
          <> value "alpha"
          <> showDefault
          <> help "Name of the lead agent"
      )
    <*> strOption
      ( long "beta"
          <> metavar "NAME"
          <> value "beta"
          <> showDefault
          <> help "Name of the reviewer agent"
      )
    <*> strOption
      ( long "agent"
          <> metavar "mock|kimi|grok|hermes"
          <> value "mock"
          <> showDefault
          <> help "Agent backend to use for both seats"
      )
    <*> option
      auto
      ( long "rounds"
          <> metavar "N"
          <> value 3
          <> showDefault
          <> help "Maximum rounds"
      )
    <*> optional
      ( strOption
          ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Write the final landed message to FILE"
          )
      )

optsInfo :: ParserInfo Opts
optsInfo =
  info
    (optsParser <**> helper)
    ( fullDesc
        <> progDesc "Runner-protocol smoke test on the muster bus"
        <> header "muster-runner-smoke — lead/coder pair over runner marks"
    )

-- ---------------------------------------------------------------------------
-- Seats: mock or live CLI
-- ---------------------------------------------------------------------------

data Seat = MockSeat Text | LiveSeat (Host IO)

validMarks :: [Text]
validMarks = ["🟣", "🟡", "🟢", "🔴", "🔵", "✓", "↩"]

markOf :: Text -> Maybe Text
markOf t = case T.words t of
  (w : _) | w `elem` validMarks -> Just w
  _ -> Nothing

isLanded :: Text -> Bool
isLanded t = case markOf t of
  Just "🟢" -> True
  _ -> False

isStuck :: Text -> Bool
isStuck t = case markOf t of
  Just "🔴" -> True
  _ -> False

-- | Resolve a seat by kind.  If a live CLI is requested but not installed,
-- fall back to a mock seat with a warning.
resolveSeat :: Text -> Text -> FilePath -> IO Seat
resolveSeat kind name sessionFile = do
  case T.toLower kind of
    "mock" -> pure (MockSeat name)
    "kimi" -> findOrFallback "kimi" (kimiSeat sessionFile)
    "grok" -> findOrFallback "grok" (grokSeat sessionFile)
    "hermes" -> findOrFallback "hermes" (hermesSeat sessionFile)
    other -> do
      putStrLn $ "  WARNING unknown agent kind " <> T.unpack other <> ", using mock"
      pure (MockSeat name)
  where
    findOrFallback exe mk = do
      mPath <- findExecutable exe
      case mPath of
        Nothing -> do
          putStrLn $ "  WARNING " <> exe <> " not found; falling back to mock for " <> T.unpack name
          pure (MockSeat name)
        Just _ -> LiveSeat <$> mk
    kimiSeat sf = do
      mModel <- lookupEnv "KIMI_MODEL"
      pure (kimiHost name (T.pack <$> mModel) sf)
    grokSeat sf = do
      mModel <- lookupEnv "GROK_MODEL"
      pure (cliHost name (grokCli (T.pack <$> mModel) sf))
    hermesSeat sf = do
      mModel <- lookupEnv "HERMES_MODEL"
      mProv <- lookupEnv "HERMES_PROVIDER"
      pure (cliHost name (hermesCli (T.pack <$> mModel) (T.pack <$> mProv) sf))

-- | Deterministic mock replies for a 3-round design-review shape.
mockReply :: Text -> Int -> Text
mockReply name turn
  | name == "alpha" && turn == 1 =
      "🟡 design-review alpha: initial elab deck — seed, agents, synthesis"
  | name == "alpha" && turn == 3 =
      "🟢 design-review alpha: final edited card with amendments applied"
  | name == "beta" && turn == 2 =
      "↩ design-review beta: tighten the seed, ✓ synthesis section"
  | otherwise =
      "🔵 design-review " <> name <> ": standing by"

-- | Strong turn instruction placed immediately before the system prompt.
turnPrefix :: Text -> Text
turnPrefix name =
  T.unlines
    [ "It is " <> name <> "'s turn.",
      "Reply with exactly ONE line starting with a runner mark (" <> T.unwords validMarks <> ").",
      "No preamble, no bullets, no markdown fences.",
      ""
    ]

-- | Extract the first line that begins with a runner mark, after stripping
-- leading bullets or quote markers.
extractLine :: Text -> Text
extractLine r =
  case mapMaybe cleanLine (T.lines r) of
    (x : _) -> x
    [] -> T.strip (T.take 200 r)
  where
    bullets = ["• ", "- ", "* ", "> "]
    dropBullet t0 = go (T.stripStart t0) bullets
      where
        go t [] = t
        go t (b : bs) = go (fromMaybe t (T.stripPrefix b t)) bs
    cleanLine t =
      let t' = dropBullet t
       in if any (`T.isPrefixOf` t') validMarks then Just (T.strip t') else Nothing

-- ---------------------------------------------------------------------------
-- Runner loop
-- ---------------------------------------------------------------------------

assert :: String -> Bool -> IO ()
assert msg ok = unless ok $ do
  putStrLn ("  FAIL " <> msg)
  exitFailure

pass :: String -> IO ()
pass msg = putStrLn ("  PASS " <> msg)

-- | Run one turn for an agent.  Returns the oneliner posted to the bus and the
-- full agent response (which may be longer than the oneliner).
runTurn ::
  BusRoot ->
  Channel ->
  Text ->
  Seat ->
  Text ->
  [Text] ->
  Int ->
  IO (Text, Text)
runTurn root chan name seat systemPrompt conversation turn = do
  unread <- readNext root chan (Nick name)
  let conversation' = conversation ++ unread
      prompt =
        turnPrefix name
          <> systemPrompt
          <> "\n\nConversation so far:\n"
          <> T.unlines conversation'
  (full, line) <- case seat of
    MockSeat _ ->
      let r = mockReply name turn
       in pure (r, r)
    LiveSeat h -> do
      er <- try @SomeException (hostRun h [prompt])
      case er of
        Left e -> do
          let r = "🔴 " <> T.pack (show e)
          pure (r, r)
        Right [] -> pure ("🔴 empty response", "🔴 empty response")
        Right (r : _) -> pure (r, extractLine r)
  post root chan (Nick name) line
  pure (line, full)

-- | Drive alternating turns up to the round limit, stopping early on 🟢.
runRounds ::
  BusRoot ->
  Channel ->
  Text ->
  Text ->
  Seat ->
  Seat ->
  Text ->
  Int ->
  IO (Maybe Text)
runRounds root chan alpha beta alphaSeat betaSeat systemPrompt maxRounds = go 1 []
  where
    go n conv
      | n > maxRounds = do
          pass ("reached round limit " <> show maxRounds)
          pure Nothing
      | otherwise = do
          (aLine, aFull) <- runTurn root chan alpha alphaSeat systemPrompt conv (2 * n - 1)
          assert ("alpha turn " <> show n <> " has a runner mark") (markOf aLine /= Nothing)
          let convA = conv ++ ["[" <> alpha <> "]: " <> aLine]
          when (isStuck aLine) $ do
            putStrLn ("  STOP alpha signalled stuck: " <> T.unpack aLine)
            pure ()
          if isLanded aLine
            then do
              pass "alpha landed the card"
              pure (Just aFull)
            else do
              (bLine, bFull) <- runTurn root chan beta betaSeat systemPrompt convA (2 * n)
              assert ("beta turn " <> show n <> " has a runner mark") (markOf bLine /= Nothing)
              let convB = convA ++ ["[" <> beta <> "]: " <> bLine]
              when (isStuck bLine) $
                putStrLn ("  STOP beta signalled stuck: " <> T.unpack bLine)
              if isLanded bLine
                then do
                  pass "beta landed the card"
                  pure (Just bFull)
                else go (n + 1) convB

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  Opts {..} <- execParser optsInfo
  raw <- TIO.readFile optCard
  let card = parseCard raw
      systemPrompt = cardSystemPrompt card
      root = BusRoot optRoot
      chan = Channel optChannel
      alphaSession = optRoot </> T.unpack optAlpha <> "-session"
      betaSession = optRoot </> T.unpack optBeta <> "-session"

  putStrLn "muster-runner-smoke: L4b runner-protocol frame"
  putStrLn $ "  card: " <> optCard
  putStrLn $ "  channel: " <> T.unpack optChannel
  putStrLn $ "  agents: " <> T.unpack optAgent

  -- Wipe any stale session state so each smoke run starts from a clean bus.
  _ <- try @SomeException (removePathForcibly (unBusRoot root))
  createDirectoryIfMissing True (unBusRoot root)

  alphaSeat <- resolveSeat optAgent optAlpha alphaSession
  betaSeat <- resolveSeat optAgent optBeta betaSession

  withBus root $ \_ -> do
    _ <- ensureChannel root chan
    -- Let the relay attach before any posts.
    threadDelay 300_000

    post root chan (Nick "runner") ("session start: " <> T.pack optCard)

    mFinal <- runRounds root chan optAlpha optBeta alphaSeat betaSeat systemPrompt optRounds

    case mFinal of
      Nothing -> do
        pass "runner session completed"
      Just final -> do
        pass "runner session landed"
        case optOutput of
          Nothing -> pure ()
          Just path -> do
            TIO.writeFile path final
            pass ("wrote landed message to " <> path)

  putStrLn "muster-runner-smoke: PASS"
