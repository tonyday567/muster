{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Data.Text qualified as T
import Muster.Api.Agent qualified as Ag
import Muster.Api.Bus qualified as Bus
import Muster.Api.Orchestrator qualified as Orc
import Muster.Api.Participant qualified as Part
import Muster.Api.Types (BusRoot (..), Channel (..), Nick (..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
  testGroup "muster"
    [ testCase "central bus relays posts" $ do
        withSystemTempDirectory "muster-test" $ \tmp -> do
          let root = BusRoot tmp
          Bus.withBus root $ \_ -> do
            alive <- Bus.waitForChannel root (Channel "bus")
            alive @?= True
            Bus.post root (Channel "bus") (Nick "kimi") ("hello bus" :: Text)
            Bus.post root (Channel "bus") (Nick "kimi") ("second line" :: Text)
            threadDelay 200_000
            ls <- Bus.readTail root (Channel "bus") (Nick "reader") 10
            length ls @?= 2,
      testCase "participant layer round-trip" $ do
        withSystemTempDirectory "muster-test" $ \tmp -> do
          let root = BusRoot tmp
          Bus.withBus root $ \_ -> do
            Part.setName root "kimi" >>= (@?= Right (Nick "kimi"))
            Part.joinChannel root (Channel "bus") >>= (@?= Right ())
            Part.post root "hello" >>= (@?= Right ())
            Part.post root "world" >>= (@?= Right ())
            threadDelay 200_000
            Part.readTail root 10 >>= \case
              Right ls -> length ls @?= 2
              Left err -> assertFailure (T.unpack err)
            Part.leaveChannel root
            Part.currentChannel root >>= (@?= Nothing),
      testCase "agent lifecycle start/stop/quit" $ do
        withSystemTempDirectory "muster-test" $ \tmp -> do
          let root = BusRoot tmp
          Bus.withBus root $ \_ -> do
            Right hermes <- Ag.newAgent root (Just "hermes")
            Ag.writeAgentConfig root hermes (Ag.AgentConfig "sleep" ["1000"])
            Ag.startAgent root hermes >>= (@?= Right ())
            Ag.agentRunning root hermes >>= (@?= True)
            Ag.stopAgent root hermes >>= (@?= Right ())
            Ag.agentRunning root hermes >>= (@?= False)
            Ag.quitAgent root hermes >>= (@?= Right ())
            Ag.agentState root hermes >>= (@?= Nothing),
      testCase "agent auto-name picks from pool" $ do
        withSystemTempDirectory "muster-test" $ \tmp -> do
          let root = BusRoot tmp
          Bus.withBus root $ \_ -> do
            Right n1 <- Ag.newAgent root Nothing
            n1 @?= Nick "fable"
            Right n2 <- Ag.newAgent root Nothing
            n2 @?= Nick "grok"
            Right n3 <- Ag.newAgent root (Just "kimi")
            n3 @?= Nick "kimi"
            Right n4 <- Ag.newAgent root Nothing
            n4 @?= Nick "claude"
            agents <- Ag.listAgents root
            length agents @?= 4,
      testCase "orchestrator ps and tell" $ do
        withSystemTempDirectory "muster-test" $ \tmp -> do
          let root = BusRoot tmp
          Bus.withBus root $ \_ -> do
            Right hermes <- Ag.newAgent root (Just "hermes")
            Ag.writeAgentConfig root hermes (Ag.AgentConfig "sleep" ["1000"])
            Ag.startAgent root hermes >>= (@?= Right ())
            states <- Orc.ps root
            length states @?= 1
            Part.setName root "kimi" >>= (@?= Right (Nick "kimi"))
            Part.joinChannel root (Channel "bus") >>= (@?= Right ())
            Orc.tell root (Nick "hermes") "check this" >>= (@?= Right ()),
      testCase "placeholder" $ True @?= True
    ]
