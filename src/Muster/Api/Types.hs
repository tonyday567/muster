{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core types for the rebuilt muster API.
--
-- The design is locked in @loom/muster-orchestrator.md@. This module holds the
-- pure, shareable definitions: names, channels, agent state, and the registry.
module Muster.Api.Types
  ( -- * Names and channels
    Nick (..),
    Channel (..),
    BusRoot (..),
    SessionId (..),
    validateNick,

    -- * Agent state
    AgentStatus (..),
    AgentState (..),

    -- * Registry
    Registry (..),
    emptyRegistry,
    claimNick,
    releaseNick,
    renameNick,
    isUsed,

    -- * Privacy
    Privacy (..),
    privacyOf,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (Permissions (..))
import System.FilePath (FilePath)
import Prelude

-- | A participant name: short, lowercase, no whitespace or brackets.
newtype Nick = Nick {unNick :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

-- | A channel name.
newtype Channel = Channel {unChannel :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

-- | Root directory for all muster state.
newtype BusRoot = BusRoot {unBusRoot :: FilePath}
  deriving stock (Eq, Ord, Show)

-- | Opaque session identifier for resuming an agent.
newtype SessionId = SessionId {unSessionId :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import System.Directory (Permissions (..), emptyPermissions)

-- | Validate a nick. Returns an error message or the cleaned nick.
--
-- >>> validateNick "kimi"
-- Right (Nick {unNick = "kimi"})
--
-- >>> validateNick "Tony Day"
-- Left "name cannot contain whitespace"
--
-- >>> validateNick "her[mes]"
-- Left "name cannot contain brackets"
validateNick :: Text -> Either Text Nick
validateNick name
  | T.null name = Left "name cannot be empty"
  | T.any isSpace name = Left "name cannot contain whitespace"
  | '[' `T.elem` name || ']' `T.elem` name = Left "name cannot contain brackets"
  | otherwise = Right (Nick name)
  where
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- | Runtime status of an agent.
data AgentStatus
  = -- | Agent process is running.
    Running
  | -- | Agent is halted but can be resumed.
    Stopped
  | -- | Agent has reported it does not want to be disturbed.
    Unavailable
  | -- | No recent heartbeat or process check.
    Unknown
  deriving stock (Eq, Ord, Show)

-- | State tracked for each dialed-up agent.
data AgentState = AgentState
  { agentName :: Nick,
    agentPid :: Maybe Int,
    agentSessionId :: Maybe SessionId,
    agentStatus :: AgentStatus,
    agentLastDone :: Maybe Text,
    agentCurrentDoing :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | In-memory registry of used names and agent states.
data Registry = Registry
  { usedNames :: Set Nick,
    agents :: Map Nick AgentState
  }
  deriving stock (Eq, Show)

-- | Empty registry.
emptyRegistry :: Registry
emptyRegistry = Registry Set.empty Map.empty

-- | Claim a nick in the registry. Fails if already used.
--
-- >>> let Right n = validateNick "kimi"
-- >>> claimNick n emptyRegistry
-- Right (Registry {usedNames = fromList [Nick {unNick = "kimi"}], agents = fromList []})
--
-- >>> let Right n = validateNick "kimi"
-- >>> claimNick n (Registry (Set.singleton n) Map.empty)
-- Left (Nick {unNick = "kimi"})
claimNick :: Nick -> Registry -> Either Nick Registry
claimNick n r
  | n `Set.member` usedNames r = Left n
  | otherwise = Right r {usedNames = Set.insert n (usedNames r)}

-- | Release a nick, removing any associated agent state.
--
-- >>> let Right n = validateNick "kimi"
-- >>> releaseNick n (Registry (Set.singleton n) Map.empty)
-- Registry {usedNames = fromList [], agents = fromList []}
releaseNick :: Nick -> Registry -> Registry
releaseNick n r =
  r
    { usedNames = Set.delete n (usedNames r),
      agents = Map.delete n (agents r)
    }

-- | Rename a nick in the registry. Fails if the old name is not used or the new
-- name is already taken.
--
-- >>> let Right old = validateNick "kimi"; Right new_ = validateNick "kimi2"
-- >>> renameNick old new_ (Registry (Set.singleton old) Map.empty)
-- Right (Registry {usedNames = fromList [Nick {unNick = "kimi2"}], agents = fromList []})
renameNick :: Nick -> Nick -> Registry -> Either Nick Registry
renameNick old new_ r
  | old `Set.notMember` usedNames r = Right r -- idempotent if old not present
  | new_ `Set.member` usedNames r && new_ /= old = Left new_
  | otherwise =
      let r' = releaseNick old r
       in Right r' {usedNames = Set.insert new_ (usedNames r')}

-- | Check whether a nick is currently used.
--
-- >>> let Right n = validateNick "kimi"
-- >>> isUsed n (Registry (Set.singleton n) Map.empty)
-- True
isUsed :: Nick -> Registry -> Bool
isUsed n r = n `Set.member` usedNames r

-- | Privacy level of a muster instance.
--
-- A bus root is private if the directory is not readable by others. The check
-- is coarse: it looks at filesystem permissions.
data Privacy = Public | Private
  deriving stock (Eq, Show)

-- | Determine privacy from directory permissions.
--
-- >>> privacyOf emptyPermissions {readable = True}
-- Public
--
-- >>> privacyOf emptyPermissions {readable = False}
-- Private
privacyOf :: Permissions -> Privacy
privacyOf p = if readable p then Public else Private
