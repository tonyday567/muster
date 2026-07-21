{-# LANGUAGE OverloadedStrings #-}

-- | Centralised configuration paths for muster.
--
-- All filesystem layout defaults live here so that individual modules do not
-- hard-code paths. The configuration home is @~/.config/muster@.
module Muster.Config
  ( musterHome,
    connectorSessionDir,
    agentSessionDir,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath (takeBaseName, (</>))
import Prelude

-- | Configuration home: @~/.config/muster@.
musterHome :: IO FilePath
musterHome = (</> ".config/muster") <$> getHomeDirectory

-- | Session directory for a persistent connector process.
--
-- Layout: @~/.config/muster/sessions/<project-base>-<channel>-<name>@
connectorSessionDir :: Text -> String -> FilePath -> IO FilePath
connectorSessionDir name channel project = do
  home <- musterHome
  let session = takeBaseName project <> "-" <> channel <> "-" <> T.unpack name
      dir = home </> "sessions" </> "connector" </> session
  createDirectoryIfMissing True dir
  pure dir

-- | Agent session state directory.
--
-- Layout: @~/.config/muster/agents/<name>@
agentSessionDir :: String -> IO FilePath
agentSessionDir name = do
  home <- musterHome
  let dir = home </> "agents" </> name
  createDirectoryIfMissing True dir
  pure dir
