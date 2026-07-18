{-# LANGUAGE OverloadedStrings #-}

-- | Cabal repl as a muster channel participant.
--
-- Uses 'Muster.Connector' to keep a persistent @cabal repl@ process on the bus.
-- Commands are accepted when they mention @<agent-name>@ (default @repl@).
--
-- Usage:
-- @
--   cabal run cabal-repl-muster -- <project-dir> <channel> <agent-name>
-- @
--
-- Defaults: project @~/haskell/circuits@, channel @cabal-repl@, agent @repl@.
module Main (main) where

import Data.Text (Text)
import Data.Text qualified as T
import Muster.Connector (ConnectorConfig (..), defaultConnectorConfig, runConnector)
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Prelude

main :: IO ()
main = do
  args <- getArgs
  home <- getHomeDirectory
  let (project, channel, agent) = parseArgs home args
      cfg =
        (defaultConnectorConfig agent)
          { connChannel = channel,
            connProject = project,
            connCommand = "cabal",
            connArgs = ["repl"]
          }
  runConnector cfg

parseArgs :: FilePath -> [String] -> (FilePath, String, Text)
parseArgs home args =
  case args of
    [p, c, a] -> (p, c, T.pack a)
    [p, c] -> (p, c, "repl")
    [p] -> (p, "cabal-repl", "repl")
    [] -> (home </> "haskell" </> "circuits", "cabal-repl", "repl")
    _ -> (home </> "haskell" </> "circuits", "cabal-repl", "repl")
