{-# LANGUAGE OverloadedStrings #-}

-- | Shared optparse-applicative options for muster CLIs and faces.
--
-- One vocabulary for channel, identity name, and bus root — used by the
-- main @muster@ CLI, @muster-ws@ (deck), and anything else that attaches
-- to a bus.
module Muster.Cli.Opts
  ( -- * Parsers (same flags everywhere)
    busRootOpt,
    channelOpt,
    nameOpt,
    portOpt,
    hostOpt,
    historyOpt,
    boardOpt,
    devOpt,
    devPathOpt,

    -- * Resolve
    resolveBusRoot,
    defaultBusRoot,
  )
where

import Options.Applicative
import System.Directory (getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Prelude

-- | @--bus-root DIR@ — empty means default home layout.
busRootOpt :: Parser FilePath
busRootOpt =
  strOption
    ( long "bus-root"
        <> value ""
        <> showDefault
        <> metavar "DIR"
        <> help "Root directory for all bus state (default: $HOME/.config/muster)"
    )

-- | @-c/--channel CHANNEL@
channelOpt :: Parser String
channelOpt =
  strOption
    ( long "channel"
        <> short 'c'
        <> value "bus"
        <> showDefault
        <> metavar "CHANNEL"
        <> help "Muster channel"
    )

-- | @-n/--name NAME@ — bus identity (deck default @deck@).
nameOpt :: String -> Parser String
nameOpt def =
  strOption
    ( long "name"
        <> short 'n'
        <> value def
        <> showDefault
        <> metavar "NAME"
        <> help "Bus identity / nick"
    )

portOpt :: Int -> Parser Int
portOpt def =
  option
    auto
    ( long "port"
        <> short 'p'
        <> value def
        <> showDefault
        <> metavar "PORT"
        <> help "HTTP port"
    )

hostOpt :: Parser String
hostOpt =
  strOption
    ( long "host"
        <> value "127.0.0.1"
        <> showDefault
        <> metavar "HOST"
        <> help "HTTP bind host"
    )

historyOpt :: Parser Int
historyOpt =
  option
    auto
    ( long "history"
        <> value 80
        <> showDefault
        <> metavar "N"
        <> help "Log lines sent on WebSocket connect"
    )

boardOpt :: Parser FilePath
boardOpt =
  strOption
    ( long "board"
        <> value ""
        <> metavar "FILE"
        <> help "Board markdown path (default: $HOME/mg/loom/board.md)"
    )

devOpt :: Parser Bool
devOpt =
  switch
    ( long "dev"
        <> help "Serve deck.html from disk instead of embedded"
    )

devPathOpt :: Parser FilePath
devPathOpt =
  strOption
    ( long "dev-path"
        <> value "app/deck.html"
        <> showDefault
        <> metavar "FILE"
        <> help "Path to deck.html in --dev mode"
    )

defaultBusRoot :: IO FilePath
defaultBusRoot = do
  m <- lookupEnv "MUSTER_BUS_ROOT"
  case m of
    Just p | not (null p) -> pure p
    _ -> (</> ".config/muster") <$> getHomeDirectory

-- | Empty string → default bus root.
resolveBusRoot :: FilePath -> IO FilePath
resolveBusRoot explicit
  | not (null explicit) = pure explicit
  | otherwise = defaultBusRoot
