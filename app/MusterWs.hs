{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | WebSocket (+ HTTP) face on a muster channel — thin Main over
-- 'Muster.Ws' (which carries the server; see there for the architecture).
--
-- @
--   cabal run muster-ws -- --name deck
--   open http://127.0.0.1:9162/
-- @
module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Muster.Cli.Opts
  ( boardOpt,
    busRootOpt,
    channelOpt,
    devOpt,
    devPathOpt,
    historyOpt,
    hostOpt,
    nameOpt,
    portOpt,
    resolveBusRoot,
  )
import Muster.Ws (WsConfig (..), mkApp)
import Network.Wai.Handler.Warp (run)
import Options.Applicative
import Control.Monad (when)
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import Prelude

-- | Deck HTML embedded at compile time so the binary is self-contained.
deckHtml :: LBS.ByteString
deckHtml = LBS.fromStrict $(embedFile "app/deck.html")

data Opts = Opts
  { optName :: String,
    optChannel :: String,
    optBusRoot :: FilePath,
    optHost :: String,
    optPort :: Int,
    optHistory :: Int,
    optBoard :: FilePath,
    optDev :: Bool,
    optDevPath :: FilePath
  }

-- | Same channel / name / bus-root vocabulary as @muster@ CLI ('Muster.Cli.Opts').
optsP :: Parser Opts
optsP =
  Opts
    <$> nameOpt "deck"
    <*> channelOpt
    <*> busRootOpt
    <*> hostOpt
    <*> portOpt 9162
    <*> historyOpt
    <*> boardOpt
    <*> devOpt
    <*> devPathOpt

main :: IO ()
main = do
  o <- execParser $ info (optsP <**> helper) (progDesc "HTTP+WS muster deck")
  root <- resolveBusRoot (optBusRoot o)
  boardPath <- resolveBoard (optBoard o)
  let fifo = root </> optChannel o </> "bus.fifo"
  ok <- doesFileExist fifo
  when (not ok) $
    error $
      "bus.fifo missing at " <> fifo <> " — run: muster bus start (channel " <> optChannel o <> ")"

  putStrLn $ "muster-ws: attaching as " <> optName o <> " on " <> optChannel o
  putStrLn $ "  bus-root " <> root
  putStrLn $ "  board    " <> boardPath
  putStrLn $ "  http     http://" <> optHost o <> ":" <> show (optPort o) <> "/"
  hFlush stdout

  app <-
    mkApp
      WsConfig
        { wcName = T.pack (optName o),
          wcChannel = optChannel o,
          wcBusRoot = root,
          wcHistory = optHistory o,
          wcBoard = boardPath,
          wcDeckHtml = deckHtml,
          wcDevPath = if optDev o then Just (optDevPath o) else Nothing
        }
  run (optPort o) app

resolveBoard :: FilePath -> IO FilePath
resolveBoard explicit
  | not (null explicit) = pure explicit
  | otherwise = (</> "coffee/loom/board.md") <$> getEnv "HOME"
