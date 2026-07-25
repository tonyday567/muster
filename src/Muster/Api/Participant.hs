{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Participant layer for the rebuilt muster API.
--
-- This layer tracks the caller's identity and current channel under a bus root,
-- and exposes the commands a human (or daemon) uses on the bus: name, join,
-- leave, post, read-next and read-tail.
module Muster.Api.Participant
  ( -- * Identity
    setName,
    currentName,

    -- * Channel membership
    joinChannel,
    leaveChannel,
    currentChannel,

    -- * Messaging
    post,
    readNext,
    readTail,

    -- * Shared name registry (also used by agents)
    claimName,
    releaseName,
    renameName,
    readUsedNames,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muster.Api.Bus qualified as Bus
import Muster.Api.Types (BusRoot (..), Channel (..), Nick (..), validateNick)
import Muster.Bus qualified as BusEngine
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import Prelude

-- $setup
-- >>> :set -XOverloadedStrings

-- | Path to the caller's nick file.
--
-- >>> mePath (BusRoot "/tmp/muster")
-- "/tmp/muster/.me"
mePath :: BusRoot -> FilePath
mePath root = Bus.busRootPath root </> ".me"

-- | Path to the caller's current-channel file.
--
-- >>> channelStatePath (BusRoot "/tmp/muster")
-- "/tmp/muster/.channel"
channelStatePath :: BusRoot -> FilePath
channelStatePath root = Bus.busRootPath root </> ".channel"

-- | Path to the simple file-backed used-names list.
--
-- >>> usedNamesPath (BusRoot "/tmp/muster")
-- "/tmp/muster/.used"
usedNamesPath :: BusRoot -> FilePath
usedNamesPath root = Bus.busRootPath root </> ".used"

-- | Read the used-names set from disk.
readUsedNames :: BusRoot -> IO (Set Nick)
readUsedNames root = do
  let path = usedNamesPath root
  exists <- doesFileExist path
  if not exists
    then pure Set.empty
    else do
      raw <- TIO.readFile path
      pure $ Set.fromList $ map Nick $ filter (not . T.null) $ T.lines raw

-- | Write the used-names set to disk.
writeUsedNames :: BusRoot -> Set Nick -> IO ()
writeUsedNames root names = do
  let path = usedNamesPath root
      body = T.unlines $ map unNick $ Set.toList names
  TIO.writeFile path body

-- | Add a name to the used list. Returns 'False' if it was already present.
claimName :: BusRoot -> Nick -> IO Bool
claimName root nick = do
  used <- readUsedNames root
  if nick `Set.member` used
    then pure False
    else do
      writeUsedNames root (Set.insert nick used)
      pure True

-- | Remove a name from the used list.
releaseName :: BusRoot -> Nick -> IO ()
releaseName root nick = do
  used <- readUsedNames root
  writeUsedNames root (Set.delete nick used)

-- | Replace one name with another in the used list.
renameName :: BusRoot -> Nick -> Nick -> IO Bool
renameName root old new
  | old == new = pure True
  | otherwise = do
      used <- readUsedNames root
      if new `Set.member` used
        then pure False
        else do
          writeUsedNames root (Set.insert new $ Set.delete old used)
          pure True

-- | Set the caller's nick.
--
-- The name is validated and checked against the used-names list. If the caller
-- already has a different nick, the old nick is released and the new one
-- claimed.
setName :: BusRoot -> Text -> IO (Either Text Nick)
setName root raw = do
  case validateNick raw of
    Left err -> pure (Left err)
    Right nick -> do
      mOld <- currentName root
      ok <- case mOld of
        Nothing -> claimName root nick
        Just old -> renameName root old nick
      if not ok
        then pure $ Left $ "name " <> unNick nick <> " is already in use"
        else do
          TIO.writeFile (mePath root) (unNick nick <> "\n")
          pure (Right nick)

-- | Read the caller's current nick, if any.
currentName :: BusRoot -> IO (Maybe Nick)
currentName root = do
  let path = mePath root
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      raw <- T.strip <$> TIO.readFile path
      pure $
        if T.null raw
          then Nothing
          else case validateNick raw of
            Left _ -> Nothing
            Right nick -> Just nick

-- | Read the caller's current channel, if any.
currentChannel :: BusRoot -> IO (Maybe Channel)
currentChannel root = do
  let path = channelStatePath root
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      raw <- T.strip <$> TIO.readFile path
      pure $ if T.null raw then Nothing else Just (Channel raw)

-- | Join a channel as the current nick.
--
-- The caller must have set a name. A join cursor is created at the current end
-- of the channel log so that 'readNext' only sees new traffic.
joinChannel :: BusRoot -> Channel -> IO (Either Text ())
joinChannel root chan = do
  Bus.ensureBusRoot root
  mName <- currentName root
  case mName of
    Nothing -> pure $ Left "set a name first (muster name <nick>)"
    Just name -> do
      _ <- Bus.ensureChannel root chan
      let dir = Bus.channelPath root chan
          cursor = Bus.cursorPath root chan name
      total <- BusEngine.countLogLines dir
      writeFile cursor (show total <> "\n")
      void $ claimName root name
      TIO.writeFile (channelStatePath root) (unChannel chan <> "\n")
      pure (Right ())

-- | Leave the current channel and release the caller's name.
leaveChannel :: BusRoot -> IO ()
leaveChannel root = do
  mName <- currentName root
  mChan <- currentChannel root
  case (mName, mChan) of
    (Just name, Just chan) -> do
      let cursor = Bus.cursorPath root chan name
      void $ try @SomeException $ removeFile cursor
      releaseName root name
      void $ try @SomeException $ removeFile (channelStatePath root)
    _ -> pure ()

-- | Post a message on the current channel as the current nick.
post :: BusRoot -> Text -> IO (Either Text ())
post root body = run $ do
  name <- requireName root
  chan <- requireChannel root
  Bus.post root chan name body

-- | Read unread messages on the current channel since the caller's cursor.
readNext :: BusRoot -> IO (Either Text [Text])
readNext root = run $ do
  name <- requireName root
  chan <- requireChannel root
  Bus.readNext root chan name

-- | Read the last @n@ lines of the current channel.
readTail :: BusRoot -> Int -> IO (Either Text [Text])
readTail root n = run $ do
  name <- requireName root
  chan <- requireChannel root
  Bus.readTail root chan name n

requireName :: BusRoot -> IO Nick
requireName root = do
  m <- currentName root
  case m of
    Just n -> pure n
    Nothing -> fail "set a name first (muster name <nick>)"

requireChannel :: BusRoot -> IO Channel
requireChannel root = do
  m <- currentChannel root
  case m of
    Just c -> pure c
    Nothing -> fail "join a channel first (muster join <channel>)"

run :: IO a -> IO (Either Text a)
run action = do
  r <- try @SomeException action
  pure $ case r of
    Left e -> Left (T.pack (show e))
    Right a -> Right a
