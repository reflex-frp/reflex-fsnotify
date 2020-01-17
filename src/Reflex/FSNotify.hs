{-|
Module: Reflex.FSNotify
Description: Watch for filesystem changes in reflex
-}

{-# LANGUAGE FlexibleContexts #-}
module Reflex.FSNotify
  ( watchDirectory
  , watchDir
  , watchTree
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Reflex
import qualified System.FSNotify as FS

-- A type synonym to disambiguate Reflex 'Event's from 'System.FSNotify.Event'
type FSEvent = FS.Event

-- | Watch a directory for changes
{-# DEPRECATED watchDirectory "Use `watchDir cfg path (const True)` instead" #-}
watchDirectory
  :: (Reflex t, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => FS.WatchConfig
  -> Event t FilePath
  -> m (Event t FS.Event)
watchDirectory cfg path = watchDir cfg path (const True)

wrapWatch
  :: (Reflex t, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => (FS.WatchManager -> FilePath -> FS.Action -> IO a)
  -> FS.WatchConfig
  -> Event t FilePath
  -> m (Event t FSEvent)
wrapWatch f cfg path =
  performEventAsync $ ffor path $ \p cb -> liftIO $ void $ forkIO $
    FS.withManagerConf cfg $ \mgr -> do
      _ <- f mgr p cb -- FS.watchTree mgr p (const True) cb
      forever $ threadDelay 1000000

watchDir
  :: (Reflex t, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => FS.WatchConfig
  -> Event t FilePath
  -> FS.ActionPredicate
  -> m (Event t FSEvent)
watchDir cfg path evFilter = wrapWatch (\mgr p -> FS.watchDir mgr p evFilter) cfg path

watchTree
  :: (Reflex t, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => FS.WatchConfig
  -> Event t FilePath
  -> FS.ActionPredicate
  -> m (Event t FSEvent)
watchTree cfg path evFilter = wrapWatch (\mgr p -> FS.watchTree mgr p evFilter) cfg path
