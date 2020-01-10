{-|
Module: Reflex.FSNotify
Description: Watch for filesystem changes in reflex
-}

{-# LANGUAGE FlexibleContexts #-}
module Reflex.FSNotify where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Reflex
import qualified System.FSNotify as FS

-- | Watch a directory for changes
watchDirectory
  :: (Reflex t, TriggerEvent t m, PerformEvent t m, MonadIO (Performable m))
  => FS.WatchConfig
  -> Event t FilePath
  -> m (Event t FS.Event)
watchDirectory cfg path =
  performEventAsync $ ffor path $ \p cb -> liftIO $ void $ forkIO $
    FS.withManagerConf cfg $ \mgr -> do
      _ <- FS.watchTree mgr p (const True) cb
      forever $ threadDelay 1000000


