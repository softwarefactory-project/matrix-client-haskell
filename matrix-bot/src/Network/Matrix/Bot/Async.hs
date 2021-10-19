{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Network.Matrix.Bot.Async ( SyncGroup
                                , SyncGroupCall
                                , syncCall
                                , asyncGroupCall
                                , syncGroupCall
                                , startSyncGroup
                                ) where

import Control.Concurrent.Async ( Async
                                , async
                                , cancel
                                , wait
                                )
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMQueue ( TBMQueue
                                       , newTBMQueueIO
                                       , readTBMQueue
                                       , writeTBMQueue
                                       )
import Control.Concurrent.MVar (MVar)
import Control.Monad.IO.Class ( MonadIO
                              , liftIO
                              )
import Control.Monad.IO.Unlift ( UnliftIO
                               , unliftIO
                               )
import Control.Monad.Trans.Resource ( MonadResource
                                    , ReleaseKey
                                    , allocate
                                    )

import Network.Matrix.Bot.State

data SyncGroup (m :: * -> *) a = SyncGroup (Async ()) (TBMQueue (a, Maybe (MVar ()))) ReleaseKey

data SyncGroupCall m where
  AsyncGroupCall :: SyncGroup m a -> a -> Maybe (MVar ()) -> SyncGroupCall m
  SyncCall :: (a -> m ()) -> a -> SyncGroupCall m

syncCall :: (a -> m ()) -> a -> SyncGroupCall m
syncCall = SyncCall

asyncGroupCall :: SyncGroup m a -> a -> Maybe (MVar ()) -> SyncGroupCall m
asyncGroupCall = AsyncGroupCall

syncGroupCall :: (MatrixBotBase m) => SyncGroupCall m -> m ()
syncGroupCall (SyncCall f x) = f x
syncGroupCall (AsyncGroupCall (SyncGroup _ chan _) x done) = liftIO $ atomically $ writeTBMQueue chan (x, done)

startSyncGroup :: (MonadIO n, MonadResource m)
               => UnliftIO n
               -> (n (Maybe (a, Maybe (MVar ()))) -> n ()) -> m (SyncGroup n a)
startSyncGroup unliftHandler handler = do
  queue <- liftIO $ newTBMQueueIO 20
  let getNextEvent = liftIO $ atomically $ readTBMQueue queue
  (releaseKey, thread) <- resourceTAsync unliftHandler $ handler getNextEvent
  pure $ SyncGroup thread queue releaseKey

resourceTAsync :: (MonadResource m)
               => UnliftIO n
               -> n a -> m (ReleaseKey, Async a)
resourceTAsync unliftHandler f = do
  allocate (async $ unliftIO unliftHandler f) killThread

killThread :: Async a -> IO ()
killThread thread = do
  cancel thread
  _ <- wait thread
  pure ()
