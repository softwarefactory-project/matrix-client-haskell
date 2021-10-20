{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Network.Matrix.Bot.Async ( AsyncHandler
                                , asyncHandler
                                , SyncGroup
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
import Control.Concurrent.MVar ( MVar
                               , putMVar
                               )
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
import Data.Function (fix)

import Network.Matrix.Bot.State

data SyncGroup (m :: * -> *) a = SyncGroup (Async ()) (TBMQueue (a, Maybe (MVar ()))) ReleaseKey

data SyncGroupCall m where
  AsyncGroupCall :: SyncGroup m a -> a -> Maybe (MVar ()) -> SyncGroupCall m
  SyncCall :: (a -> m ()) -> a -> SyncGroupCall m

newtype AsyncHandler m a = AsyncHandler (TBMQueue (a, Maybe (MVar ())) -> m ())

asyncHandler :: (MonadIO n)
             => (forall b. n b -> m b)
             -> (a -> n ())
             -> AsyncHandler m a
asyncHandler runT handler = AsyncHandler $ \q -> runT $ fix $ \recurse ->
  liftIO (atomically (readTBMQueue q))
  >>= maybe (pure ()) (\(e, done) -> handler e *> signalDone done *> recurse)
  where signalDone Nothing = pure ()
        signalDone (Just done) = liftIO $ putMVar done ()

syncCall :: (a -> m ()) -> a -> SyncGroupCall m
syncCall = SyncCall

asyncGroupCall :: SyncGroup m a -> a -> Maybe (MVar ()) -> SyncGroupCall m
asyncGroupCall = AsyncGroupCall

syncGroupCall :: (MatrixBotBase m) => SyncGroupCall m -> m ()
syncGroupCall (SyncCall f x) = f x
syncGroupCall (AsyncGroupCall (SyncGroup _ chan _) x done) = liftIO $ atomically $ writeTBMQueue chan (x, done)

startSyncGroup :: (MonadIO n, MonadResource m)
               => UnliftIO n
               -> AsyncHandler n a -> m (SyncGroup n a)
startSyncGroup unliftHandler (AsyncHandler handler) = do
  queue <- liftIO $ newTBMQueueIO 20
  (releaseKey, thread) <- resourceTAsync unliftHandler $ handler queue
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
