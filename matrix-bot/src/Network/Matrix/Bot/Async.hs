{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Matrix.Bot.Async ( AsyncHandler
                                , asyncHandler
                                , syncGroupHandler
                                , SyncGroup
                                , SyncGroupCall
                                , syncCall
                                , asyncGroupCall
                                , syncGroupCall
                                , IsSyncGroupManager(..)
                                , runSyncGroupManager
                                ) where

import Control.Concurrent.Async ( Async
                                , AsyncCancelled(AsyncCancelled)
                                , async
                                , cancel
                                , poll
                                , waitCatch
                                )
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMQueue ( TBMQueue
                                       , newTBMQueueIO
                                       , readTBMQueue
                                       , unGetTBMQueue
                                       , writeTBMQueue
                                       )
import Control.Concurrent.MVar ( MVar
                               , putMVar
                               )
import Control.Monad.Catch ( MonadCatch
                           , MonadMask
                           , MonadThrow
                           , SomeException
                           , catch
                           , fromException
                           , try
                           )
import Control.Monad.IO.Class ( MonadIO
                              , liftIO
                              )
import Control.Monad.IO.Unlift ( MonadUnliftIO
                               , UnliftIO
                               , askUnliftIO
                               , unliftIO
                               )
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Resource ( MonadResource
                                    , ReleaseKey
                                    , ResourceT
                                    , allocate
                                    , release
                                    , runResourceT
                                    )
import Control.Monad.Trans.State ( StateT
                                 , evalStateT
                                 , get
                                 , modify
                                 )
import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.IORef ( newIORef
                  , readIORef
                  , writeIORef
                  )
import qualified Data.Text as T

import Network.Matrix.Bot.ErrorHandling
import Network.Matrix.Bot.State

data SyncGroup (m :: * -> *) a = SyncGroup (Async ()) (TBMQueue (a, T.Text, Maybe (MVar ())))

data SyncGroupCall m where
  AsyncGroupCall :: SyncGroup m a -> a -> Maybe (MVar ()) -> SyncGroupCall m
  SyncCall :: (a -> m ()) -> a -> SyncGroupCall m

newtype AsyncHandler m a = AsyncHandler (TBMQueue (a, T.Text, Maybe (MVar ())) -> m ())

class (IsMatrixBot m, HasMatrixBotBaseLevel m) => IsSyncGroupManager m where
  newSyncGroup :: AsyncHandler (MatrixBotBaseLevel m) a
               -> m (SyncGroup (MatrixBotBaseLevel m) a)
  default newSyncGroup :: ( m ~ m1 m2, MonadTrans m1, IsSyncGroupManager m2
                          , MatrixBotBaseLevel m ~ MatrixBotBaseLevel m2
                          )
                       => AsyncHandler (MatrixBotBaseLevel m) a
                       -> m (SyncGroup (MatrixBotBaseLevel m) a)
  newSyncGroup = lift . newSyncGroup

  gcSyncGroups :: m ()
  default gcSyncGroups :: (m ~ m1 m2, MonadTrans m1, IsSyncGroupManager m2) => m ()
  gcSyncGroups = lift gcSyncGroups

instance (IsSyncGroupManager m) => IsSyncGroupManager (StateT s m)

newtype SyncGroupManager m a =
  SyncGroupManager (StateT (M.Map (Async ()) ReleaseKey) (ResourceT m) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

runSyncGroupManager :: (MonadUnliftIO m) => SyncGroupManager m a -> m a
runSyncGroupManager (SyncGroupManager a) = runResourceT $ evalStateT a M.empty

instance MonadTrans SyncGroupManager where
  lift = SyncGroupManager . lift . lift

instance MonadResyncableMatrixBot m => MonadResyncableMatrixBot (SyncGroupManager m) where
  withSyncStartedAt st (SyncGroupManager a) = SyncGroupManager $ withSyncStartedAt st a

instance IsMatrixBot m => IsMatrixBot (SyncGroupManager m)

instance (IsMatrixBot m) => HasMatrixBotBaseLevel (SyncGroupManager m) where
  type MatrixBotBaseLevel (SyncGroupManager m) = m
  liftBotBase = lift

instance (IsMatrixBot m, MonadUnliftIO m) => IsSyncGroupManager (SyncGroupManager m) where
  newSyncGroup (AsyncHandler handler) = do
    unliftHandler <- liftBotBase askUnliftIO
    SyncGroupManager $ do
      queue <- liftIO $ newTBMQueueIO 20
      (releaseKey, thread) <- resourceTAsync unliftHandler $ handler queue
      modify $ M.insert thread releaseKey
      pure $ SyncGroup thread queue

  gcSyncGroups = SyncGroupManager $ do
    ended <- get >>= liftIO
      . fmap catMaybes . traverse (\(thread, rk) -> fmap (thread,rk,) <$> poll thread) . M.toList
    mapM_ (\(_, rk, _) -> release rk) ended
    modify $ \allThreads -> foldl' (\acc (thread, _, _) -> M.delete thread acc) allThreads ended

syncGroupHandler :: (MonadIO n, MonadCatch m, MonadIO m, MonadResyncableMatrixBot m)
                 => (forall b. n b -> m b)
                 -> (a -> n ())
                 -> AsyncHandler m a
syncGroupHandler runT handler = AsyncHandler $ \queue -> do
  lastSeenSyncTokenRef <- syncedSince >>= liftIO . newIORef
  startHandler lastSeenSyncTokenRef queue
  where startHandler lastSeenSyncTokenRef queue =
          catch (runT $ go lastSeenSyncTokenRef queue) (restart lastSeenSyncTokenRef queue)
        restart lastSeenSyncTokenRef queue e =
          case fromException e of
            Just AsyncCancelled -> pure ()
            Nothing -> do
              logStderr $ "Restarting permanent sync group due to unhandled exception: "
                ++ show (e :: SomeException)
              resyncAt <- liftIO $ discardUntilNextSyncToken lastSeenSyncTokenRef queue
              withSyncStartedAt resyncAt $ startHandler lastSeenSyncTokenRef queue
        go lastSeenSyncTokenRef queue = do
          event <- liftIO $ atomically $ readTBMQueue queue
          case event of
            Nothing -> pure ()
            Just (e, syncToken, done) -> liftIO (writeIORef lastSeenSyncTokenRef (Just syncToken))
                                         *> handler e
                                         *> signalDone done
                                         *> go lastSeenSyncTokenRef queue
        discardUntilNextSyncToken lastSeenSyncTokenRef queue = do
          lastSeenSyncTokenMaybe <- readIORef lastSeenSyncTokenRef
          case lastSeenSyncTokenMaybe of
            Nothing -> pure Nothing
            Just lastSeenSyncToken -> do
              eMaybe <- liftIO $ atomically $ readTBMQueue queue
              case eMaybe of
                Nothing -> pure Nothing
                Just e@(_, syncToken, _) ->
                  if syncToken == lastSeenSyncToken
                  then discardUntilNextSyncToken lastSeenSyncTokenRef queue
                  else do
                    atomically $ unGetTBMQueue queue e
                    writeIORef lastSeenSyncTokenRef $ Just syncToken
                    pure $ Just syncToken

asyncHandler :: (MonadIO m) => (a -> m ()) -> AsyncHandler m a
asyncHandler handle = AsyncHandler $ \q -> do
  eventM <- liftIO $ atomically $ readTBMQueue q
  case eventM of
    Nothing -> pure ()
    Just (event, _, done) -> handle event *> signalDone done

signalDone :: (MonadIO m) => Maybe (MVar ()) -> m ()
signalDone Nothing = pure ()
signalDone (Just done) = liftIO $ putMVar done ()

syncCall :: (a -> m ()) -> a -> SyncGroupCall m
syncCall = SyncCall

asyncGroupCall :: SyncGroup m a -> a -> Maybe (MVar ()) -> SyncGroupCall m
asyncGroupCall = AsyncGroupCall

syncGroupCall :: (MatrixBotBase m) => T.Text -> SyncGroupCall m -> m ()
syncGroupCall _syncToken (SyncCall f x) = do
  result <- try $ f x
  case result of
    Left e ->
      logStderr $ "Sync handler threw an unexpected exception: " ++ show (e :: SomeException)
    Right () -> pure ()
syncGroupCall syncToken (AsyncGroupCall (SyncGroup _ chan) x done) =
  liftIO $ atomically $ writeTBMQueue chan (x, syncToken, done)

resourceTAsync :: (MonadResource m)
               => UnliftIO n
               -> n a -> m (ReleaseKey, Async a)
resourceTAsync unliftHandler f = do
  allocate (async $ unliftIO unliftHandler f) killThread

killThread :: Async a -> IO ()
killThread thread = do
  cancel thread
  result <- waitCatch thread
  case result of
    Left e -> case fromException e of
      Just AsyncCancelled -> pure ()
      Nothing -> logStderr $ "SyncGroup threw an unexpected exception: " ++ show e
    Right _ -> pure ()
  pure ()
