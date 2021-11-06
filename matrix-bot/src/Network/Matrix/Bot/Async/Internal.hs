module Network.Matrix.Bot.Async.Internal ( AsyncHandler
                                         , asyncHandler
                                         , syncGroupHandler
                                         , SyncGroup
                                         , SyncGroupCall
                                         , syncCall
                                         , asyncGroupCall
                                         , syncGroupCall
                                         , MonadSyncGroupManager(..)
                                         , SyncGroupManagerT
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

data SyncGroup a = SyncGroup (Async ()) (TBMQueue (a, T.Text, Maybe (MVar ())))

data SyncGroupCall m where
  AsyncGroupCall :: SyncGroup a -> a -> Maybe (MVar ()) -> SyncGroupCall m
  SyncCall :: (a -> m ()) -> a -> SyncGroupCall m

newtype AsyncHandler m a = AsyncHandler (TBMQueue (a, T.Text, Maybe (MVar ())) -> m ())

class (MonadMatrixBot m) => MonadSyncGroupManager m where
  newSyncGroup :: (forall n. (MonadMatrixBotBase n, MonadResyncableMatrixBot n) => AsyncHandler n a)
               -> m (SyncGroup a)
  default newSyncGroup :: ( m ~ m1 m2, MonadTrans m1, MonadSyncGroupManager m2
                          )
                       => (forall n. (MonadMatrixBotBase n, MonadResyncableMatrixBot n) => AsyncHandler n a)
                       -> m (SyncGroup a)
  newSyncGroup a = lift $ newSyncGroup a

  gcSyncGroups :: m ()
  default gcSyncGroups :: (m ~ m1 m2, MonadTrans m1, MonadSyncGroupManager m2) => m ()
  gcSyncGroups = lift gcSyncGroups

instance (MonadSyncGroupManager m) => MonadSyncGroupManager (StateT s m)

newtype SyncGroupManagerT m a =
  SyncGroupManagerT (StateT (M.Map (Async ()) ReleaseKey) (ResourceT m) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

runSyncGroupManager :: (MonadUnliftIO m) => SyncGroupManagerT m a -> m a
runSyncGroupManager (SyncGroupManagerT a) = runResourceT $ evalStateT a M.empty

instance MonadTrans SyncGroupManagerT where
  lift = SyncGroupManagerT . lift . lift

instance MonadResyncableMatrixBot m => MonadResyncableMatrixBot (SyncGroupManagerT m) where
  withSyncStartedAt st (SyncGroupManagerT a) = SyncGroupManagerT $ withSyncStartedAt st a

instance MonadMatrixBot m => MonadMatrixBot (SyncGroupManagerT m)

instance (MonadMatrixBotBase m, MonadResyncableMatrixBot m, MonadUnliftIO m) => MonadSyncGroupManager (SyncGroupManagerT m) where
  newSyncGroup (AsyncHandler handler) = do
    unliftHandler <- lift askUnliftIO
    SyncGroupManagerT $ do
      queue <- liftIO $ newTBMQueueIO 20
      (releaseKey, thread) <- resourceTAsync unliftHandler $ handler queue
      modify $ M.insert thread releaseKey
      pure $ SyncGroup thread queue

  gcSyncGroups = SyncGroupManagerT $ do
    ended <- get >>= liftIO
      . fmap catMaybes . traverse (\(thread, rk) -> fmap (thread,rk,) <$> poll thread) . M.toList
    mapM_ (\(_, rk, _) -> release rk) ended
    modify $ \allThreads -> foldl' (\acc (thread, _, _) -> M.delete thread acc) allThreads ended

syncGroupHandler :: (MonadMatrixBotBase m, MonadResyncableMatrixBot m)
                 => m s
                 -> (s -> a -> m s)
                 -> AsyncHandler m a
syncGroupHandler mkS handler = AsyncHandler $ \queue -> do
  lastSeenSyncTokenRef <- syncedSince >>= liftIO . newIORef
  startHandler lastSeenSyncTokenRef queue
  where startHandler lastSeenSyncTokenRef queue =
          catch (mkS >>= go lastSeenSyncTokenRef queue) (restart lastSeenSyncTokenRef queue)
        restart lastSeenSyncTokenRef queue e =
          case fromException e of
            Just AsyncCancelled -> pure ()
            Nothing -> do
              logStderr $ "Restarting permanent sync group due to unhandled exception: "
                ++ show (e :: SomeException)
              resyncAt <- liftIO $ discardUntilNextSyncToken lastSeenSyncTokenRef queue
              withSyncStartedAt resyncAt $ startHandler lastSeenSyncTokenRef queue
        go lastSeenSyncTokenRef queue s = do
          event <- liftIO $ atomically $ readTBMQueue queue
          case event of
            Nothing -> pure ()
            Just (e, syncToken, done) -> do
              liftIO (writeIORef lastSeenSyncTokenRef (Just syncToken))
              s' <- handler s e
              signalDone done
              go lastSeenSyncTokenRef queue s'
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

asyncGroupCall :: SyncGroup a -> a -> Maybe (MVar ()) -> SyncGroupCall m
asyncGroupCall = AsyncGroupCall

syncGroupCall :: (MonadMatrixBotBase m) => T.Text -> SyncGroupCall m -> m ()
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