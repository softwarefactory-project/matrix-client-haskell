module Network.Matrix.Bot.Async.Internal
    ( AsyncHandler
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

import           Control.Concurrent.Async
                     ( AsyncCancelled(AsyncCancelled)
                     , withAsync
                     )
import           Control.Concurrent.MVar          ( MVar, putMVar )
import           Control.Concurrent.Supervisor (Actor(Actor), Inbox, SupervisorQueue, newActor, newSimpleOneForOneSupervisor, ActorQ, newBoundedActor, Restart(Permanent, Transient), newChild, newChildSpec, receive, send)
import Control.Concurrent.SupervisorInternal (InboxLength(InboxLength), CallTimeout(CallTimeout))
import           Control.Monad.Catch
                     ( MonadCatch
                     , MonadMask
                     , MonadThrow
                     , SomeException
                     , catch
                     , fromException
                     , try
                     )
import           Control.Monad.IO.Class           ( MonadIO, liftIO )
import           Control.Monad.IO.Unlift
                     ( MonadUnliftIO
                     , withRunInIO
                     )
import           Control.Monad.Trans.Class        ( MonadTrans(lift) )
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Control.Monad.Trans.State
                     ( StateT
                     )
import           Data.IORef                       ( newIORef
                                                  , readIORef
                                                  , writeIORef
                                                  )
import qualified Data.Text                        as T

import           Network.Matrix.Bot.ErrorHandling
import           Network.Matrix.Bot.State

newtype SyncGroup a = SyncGroup (ActorQ (a, T.Text, Maybe (MVar ())))

data SyncGroupCall m where
    AsyncGroupCall :: SyncGroup a -> a -> Maybe (MVar ()) -> SyncGroupCall m
    SyncCall :: (a -> m ()) -> a -> SyncGroupCall m

data AsyncHandler m a =
    AsyncHandler { asyncHandlerHandler :: Inbox (a, T.Text, Maybe (MVar ())) -> m ()
                 , asyncHandlerRestart :: Restart
                 }

class (MonadMatrixBot m) => MonadSyncGroupManager m where
    newSyncGroup :: (forall n.
                     (MonadMatrixBotBase n, MonadResyncableMatrixBot n)
                     => AsyncHandler n a)
                 -> m (SyncGroup a)
    default newSyncGroup
        :: (m ~ m1 m2, MonadTrans m1, MonadSyncGroupManager m2)
        => (forall n.
            (MonadMatrixBotBase n, MonadResyncableMatrixBot n)
            => AsyncHandler n a)
        -> m (SyncGroup a)
    newSyncGroup a = lift $ newSyncGroup a

instance (MonadSyncGroupManager m) => MonadSyncGroupManager (StateT s m)

newtype SyncGroupManagerT m a =
    SyncGroupManagerT (ReaderT SupervisorQueue m a)
    deriving ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch
             , MonadMask, MonadTrans, MonadFail )

runSyncGroupManager :: (MonadUnliftIO m) => SyncGroupManagerT m a -> m a
runSyncGroupManager (SyncGroupManagerT a) = do
  Actor svQ svA <- liftIO $ newActor newSimpleOneForOneSupervisor
  withRunInIO $ \runInIO ->
    withAsync svA $ const $ runInIO $ runReaderT a svQ

instance MonadResyncableMatrixBot m
    => MonadResyncableMatrixBot (SyncGroupManagerT m) where
    withSyncStartedAt st (SyncGroupManagerT a) =
        SyncGroupManagerT $ withSyncStartedAt st a

instance MonadMatrixBot m => MonadMatrixBot (SyncGroupManagerT m)

instance (MonadMatrixBotBase m, MonadResyncableMatrixBot m, MonadUnliftIO m)
    => MonadSyncGroupManager (SyncGroupManagerT m) where
    newSyncGroup (AsyncHandler handler restart) = do
      Actor aQ aA <- lift $ withRunInIO $ \runInIO -> newBoundedActor (InboxLength 20) $ runInIO . handler
      svQ <- SyncGroupManagerT ask
      -- Should never return Nothing, because the timeout is endless.
      _ <- liftIO $ newChild (CallTimeout $ -1) svQ $ newChildSpec restart aA
      pure $ SyncGroup aQ

syncGroupHandler :: (MonadMatrixBotBase m, MonadResyncableMatrixBot m)
                 => m s
                 -> (s -> a -> m s)
                 -> AsyncHandler m a
syncGroupHandler mkS handler =
  AsyncHandler
    { asyncHandlerHandler = \inbox -> do
        lastSeenSyncTokenRef <- syncedSince >>= liftIO . newIORef
        startHandler lastSeenSyncTokenRef inbox Nothing
    , asyncHandlerRestart = Permanent
    }
  where
    startHandler lastSeenSyncTokenRef inbox event =
      catch (mkS >>= go lastSeenSyncTokenRef inbox event)
      (restart lastSeenSyncTokenRef inbox)

    restart lastSeenSyncTokenRef inbox e = case fromException e of
      Just AsyncCancelled -> pure ()
      Nothing -> do
        logStderr $
          "Restarting permanent sync group due to unhandled exception: "
          ++ show (e :: SomeException)
        resyncAtAndEventM <- liftIO $
          discardUntilNextSyncToken lastSeenSyncTokenRef inbox
        let resyncAt = snd <$> resyncAtAndEventM
            event = fst <$> resyncAtAndEventM
        withSyncStartedAt resyncAt $
          startHandler lastSeenSyncTokenRef inbox event

    go lastSeenSyncTokenRef inbox (Just e) s =
      process lastSeenSyncTokenRef inbox e s
    go lastSeenSyncTokenRef inbox Nothing s =
      fetch lastSeenSyncTokenRef inbox s

    fetch lastSeenSyncTokenRef inbox s = do
      event <- liftIO $ receive inbox
      process lastSeenSyncTokenRef inbox event s

    process lastSeenSyncTokenRef inbox (e, syncToken, done) s = do
      liftIO (writeIORef lastSeenSyncTokenRef (Just syncToken))
      s' <- handler s e
      signalDone done
      go lastSeenSyncTokenRef inbox Nothing s'

    discardUntilNextSyncToken lastSeenSyncTokenRef inbox = do
      lastSeenSyncTokenMaybe <- readIORef lastSeenSyncTokenRef
      case lastSeenSyncTokenMaybe of
        Nothing -> pure Nothing
        Just lastSeenSyncToken -> do
          e@(_, syncToken, _) <- liftIO $ receive inbox
          if syncToken == lastSeenSyncToken
            then discardUntilNextSyncToken lastSeenSyncTokenRef
                 inbox
            else do
            writeIORef lastSeenSyncTokenRef $ Just syncToken
            pure $ Just (e, syncToken)

asyncHandler :: (MonadIO m) => (a -> m ()) -> AsyncHandler m a
asyncHandler handle = AsyncHandler
  { asyncHandlerHandler = \q -> do
      (event, _, done) <- liftIO $ receive q
      handle event *> signalDone done
  , asyncHandlerRestart = Transient
  }

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
        Left e -> logStderr $ "Sync handler threw an unexpected exception: "
            ++ show (e :: SomeException)
        Right () -> pure ()
syncGroupCall syncToken (AsyncGroupCall (SyncGroup chan) x done) =
    liftIO $ send chan (x, syncToken, done)
