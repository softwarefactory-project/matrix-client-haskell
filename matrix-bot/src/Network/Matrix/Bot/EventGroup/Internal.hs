module Network.Matrix.Bot.EventGroup.Internal
  ( AsyncHandler,
    asyncHandler,
    groupHandler,
    EventGroup (..),
    MonadEventGroupManager (..),
    EventGroupManagerT,
    runEventGroupManager,
  )
where

import Control.Concurrent.Async
  ( AsyncCancelled (AsyncCancelled),
    withAsync,
  )
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Concurrent.Supervisor
  ( Actor (Actor),
    ActorQ,
    Inbox,
    Restart (Permanent, Transient),
    SupervisorQueue,
    newActor,
    newBoundedActor,
    newChild,
    newChildSpec,
    newSimpleOneForOneSupervisor,
    receive,
  )
import Control.Concurrent.SupervisorInternal
  ( CallTimeout (CallTimeout),
    InboxLength (InboxLength),
  )
import Control.Monad.Catch
  ( MonadCatch,
    MonadMask,
    MonadThrow,
    SomeException,
    catch,
    fromException,
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift
  ( MonadUnliftIO,
    withRunInIO,
  )
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.State
  ( StateT,
  )
import Data.IORef
  ( newIORef,
    readIORef,
    writeIORef,
  )
import qualified Data.Text as T
import Network.Matrix.Bot.ErrorHandling
import Network.Matrix.Bot.State

newtype EventGroup a = EventGroup (ActorQ (a, T.Text, Maybe (MVar ())))

data AsyncHandler m a = AsyncHandler
  { asyncHandlerHandler :: Inbox (a, T.Text, Maybe (MVar ())) -> m (),
    asyncHandlerRestart :: Restart
  }

class (MonadMatrixBot m) => MonadEventGroupManager m where
  newEventGroup ::
    ( forall n.
      (MonadMatrixBotBase n, MonadResyncableMatrixBot n) =>
      AsyncHandler n a
    ) ->
    m (EventGroup a)
  default newEventGroup ::
    (m ~ m1 m2, MonadTrans m1, MonadEventGroupManager m2) =>
    ( forall n.
      (MonadMatrixBotBase n, MonadResyncableMatrixBot n) =>
      AsyncHandler n a
    ) ->
    m (EventGroup a)
  newEventGroup a = lift $ newEventGroup a

instance (MonadEventGroupManager m) => MonadEventGroupManager (StateT s m)

newtype EventGroupManagerT m a
  = EventGroupManagerT (ReaderT SupervisorQueue m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadTrans,
      MonadFail
    )

runEventGroupManager :: (MonadUnliftIO m) => EventGroupManagerT m a -> m a
runEventGroupManager (EventGroupManagerT a) = do
  Actor svQ svA <- liftIO $ newActor newSimpleOneForOneSupervisor
  withRunInIO $ \runInIO ->
    withAsync svA $ const $ runInIO $ runReaderT a svQ

instance
  MonadResyncableMatrixBot m =>
  MonadResyncableMatrixBot (EventGroupManagerT m)
  where
  withSyncStartedAt st (EventGroupManagerT a) =
    EventGroupManagerT $ withSyncStartedAt st a

instance MonadMatrixBot m => MonadMatrixBot (EventGroupManagerT m)

instance
  (MonadMatrixBotBase m, MonadResyncableMatrixBot m, MonadUnliftIO m) =>
  MonadEventGroupManager (EventGroupManagerT m)
  where
  newEventGroup (AsyncHandler handler restart) = do
    Actor aQ aA <- lift $ withRunInIO $ \runInIO -> newBoundedActor (InboxLength 20) $ runInIO . handler
    svQ <- EventGroupManagerT ask
    -- Should never return Nothing, because the timeout is endless.
    _ <- liftIO $ newChild (CallTimeout $ -1) svQ $ newChildSpec restart aA
    pure $ EventGroup aQ

groupHandler ::
  (MonadMatrixBotBase m, MonadResyncableMatrixBot m) =>
  m s ->
  (s -> a -> m s) ->
  AsyncHandler m a
groupHandler mkS handler =
  AsyncHandler
    { asyncHandlerHandler = \inbox -> do
        lastSeenSyncTokenRef <- syncedSince >>= liftIO . newIORef
        startHandler lastSeenSyncTokenRef inbox Nothing,
      asyncHandlerRestart = Permanent
    }
  where
    startHandler lastSeenSyncTokenRef inbox event =
      catch
        (mkS >>= go lastSeenSyncTokenRef inbox event)
        (restart lastSeenSyncTokenRef inbox)

    restart lastSeenSyncTokenRef inbox e = case fromException e of
      Just AsyncCancelled -> pure ()
      Nothing -> do
        logStderr $
          "Restarting permanent sync group due to unhandled exception: "
            ++ show (e :: SomeException)
        resyncAtAndEventM <-
          liftIO $
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
            then
              discardUntilNextSyncToken
                lastSeenSyncTokenRef
                inbox
            else do
              writeIORef lastSeenSyncTokenRef $ Just syncToken
              pure $ Just (e, syncToken)

asyncHandler :: (MonadIO m) => (a -> m ()) -> AsyncHandler m a
asyncHandler handle =
  AsyncHandler
    { asyncHandlerHandler = \q -> do
        (event, _, done) <- liftIO $ receive q
        handle event *> signalDone done,
      asyncHandlerRestart = Transient
    }

signalDone :: (MonadIO m) => Maybe (MVar ()) -> m ()
signalDone Nothing = pure ()
signalDone (Just done) = liftIO $ putMVar done ()
