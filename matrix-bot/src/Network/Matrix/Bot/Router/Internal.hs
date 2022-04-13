module Network.Matrix.Bot.Router.Internal
    ( MonadEventRouter(..)
    , routeAsyncEvent
    , BotEventRouter(..)
    , runRouterT
    , customRouter
    ) where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.Supervisor (send)
import           Control.Monad.Catch               ( MonadCatch
                                                   , MonadMask
                                                   , MonadThrow
                                                   , SomeException
                                                   , try
                                                   )
import           Control.Monad.IO.Class            ( MonadIO, liftIO )
import           Control.Monad.Trans.Class         ( MonadTrans, lift )
import           Control.Monad.Trans.State         ( StateT, get, put )
import           Control.Monad.Trans.Writer.Lazy   ( WriterT
                                                   , runWriterT
                                                   , tell
                                                   )
import qualified Data.Text as T
import           Network.Matrix.Client             ( SyncResult(SyncResult, srNextBatch) )

import Network.Matrix.Bot.ErrorHandling
import           Network.Matrix.Bot.Event.Internal
import           Network.Matrix.Bot.EventGroup.Internal
import           Network.Matrix.Bot.State

class (MonadEventGroupManager m) => MonadEventRouter m where
    routeSyncEvent :: (forall n.
                       (MonadMatrixBotBase n, MonadResyncableMatrixBot n)
                       => a
                       -> n ())
                   -> a
                   -> m ()
    default routeSyncEvent
        :: (m ~ m1 m2, MonadTrans m1, MonadEventRouter m2)
        => (forall n.
            (MonadMatrixBotBase n, MonadResyncableMatrixBot n)
            => a
            -> n ())
        -> a
        -> m ()
    routeSyncEvent f = lift . routeSyncEvent f

    routeGroupEvent :: EventGroup a -> a -> m ()
    default routeGroupEvent
        :: (m ~ m1 m2, MonadTrans m1, MonadEventRouter m2)
        => EventGroup a
        -> a
        -> m ()
    routeGroupEvent g = lift . routeGroupEvent g

routeAsyncEvent :: (MonadEventRouter m)
                => (forall n.
                    (MonadMatrixBotBase n, MonadResyncableMatrixBot n)
                    => a
                    -> n ())
                -> a
                -> m ()
routeAsyncEvent handle e = do
    syncGroup <- newEventGroup $ asyncHandler handle
    routeGroupEvent syncGroup e

-- | An event router with state @s@ and acces to an environment @r@
-- that is executed on top of the monad @m@.
data BotEventRouter m = forall s. BotEventRouter
    { initializeRouterState :: m s
    , berDoRoute :: s -> BotEvent -> RouterT m s
    }

data EventDispatch m where
    GroupDispatch :: EventGroup a -> a -> Maybe (MVar ()) -> EventDispatch m
    SyncDispatch :: (a -> m ()) -> a -> EventDispatch m

dispatchEvent :: (MonadMatrixBotBase m) => T.Text -> EventDispatch m -> m ()
dispatchEvent _syncToken (SyncDispatch f x) = do
    result <- try $ f x
    case result of
        Left e -> logStderr $ "Sync handler threw an unexpected exception: "
            ++ show (e :: SomeException)
        Right () -> pure ()
dispatchEvent syncToken (GroupDispatch (EventGroup chan) x done) =
    liftIO $ send chan (x, syncToken, done)

newtype RouterT m a = RouterT { unRouterM :: WriterT [EventDispatch m] m a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch
             , MonadMask )

instance MonadTrans RouterT where
    lift = RouterT . lift

instance MonadMatrixBot m => MonadMatrixBot (RouterT m)

instance (MonadResyncableMatrixBot m)
    => MonadResyncableMatrixBot (RouterT m) where
    withSyncStartedAt syncToken =
        RouterT . withSyncStartedAt syncToken . unRouterM

instance (MonadEventGroupManager m) => MonadEventGroupManager (RouterT m)

instance ( MonadMatrixBotBase m
         , MonadResyncableMatrixBot m
         , MonadEventGroupManager m
         ) => MonadEventRouter (RouterT m) where
    routeSyncEvent f x = RouterT $ tell [ SyncDispatch f x ]

    routeGroupEvent group x =
        RouterT $ tell [ GroupDispatch group x Nothing ]

runRouterT :: (MonadMatrixBotBase m)
           => (s -> BotEvent -> RouterT m s)
           -> SyncResult
           -> StateT s m ()
runRouterT router sr@SyncResult{srNextBatch} =
    mapM_ routeEvent $ extractBotEvents sr
  where
    routeEvent e = do
        s <- get
        (s', cs) <- lift $ runWriterT (unRouterM $ router s e)
        put s'
        mapM_ (lift . dispatchEvent srNextBatch) cs

customRouter
    :: ( MonadMatrixBotBase m
       , MonadResyncableMatrixBot m
       , MonadEventGroupManager m
       )
    => m s
    -> (forall n.
        ( MonadMatrixBotBase n
        , MonadResyncableMatrixBot n
        , MonadEventGroupManager n
        , MonadEventRouter n
        )
        => s
        -> BotEvent
        -> n s)
    -> BotEventRouter m
customRouter = BotEventRouter
