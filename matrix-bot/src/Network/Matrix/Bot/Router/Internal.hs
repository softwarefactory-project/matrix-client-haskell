module Network.Matrix.Bot.Router.Internal
    ( MonadEventRouter(..)
    , routeAsyncEvent
    , BotEventRouter(..)
    , runRouterT
    , customRouter
    ) where

import           Control.Monad.Catch               ( MonadCatch
                                                   , MonadMask
                                                   , MonadThrow
                                                   )
import           Control.Monad.IO.Class            ( MonadIO )
import           Control.Monad.Trans.Class         ( MonadTrans, lift )
import           Control.Monad.Trans.State         ( StateT, get, put )
import           Control.Monad.Trans.Writer.Lazy   ( WriterT
                                                   , runWriterT
                                                   , tell
                                                   )
import           Network.Matrix.Client             ( SyncResult(SyncResult, srNextBatch) )

import           Network.Matrix.Bot.Async.Internal
import           Network.Matrix.Bot.Event.Internal
import           Network.Matrix.Bot.State

class (MonadSyncGroupManager m) => MonadEventRouter m where
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

    routeSyncGroupEvent :: SyncGroup a -> a -> m ()
    default routeSyncGroupEvent
        :: (m ~ m1 m2, MonadTrans m1, MonadEventRouter m2)
        => SyncGroup a
        -> a
        -> m ()
    routeSyncGroupEvent g = lift . routeSyncGroupEvent g

routeAsyncEvent :: (MonadEventRouter m)
                => (forall n.
                    (MonadMatrixBotBase n, MonadResyncableMatrixBot n)
                    => a
                    -> n ())
                -> a
                -> m ()
routeAsyncEvent handle e = do
    syncGroup <- newSyncGroup $ asyncHandler handle
    routeSyncGroupEvent syncGroup e

-- | An event router with state @s@ and acces to an environment @r@
-- that is executed on top of the monad @m@.
data BotEventRouter m = forall s. BotEventRouter
    { initializeRouterState :: m s
    , berDoRoute :: s -> BotEvent -> RouterT m s
    }

newtype RouterT m a = RouterT { unRouterM :: WriterT [SyncGroupCall m] m a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch
             , MonadMask )

instance MonadTrans RouterT where
    lift = RouterT . lift

instance MonadMatrixBot m => MonadMatrixBot (RouterT m)

instance (MonadResyncableMatrixBot m)
    => MonadResyncableMatrixBot (RouterT m) where
    withSyncStartedAt syncToken =
        RouterT . withSyncStartedAt syncToken . unRouterM

instance (MonadSyncGroupManager m) => MonadSyncGroupManager (RouterT m)

instance ( MonadMatrixBotBase m
         , MonadResyncableMatrixBot m
         , MonadSyncGroupManager m
         ) => MonadEventRouter (RouterT m) where
    routeSyncEvent f x = RouterT $ tell [ syncCall f x ]

    routeSyncGroupEvent group x =
        RouterT $ tell [ asyncGroupCall group x Nothing ]

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
        mapM_ (lift . syncGroupCall srNextBatch) cs

customRouter
    :: ( MonadMatrixBotBase m
       , MonadResyncableMatrixBot m
       , MonadSyncGroupManager m
       )
    => m s
    -> (forall n.
        ( MonadMatrixBotBase n
        , MonadResyncableMatrixBot n
        , MonadSyncGroupManager n
        , MonadEventRouter n
        )
        => s
        -> BotEvent
        -> n s)
    -> BotEventRouter m
customRouter = BotEventRouter
