module Network.Matrix.Bot.Router ( IsEventRouter(..)
                                 , routeAsyncEvent
                                 , BotEventRouter(..)
                                 , runRouterM
                                 , customRouter
                                 ) where

import Control.Monad.Catch ( MonadCatch
                           , MonadMask
                           , MonadThrow
                           )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class ( MonadTrans
                                 , lift
                                 )
import Control.Monad.Trans.State ( StateT
                                 , get
                                 , put
                                 )
import Control.Monad.Trans.Writer.Lazy ( WriterT
                                       , runWriterT
                                       , tell
                                       )
import Network.Matrix.Client (SyncResult( SyncResult
                                        , srNextBatch
                                        )
                             )

import Network.Matrix.Bot.Async
import Network.Matrix.Bot.Event
import Network.Matrix.Bot.State

class (IsSyncGroupManager m) => IsEventRouter m where
  routeSyncEvent :: (forall n. (MatrixBotBase n, MonadResyncableMatrixBot n) => a -> n ())
                 -> a -> m ()
  default routeSyncEvent :: ( m ~ m1 m2, MonadTrans m1, IsEventRouter m2)
                         => (forall n. (MatrixBotBase n, MonadResyncableMatrixBot n) => a -> n ())
                         -> a -> m ()
  routeSyncEvent f = lift . routeSyncEvent f

  routeSyncGroupEvent :: SyncGroup a -> a -> m ()
  default routeSyncGroupEvent :: ( m ~ m1 m2, MonadTrans m1, IsEventRouter m2)
                          => SyncGroup a -> a -> m ()
  routeSyncGroupEvent g = lift . routeSyncGroupEvent g

routeAsyncEvent :: (IsEventRouter m)
                => (forall n. (MatrixBotBase n, MonadResyncableMatrixBot n) => a -> n ())
                -> a -> m ()
routeAsyncEvent handle e = do
  syncGroup <- newSyncGroup $ asyncHandler handle
  routeSyncGroupEvent syncGroup e

-- | An event router with state @s@ and acces to an environment @r@
-- that is executed on top of the monad @m@.
data BotEventRouter m = forall s. BotEventRouter
  { initializeRouterState :: m s
  , berDoRoute :: s -> BotEvent -> RouterM m s
  }

newtype RouterM m a = RouterM
  { unRouterM :: WriterT [SyncGroupCall m] m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

instance MonadTrans RouterM where
  lift = RouterM . lift

instance IsMatrixBot m => IsMatrixBot (RouterM m)

instance (MonadResyncableMatrixBot m) => MonadResyncableMatrixBot (RouterM m) where
  withSyncStartedAt syncToken = RouterM . withSyncStartedAt syncToken . unRouterM

instance (IsSyncGroupManager m) => IsSyncGroupManager (RouterM m)

instance (MatrixBotBase m, MonadResyncableMatrixBot m, IsSyncGroupManager m) => IsEventRouter (RouterM m) where
  routeSyncEvent f x = RouterM $ tell [syncCall f x]
  routeSyncGroupEvent group x = RouterM $ tell [asyncGroupCall group x Nothing]

runRouterM :: (MatrixBotBase m)
           => (s -> BotEvent -> RouterM m s)
           -> SyncResult
           -> StateT s m ()
runRouterM router sr@SyncResult{srNextBatch} = mapM_ routeEvent $ extractBotEvents sr
  where routeEvent e = do
          s <- get
          (s', cs) <- lift $ runWriterT (unRouterM $ router s e)
          put s'
          mapM_ (lift . syncGroupCall srNextBatch) cs

customRouter :: ( MatrixBotBase m
                , MonadResyncableMatrixBot m
                , IsSyncGroupManager m
                )
             => m s
             -> (forall n. (MatrixBotBase n, MonadResyncableMatrixBot n, IsSyncGroupManager n, IsEventRouter n) => s -> BotEvent -> n s)
             -> BotEventRouter m
customRouter = BotEventRouter
