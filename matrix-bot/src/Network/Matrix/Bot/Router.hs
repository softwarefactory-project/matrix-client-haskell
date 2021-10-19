{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Network.Matrix.Bot.Router ( IsEventRouter(..)
                                 , BotEventRouter(..)
                                 , SimpleBotEventRouter
                                 , execRouter
                                 , customRouter
                                 , transformRouter
                                 , WithExtraState
                                 , withExtraState
                                 ) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Catch ( MonadCatch
                           , MonadMask
                           , MonadThrow
                           )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift ( MonadUnliftIO
                               , askUnliftIO
                               )
import Control.Monad.State.Class ( MonadState( get
                                             , put
                                             )
                                 )
import Control.Monad.Trans.Class ( MonadTrans
                                 , lift
                                 )
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.State.Lazy (StateT)
import Control.Monad.Trans.Writer.Lazy ( WriterT
                                       , execWriterT
                                       , tell
                                       )
import Network.Matrix.Client (SyncResult)

import Network.Matrix.Bot.Async
import Network.Matrix.Bot.Event
import Network.Matrix.Bot.State

class (IsMatrixBot m, IsMatrixBot n) => IsEventRouter m s n | m -> s n where
  routeSyncEvent :: (a -> n ()) -> a -> m ()
  default routeSyncEvent :: (m ~ m1 m2, MonadTrans m1, IsEventRouter m2 s' n)
                         => (a -> n ()) -> a -> m ()
  routeSyncEvent f = lift . routeSyncEvent f

  routeAsyncEvent :: SyncGroup n a -> a -> m ()
  default routeAsyncEvent :: (m ~ m1 m2, MonadTrans m1, IsEventRouter m2 s' n)
                          => SyncGroup n a -> a -> m ()
  routeAsyncEvent g = lift . routeAsyncEvent g

  newSyncGroup :: (n (Maybe (a, Maybe (MVar ()))) -> n ())
               -> m (SyncGroup n a)
  default newSyncGroup :: (m ~ m1 m2, MonadTrans m1, IsEventRouter m2 s' n)
                       => (n (Maybe (a, Maybe (MVar ()))) -> n ()) -> m (SyncGroup n a)
  newSyncGroup = lift . newSyncGroup

  liftRouterBase :: n a -> m a
  default liftRouterBase :: (m ~ m1 m2, MonadTrans m1, IsEventRouter m2 s' n)
                         => n a -> m a
  liftRouterBase = lift . liftRouterBase

  getRouterState :: m s
  default getRouterState :: (m ~ m1 m2, MonadTrans m1, IsEventRouter m2 s n) => m s
  getRouterState = lift getRouterState

  putRouterState :: s -> m ()
  default putRouterState :: (m ~ m1 m2, MonadTrans m1, IsEventRouter m2 s n) => s -> m ()
  putRouterState = lift . putRouterState

getsRouterState :: (IsEventRouter m s n) => (s -> a) -> m a
getsRouterState = (<$> getRouterState)

modifyRouterState :: (IsEventRouter m s n) => (s -> s) -> m ()
modifyRouterState f = getRouterState >>= putRouterState . f

data BotEventRouter s m n where
  BotEventRouter :: { berInitialState :: n s
                    , berDoRoute :: BotEvent -> m ()
                    } -> BotEventRouter s m n

type SimpleBotEventRouter s n = forall m. ( MatrixBotBase m
                                          , IsEventRouter m s n
                                          , MatrixBotBase n
                                          ) => BotEventRouter s m n

newtype RouterM s m a = RouterM 
  { unRouterM :: WriterT [SyncGroupCall m] (StateT s (ResourceT m)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

instance MonadTrans (RouterM s) where
  lift = RouterM . lift . lift . lift

instance IsMatrixBot n => IsMatrixBot (RouterM s n)

instance (MonadUnliftIO m, IsMatrixBot m) => IsEventRouter (RouterM s m) s m where
  routeSyncEvent f x = RouterM $ tell [syncCall f x]
  routeAsyncEvent group x = RouterM $ tell [asyncGroupCall group x Nothing]
  newSyncGroup handler = RouterM $ do
    unliftHandler <- lift $ lift $ lift askUnliftIO
    startSyncGroup unliftHandler handler
  liftRouterBase = lift
  getRouterState = RouterM get
  putRouterState = RouterM . put

execRouter :: (MatrixBotBase m)
           => (BotEvent -> RouterM s m ())
           -> SyncResult
           -> StateT s (ResourceT m) ()
execRouter router = mapM_ routeEvent . extractBotEvents
  where routeEvent e =
          execWriterT (unRouterM $ router e) >>= mapM_ (lift . lift . syncGroupCall)

customRouter :: ( IsEventRouter m () n
                , MatrixBotBase m
                , Applicative n
                )
             => (BotEvent -> m ())
             -> BotEventRouter () m n
customRouter = BotEventRouter (pure ())

transformRouter :: (Monad m, Monad n)
                => (s -> n s')
                -> (forall a. m' a -> m a)
                -> BotEventRouter s m' n
                -> BotEventRouter s' m n
transformRouter transformInitialState runTransformer (BotEventRouter initialState handler) =
  BotEventRouter (initialState >>= transformInitialState) $ runTransformer . handler

newtype WithExtraState s es (n :: * -> *) m a = WithExtraState { runWithExtraState :: m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

instance MonadTrans (WithExtraState s es n) where
  lift = WithExtraState

instance IsMatrixBot m => IsMatrixBot (WithExtraState s es n m)
instance (IsEventRouter m (s, es) n) => IsEventRouter (WithExtraState s es n m) s n where
  getRouterState = lift $ getsRouterState fst
  putRouterState s = lift $ modifyRouterState $ \(_, es) -> (s, es)

instance (IsEventRouter m (s, es) n) => MonadState es (WithExtraState s es n m) where
  get = lift $ getsRouterState snd
  put es = lift $ modifyRouterState $ \(s, _) -> (s, es)

withExtraState :: (Monad m, Monad n)
               => n es
               -> BotEventRouter s (WithExtraState s es n m) n
               -> BotEventRouter (s, es) m n
withExtraState initialState = transformRouter (\s -> (s,) <$> initialState) runWithExtraState
