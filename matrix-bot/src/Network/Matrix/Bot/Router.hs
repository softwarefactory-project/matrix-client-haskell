{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Matrix.Bot.Router ( IsEventRouter(..)
                                 , routeAsyncEvent
                                 , BotEventRouter(..)
                                 , RunnableBotEventRouter
                                 , runRouterM
                                 , customRouter
                                 , hoistRouter
                                 ) where

import Control.Monad.Catch ( MonadCatch
                           , MonadMask
                           , MonadThrow
                           )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class ( MonadTrans
                                 , lift
                                 )
import Control.Monad.Trans.Writer.Lazy ( WriterT
                                       , execWriterT
                                       , tell
                                       )
import Network.Matrix.Client (SyncResult)

import Network.Matrix.Bot.Async
import Network.Matrix.Bot.Event
import Network.Matrix.Bot.State

class (IsSyncGroupManager m) => IsEventRouter m where
  routeSyncEvent :: (a -> MatrixBotBaseLevel m ()) -> a -> m ()
  default routeSyncEvent :: ( m ~ m1 m2, MonadTrans m1, IsEventRouter m2
                            , MatrixBotBaseLevel m ~ MatrixBotBaseLevel m2)
                         => (a -> MatrixBotBaseLevel m ()) -> a -> m ()
  routeSyncEvent f = lift . routeSyncEvent f

  routeSyncGroupEvent :: SyncGroup (MatrixBotBaseLevel m) a -> a -> m ()
  default routeSyncGroupEvent :: ( m ~ m1 m2, MonadTrans m1, IsEventRouter m2
                             , MatrixBotBaseLevel m ~ MatrixBotBaseLevel m2)
                          => SyncGroup (MatrixBotBaseLevel m) a -> a -> m ()
  routeSyncGroupEvent g = lift . routeSyncGroupEvent g

routeAsyncEvent :: (IsEventRouter m, MatrixBotBase (MatrixBotBaseLevel m))
                => (a -> MatrixBotBaseLevel m ()) -> a -> m ()
routeAsyncEvent handle e = do
  syncGroup <- newSyncGroup $ asyncHandler handle
  routeSyncGroupEvent syncGroup e

-- | An event router that can be executed in the monad @m@. Note that
-- the routing logic inside the router might use additional monad
-- transformers on top of @m@, for example if it the result of an
-- application of 'hoistRouter'.
data BotEventRouter m where
  BotEventRouter :: ( MatrixBotBase m'
                    , HasMatrixBotBaseLevel m'
                    , MatrixBotBase (MatrixBotBaseLevel m')
                    , IsSyncGroupManager m'
                    )
                 => { runRouterT :: forall a. m' a -> m a
                    , berDoRoute :: BotEvent -> RouterM m' ()
                    } -> BotEventRouter m

-- | An event router that can be executed under the conditions
-- provided by the bot framework, given the base monad @m@. I.e. if
-- you initialized the bot framework with the base monad @m@, you need
-- to provide a router of type @RunnableBotEventRouter m@. Note that
-- the @m@ here is different from the @m@ to 'BotEventRouter', because
-- the bot framework automatically runs the router on to of additional
-- transformers above the base stack.
type RunnableBotEventRouter m = forall n. ( MatrixBotBase n
                                          , MatrixBotBaseLevel n ~ m
                                          , IsSyncGroupManager n
                                          ) => BotEventRouter n

newtype RouterM m a = RouterM
  { unRouterM :: WriterT [SyncGroupCall (MatrixBotBaseLevel m)] m a }
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

instance (HasMatrixBotBaseLevel m) => HasMatrixBotBaseLevel (RouterM m) where
  type MatrixBotBaseLevel (RouterM m) = MatrixBotBaseLevel m

instance (IsSyncGroupManager m) => IsSyncGroupManager (RouterM m)

instance (IsSyncGroupManager m) => IsEventRouter (RouterM m) where
  routeSyncEvent f x = RouterM $ tell [syncCall f x]
  routeSyncGroupEvent group x = RouterM $ tell [asyncGroupCall group x Nothing]

runRouterM :: (HasMatrixBotBaseLevel m, MatrixBotBase (MatrixBotBaseLevel m))
           => (BotEvent -> RouterM m ())
           -> SyncResult
           -> m ()
runRouterM router = mapM_ routeEvent . extractBotEvents
  where routeEvent e =
          execWriterT (unRouterM $ router e) >>= mapM_ (liftBotBase . syncGroupCall)

customRouter :: ( IsSyncGroupManager m
                , MatrixBotBase m
                , MatrixBotBase (MatrixBotBaseLevel m)
                )
             => (forall n. (IsEventRouter (n m), MonadTrans n, MatrixBotBaseLevel (n m) ~ MatrixBotBaseLevel m) => BotEvent -> (n m) ())
             -> BotEventRouter m
customRouter = BotEventRouter id

hoistRouter :: (forall a. m' a -> m a)
                -> BotEventRouter m'
                -> BotEventRouter m
hoistRouter transformStack (BotEventRouter runStack handler) =
  BotEventRouter (transformStack . runStack) handler
