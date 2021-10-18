{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.Matrix.Bot.State ( MatrixBotBase
                                , HasSession(..)
                                , HasSessionT
                                , runHasSessionT
                                , MatrixBotSynced
                                , IsSynced(..)
                                , IsSyncedT
                                , runIsSyncedT
                                ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch ( MonadCatch
                           , MonadMask
                           , MonadThrow
                           )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class ( MonadTrans
                                 , lift
                                 )
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader ( ReaderT
                                  , ask
                                  , asks
                                  , runReaderT
                                  )
import qualified Data.Text as T
import Network.Matrix.Client ( ClientSession
                             , UserID
                             )

type MatrixBotBase m = ( HasSession m
                       , MonadIO m
                       , MonadMask m
                       , MonadBaseControl IO m
                       )

class (Monad m) => HasSession m where
  clientSession :: m ClientSession
  default clientSession :: (m ~ m' n, MonadTrans m', HasSession n) => m ClientSession
  clientSession = lift clientSession
  myUserID :: m UserID
  default myUserID :: (m ~ m' n, MonadTrans m', HasSession n) => m UserID
  myUserID = lift myUserID

data HasSessionTEnv = HasSessionTEnv
  { hsteUserID :: !UserID
  , hsteSession :: !ClientSession
  }

newtype HasSessionT m a = HasSessionT (ReaderT HasSessionTEnv m a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadTrans
           , MonadIO
           , MonadCatch
           , MonadMask
           , MonadThrow
           )

deriving instance (MonadBase b m) => MonadBase b (HasSessionT m)
deriving instance (MonadBaseControl b m) => MonadBaseControl b (HasSessionT m)

runHasSessionT :: ClientSession -> UserID -> HasSessionT m a -> m a
runHasSessionT hsteSession hsteUserID (HasSessionT ac) =
  runReaderT ac HasSessionTEnv{..}

instance Monad m => HasSession (HasSessionT m) where
  clientSession = HasSessionT $ asks hsteSession
  myUserID = HasSessionT $ asks hsteUserID

type MatrixBotSynced m = (MatrixBotBase m, IsSynced m)

class HasSession m => IsSynced m where
  syncedSince :: m (Maybe T.Text)
  default syncedSince :: (m ~ m' n, MonadTrans m', IsSynced n) => m (Maybe T.Text)
  syncedSince = lift syncedSince

newtype IsSyncedT m a = IsSyncedT (ReaderT (Maybe T.Text) m a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadTrans
           , MonadIO
           , MonadCatch
           , MonadMask
           , MonadThrow
           )

deriving instance (MonadBase b m) => MonadBase b (IsSyncedT m)
deriving instance (MonadBaseControl b m) => MonadBaseControl b (IsSyncedT m)

runIsSyncedT :: Maybe T.Text -> IsSyncedT m a -> m a
runIsSyncedT since (IsSyncedT ac) = runReaderT ac since

instance (HasSession m) => HasSession (IsSyncedT m)

instance (HasSession m) => IsSynced (IsSyncedT m) where
  syncedSince = IsSyncedT ask
