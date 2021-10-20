{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Network.Matrix.Bot.State ( MatrixBotBase
                                , HasMatrixBotBaseLevel(..)
                                , IsMatrixBot(..)
                                , MatrixBot
                                , runMatrixBot
                                ) where

import Control.Monad.Catch ( MonadCatch
                           , MonadMask
                           , MonadThrow
                           )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class ( MonadTrans
                                 , lift
                                 )
import Control.Monad.Trans.Reader ( ReaderT
                                  , asks
                                  , runReaderT
                                  )
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Text as T
import Network.Matrix.Client ( ClientSession
                             , UserID
                             )

type MatrixBotBase m = ( IsMatrixBot m
                       , MonadIO m
                       , MonadMask m
                       )

class (Monad m) => IsMatrixBot m where
  clientSession :: m ClientSession
  default clientSession :: (m ~ m' n, MonadTrans m', IsMatrixBot n) => m ClientSession
  clientSession = lift clientSession
  myUserID :: m UserID
  default myUserID :: (m ~ m' n, MonadTrans m', IsMatrixBot n) => m UserID
  myUserID = lift myUserID
  syncedSince :: m (Maybe T.Text)
  default syncedSince :: (m ~ m' n, MonadTrans m', IsMatrixBot n) => m (Maybe T.Text)
  syncedSince = lift syncedSince

instance IsMatrixBot m => IsMatrixBot (ResourceT m)
instance IsMatrixBot m => IsMatrixBot (ReaderT r m)
instance IsMatrixBot m => IsMatrixBot (StateT s m)

-- | 'Network.Matrix.Bot.matrixBot' provides a base monad transformer
-- stack (optionally including transformer provided by the user
-- through 'Network.Matrix.Bot.runBotT'). But in some places other
-- transformers are stacked on top of this base transformer. In those
-- cases, this class allows lifting operations in the base monad up
-- the whole transformer stack.
class (IsMatrixBot m, IsMatrixBot (MatrixBotBaseLevel m)) => HasMatrixBotBaseLevel m where
  type MatrixBotBaseLevel m :: * -> *
  liftBotBase :: MatrixBotBaseLevel m a -> m a
  default liftBotBase  :: ( m ~ m' n, MonadTrans m', HasMatrixBotBaseLevel n
                          , MatrixBotBaseLevel m ~ MatrixBotBaseLevel n
                          )
                       => MatrixBotBaseLevel m a -> m a
  liftBotBase = lift . liftBotBase

instance HasMatrixBotBaseLevel m => HasMatrixBotBaseLevel (StateT s m) where
  type MatrixBotBaseLevel (StateT s m) = MatrixBotBaseLevel m

data MatrixBotEnv = MatrixBotEnv
  { mbeUserID :: UserID
  , mbeSession :: ClientSession
  , mbeSyncedSince :: Maybe T.Text
  }

newtype MatrixBot m a = MatrixBot (ReaderT MatrixBotEnv m a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadTrans
           , MonadIO
           , MonadCatch
           , MonadMask
           , MonadThrow
           , MonadUnliftIO
           )

runMatrixBot :: ClientSession -> UserID -> Maybe T.Text -> MatrixBot m a -> m a
runMatrixBot mbeSession mbeUserID mbeSyncedSince (MatrixBot ac) =
  runReaderT ac MatrixBotEnv{..}

instance Monad m => IsMatrixBot (MatrixBot m) where
  clientSession = MatrixBot $ asks mbeSession
  myUserID = MatrixBot $ asks mbeUserID
  syncedSince = MatrixBot $ asks mbeSyncedSince
