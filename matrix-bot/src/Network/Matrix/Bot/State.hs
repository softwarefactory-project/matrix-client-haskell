{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Matrix.Bot.State ( MatrixBotBase
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
