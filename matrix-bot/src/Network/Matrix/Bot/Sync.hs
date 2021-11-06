{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Network.Matrix.Bot.Sync ( syncLoop
                               , getInitialSyncToken
                               ) where

import Control.Monad.IO.Class ( MonadIO
                              , liftIO
                              )
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.State (evalStateT)
import Data.Aeson ( FromJSON (parseJSON)
                  , ToJSON ( toEncoding
                           , toJSON
                           )
                  , genericParseJSON
                  , genericToEncoding
                  , genericToJSON
                  )
import Data.Functor ((<&>))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Matrix.Client ( ClientSession
                             , AccountData(accountDataType)
                             , EventFilter(efNotTypes)
                             , Filter(filterAccountData)
                             , MatrixError( MatrixError
                                          , meErrcode
                                          )
                             , MatrixM
                             , UserID
                             , createFilter
                             , defaultEventFilter
                             , defaultFilter
                             , getAccountData
                             , retry
                             , setAccountData
                             , srNextBatch
                             , syncPoll
                             )

import Network.Matrix.Bot.Async
import Network.Matrix.Bot.ErrorHandling
import Network.Matrix.Bot.JSON
import Network.Matrix.Bot.Router
import Network.Matrix.Bot.State

newtype SyncTokenAccountData = SyncTokenAccountData
  { stadSyncToken :: T.Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SyncTokenAccountData where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

instance FromJSON SyncTokenAccountData where
  parseJSON = genericParseJSON aesonOptions

instance AccountData SyncTokenAccountData where
  accountDataType _ = syncTokenAccountDataType

syncTokenAccountDataType :: T.Text
syncTokenAccountDataType = "invalid.example.fixme.use.real.domain.sync_token"

getInitialSyncToken :: (MonadIO m)
                    => ClientSession -> UserID -> MatrixM m (Maybe T.Text)
getInitialSyncToken session userID =
 liftIO (getAccountData session userID) <&> \case
   Left MatrixError{meErrcode="M_NOT_FOUND"} -> Right Nothing
   Left e -> Left e
   Right x -> Right (Just $ stadSyncToken x)

syncLoop :: (MatrixBotBase m, MonadUnliftIO m, MonadResyncableMatrixBot m)
         => (forall n. (MatrixBotBase n, MonadResyncableMatrixBot n, IsSyncGroupManager n) => BotEventRouter n)
         -> MatrixM m ()
syncLoop (BotEventRouter mkIRS router) = do
  session <- clientSession
  userID <- myUserID
  initialSyncToken <- syncedSince
  filterID <- liftIO (retry $ createFilter session userID mkFilter)
    >>= dieOnLeft "Could not create filter"
  runSyncGroupManager $ do
    initialRouterState <- mkIRS
    flip evalStateT initialRouterState $
      syncPoll session (Just filterID) initialSyncToken Nothing $ \sr -> do
      retry (saveSyncToken $ srNextBatch sr) >>= logOnLeft "Could not save sync token"
      runRouterM router sr
      gcSyncGroups

saveSyncToken :: (IsMatrixBot m, MonadIO m) => T.Text -> MatrixM m ()
saveSyncToken token = do
  session <- clientSession
  userID <- myUserID
  liftIO $ setAccountData session userID $ SyncTokenAccountData
    { stadSyncToken = token
    }

mkFilter :: Filter
mkFilter = defaultFilter
  { filterAccountData = Just $ defaultEventFilter
    { efNotTypes = Just [syncTokenAccountDataType]
    }
  }
