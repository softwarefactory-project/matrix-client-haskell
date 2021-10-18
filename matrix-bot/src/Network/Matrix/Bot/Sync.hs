{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Network.Matrix.Bot.Sync ( syncLoop
                               ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class ( MonadIO
                              , liftIO
                              )
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
import Network.Matrix.Client ( AccountData(accountDataType)
                             , EventFilter(efNotTypes)
                             , Filter(filterAccountData)
                             , MatrixError( MatrixError
                                          , meErrcode
                                          )
                             , MatrixM
                             , createFilter
                             , defaultEventFilter
                             , defaultFilter
                             , getAccountData
                             , retry
                             , setAccountData
                             , srNextBatch
                             , syncPoll
                             )

import Network.Matrix.Bot.ErrorHandling
import Network.Matrix.Bot.Handler
import Network.Matrix.Bot.JSON
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

getInitialSyncToken :: (HasSession m, MonadIO m, MonadMask m)
                    => MatrixM m (Maybe T.Text)
getInitialSyncToken = do
 session <- clientSession
 userID <- myUserID
 liftIO (getAccountData session userID) <&> \case
   Left MatrixError{meErrcode="M_NOT_FOUND"} -> Right Nothing
   Left e -> Left e
   Right x -> Right (Just $ stadSyncToken x)

syncLoop :: (MatrixBotBase m)
         => BotEventHandler
         -> MatrixM m ()
syncLoop handler = do
  userID <- myUserID
  session <- clientSession
  initialSyncToken <- retry getInitialSyncToken
    >>= dieOnLeft "Could not retrieve saved sync token"
  liftIO $ print initialSyncToken
  runIsSyncedT initialSyncToken $ do
    filterID <- liftIO (retry $ createFilter session userID mkFilter)
      >>= dieOnLeft "Could not create filter"
    withAsyncEventRouter handler $ \sendToHandler ->
      syncPoll session (Just filterID) initialSyncToken Nothing $ \sr -> do
      retry (saveSyncToken $ srNextBatch sr) >>= logOnLeft "Could not save sync token"
      sendToHandler sr

saveSyncToken :: (HasSession m, MonadIO m) => T.Text -> MatrixM m ()
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
