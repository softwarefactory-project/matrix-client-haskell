{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | This module contains low-level HTTP utility
module Network.Matrix.Internal where

import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), Value (Object), eitherDecode, (.:), (.:?))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv)

newtype MatrixToken = MatrixToken Text

getTokenFromEnv ::
  -- | The envirnoment variable name
  Text ->
  IO MatrixToken
getTokenFromEnv env = MatrixToken . pack <$> getEnv (unpack env)

mkManager :: IO HTTP.Manager
mkManager = HTTP.newManager tlsManagerSettings

mkRequest' :: Text -> MatrixToken -> Bool -> Text -> IO HTTP.Request
mkRequest' baseUrl (MatrixToken token) auth path = do
  initRequest <- HTTP.parseUrlThrow (unpack $ baseUrl <> path)
  pure $
    initRequest
      { HTTP.requestHeaders =
          [("Content-Type", "application/json")] <> authHeaders
      }
  where
    authHeaders =
      [("Authorization", "Bearer " <> encodeUtf8 token) | auth]

doRequest' :: FromJSON a => HTTP.Manager -> HTTP.Request -> IO (Either MatrixError a)
doRequest' manager request = do
  response <- HTTP.httpLbs request manager
  pure $ decodeResp (HTTP.responseBody response)

decodeResp :: FromJSON a => ByteString -> Either MatrixError a
decodeResp resp = case eitherDecode resp of
  Right a -> pure a
  Left err -> case eitherDecode resp of
    Right me -> Left me
    Left _ -> error $ "Could not decode: " <> err

newtype WhoAmI = WhoAmI Text deriving (Show, Eq)

instance FromJSON WhoAmI where
  parseJSON (Object v) = WhoAmI <$> v .: "user_id"
  parseJSON _ = mzero

data MatrixError = MatrixError
  { meErrcode :: Text,
    meError :: Text,
    meRetryAfterMS :: Maybe Int
  }
  deriving (Show, Eq)

instance FromJSON MatrixError where
  parseJSON (Object v) =
    MatrixError
      <$> v .: "errcode"
      <*> v .: "error"
      <*> v .:? "retry_after_ms"
  parseJSON _ = mzero

-- | 'MatrixIO' is a convenient type alias for server response
type MatrixIO a = IO (Either MatrixError a)
