{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | This module contains low-level HTTP utility
module Network.Matrix.Internal where

import Control.Exception (throwIO)
import Control.Monad (mzero, unless, void)
import Control.Monad.Catch (Handler (Handler), MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Retry (RetryStatus (..))
import qualified Control.Retry as Retry
import Data.Aeson (FromJSON (..), Value (Object), eitherDecode, (.:), (.:?))
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO (hPutStrLn)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status (..))
import System.Environment (getEnv)
import System.IO (stderr)

newtype MatrixToken = MatrixToken Text

getTokenFromEnv ::
  -- | The envirnoment variable name
  Text ->
  IO MatrixToken
getTokenFromEnv env = MatrixToken . pack <$> getEnv (unpack env)

mkManager :: IO HTTP.Manager
mkManager = HTTP.newManager tlsManagerSettings

checkMatrixResponse :: HTTP.Request -> HTTP.Response HTTP.BodyReader -> IO ()
checkMatrixResponse req res =
  unless (200 <= code && code < 500) $ do
    chunk <- HTTP.brReadSome (HTTP.responseBody res) 1024
    throwResponseError req res chunk
  where
    Status code _ = HTTP.responseStatus res

throwResponseError :: HTTP.Request -> HTTP.Response body -> ByteString -> IO a
throwResponseError req res chunk =
  throwIO $ HTTP.HttpExceptionRequest req ex
  where
    ex = HTTP.StatusCodeException (void res) (toStrict chunk)

mkRequest' :: Text -> MatrixToken -> Bool -> Text -> IO HTTP.Request
mkRequest' baseUrl (MatrixToken token) auth path = do
  initRequest <- HTTP.parseUrlThrow (unpack $ baseUrl <> path)
  pure $
    initRequest
      { HTTP.requestHeaders =
          [("Content-Type", "application/json")] <> authHeaders,
        HTTP.checkResponse = checkMatrixResponse
      }
  where
    authHeaders =
      [("Authorization", "Bearer " <> encodeUtf8 token) | auth]

doRequest' :: FromJSON a => HTTP.Manager -> HTTP.Request -> IO (Either MatrixError a)
doRequest' manager request = do
  response <- HTTP.httpLbs request manager
  case decodeResp (HTTP.responseBody response) of
    Nothing -> throwResponseError request response (HTTP.responseBody response)
    Just a -> pure a

decodeResp :: FromJSON a => ByteString -> Maybe (Either MatrixError a)
decodeResp resp = case eitherDecode resp of
  Right a -> Just $ pure a
  Left _ -> case eitherDecode resp of
    Right me -> Just $ Left me
    Left _ -> Nothing

newtype UserID = UserID Text deriving (Show, Eq)

instance FromJSON UserID where
  parseJSON (Object v) = UserID <$> v .: "user_id"
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

-- | Retry 5 times network action, doubling backoff each time
retry :: (MonadMask m, MonadIO m) => m a -> m a
retry action =
  Retry.recovering
    (Retry.exponentialBackoff backoff <> Retry.limitRetries 7)
    [handler]
    (const action)
  where
    backoff = 1000000 -- 100ms
    -- Log network error
    handler (RetryStatus num _ _) = Handler $ \case
      HTTP.HttpExceptionRequest req ctx -> do
        let url = decodeUtf8 (HTTP.host req) <> ":" <> pack (show (HTTP.port req)) <> decodeUtf8 (HTTP.path req)
            arg = decodeUtf8 $ HTTP.queryString req
            loc = if num == 0 then url <> arg else url
        liftIO $
          hPutStrLn stderr $
            "NetworkFailure: "
              <> pack (show num)
              <> "/5 "
              <> loc
              <> " failed: "
              <> pack (show ctx)
        pure True
      HTTP.InvalidUrlException _ _ -> pure False
