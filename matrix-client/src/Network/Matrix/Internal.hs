{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

-- | This module contains low-level HTTP utility
module Network.Matrix.Internal where

import Control.Concurrent (threadDelay)
import Control.Exception (throw, throwIO)
import Control.Retry (RetryStatus (..))
import qualified Control.Retry as Retry
import Data.Aeson (FromJSON (..), FromJSONKey (..), Value (Object), encode, eitherDecode, object, withObject, (.:), (.:?), (.=))
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Hashable (Hashable)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO (hPutStrLn)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status (..))
import Network.HTTP.Types.Status (statusIsSuccessful)
import System.Environment (getEnv)
import System.IO (stderr)
import Control.Monad.Except
import Control.Monad.Catch.Pure
import Control.Monad.Reader
import Data.Coerce

newtype MatrixToken = MatrixToken Text
newtype Username = Username { username :: Text }
newtype DeviceId = DeviceId { deviceId :: Text }
newtype InitialDeviceDisplayName = InitialDeviceDisplayName { initialDeviceDisplayName :: Text} 
data LoginSecret = Password Text | Token Text

data LoginResponse = LoginResponse
  { lrUserId :: Text
  , lrAccessToken :: Text
  , lrHomeServer :: Text
  , lrDeviceId :: Text
  }

instance FromJSON LoginResponse where
  parseJSON = withObject "LoginResponse" $ \v -> do
    userId' <- v .: "user_id"
    accessToken' <- v .: "access_token"
    homeServer' <- v .: "home_server"
    deviceId' <- v .: "device_id"
    pure $ LoginResponse userId' accessToken' homeServer' deviceId'

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

mkRequest' :: MonadIO m => Text -> MatrixToken -> Bool -> Text -> m HTTP.Request
mkRequest' baseUrl' (MatrixToken token') auth path = do
  initRequest <- liftIO $ HTTP.parseUrlThrow (unpack $ baseUrl' <> path)
  pure $
    initRequest
      { HTTP.requestHeaders =
          [("Content-Type", "application/json")] <> authHeaders,
        HTTP.checkResponse = checkMatrixResponse
      }
  where
    authHeaders =
      [("Authorization", "Bearer " <> encodeUtf8 token') | auth]

mkLoginRequest' :: Text -> Maybe DeviceId -> Maybe InitialDeviceDisplayName -> Username -> LoginSecret -> IO HTTP.Request
mkLoginRequest' baseUrl' did idn (Username name) secret' = do
  let path = "/_matrix/client/r0/login"
  initRequest <- HTTP.parseUrlThrow (unpack $ baseUrl' <> path)

  let (secretKey, secret, secretType) = case secret' of
        Password pass -> ("password", pass, "m.login.password")
        Token tok -> ("token", tok, "m.login.token")

  let body = HTTP.RequestBodyLBS $ encode $ object $
        [ "identifier" .= object [ "type" .= ("m.id.user" :: Text), "user" .= name ]
        , secretKey  .= secret
        , "type" .= (secretType :: Text)
        ] <> catMaybes [ fmap (("device_id" .=) . deviceId) did
                       , fmap (("initial_device_display_name" .=) . initialDeviceDisplayName) idn
                       ]

  pure $ initRequest { HTTP.method = "POST", HTTP.requestBody = body, HTTP.requestHeaders = [("Content-Type", "application/json")] }

mkLogoutRequest' :: Text -> MatrixToken -> IO HTTP.Request
mkLogoutRequest' baseUrl' (MatrixToken token') = do
  let path = "/_matrix/client/r0/logout"
  initRequest <- HTTP.parseUrlThrow (unpack $ baseUrl' <> path)
  let headers = [("Authorization", encodeUtf8 $ "Bearer " <> token')]
  pure $ initRequest { HTTP.method = "POST", HTTP.requestHeaders = headers }

doRequest' :: FromJSON a => HTTP.Manager -> HTTP.Request -> IO (Either MatrixError a)
doRequest' manager' request = do
  response <- HTTP.httpLbs request manager'
  case decodeResp $ HTTP.responseBody response of
    Right x -> pure x
    Left e -> if statusIsSuccessful $ HTTP.responseStatus response
      then fail e
      else throwResponseError request response (HTTP.responseBody response)

decodeResp :: FromJSON a => ByteString -> Either String (Either MatrixError a)
decodeResp resp = case eitherDecode resp of
  Right a -> Right $ pure a
  Left e -> case eitherDecode resp of
    Right me -> Right $ Left me
    Left _ -> Left e

newtype UserID = UserID Text
  deriving (Show, Eq, Ord, Hashable, FromJSONKey)

instance FromJSON UserID where
  parseJSON (Object v) = UserID <$> v .: "user_id"
  parseJSON _ = mzero

data MatrixError = MatrixError
  { meErrcode :: Text,
    meError :: Text,
    meRetryAfterMS :: Maybe Int
  }
  deriving (Show, Eq)

data MatrixException = MatrixRateLimit deriving (Show)

instance Exception MatrixException

instance FromJSON MatrixError where
  parseJSON (Object v) =
    MatrixError
      <$> v .: "errcode"
      <*> v .: "error"
      <*> v .:? "retry_after_ms"
  parseJSON _ = mzero

-- | 'MatrixIO' is a convenient type alias for server response
type MatrixIO a = MatrixM IO a

-- | The session record, use 'createSession' to create it.
data ClientSession = ClientSession
  { baseUrl :: Text,
    token :: MatrixToken,
    manager :: HTTP.Manager
  }

newtype MatrixM m a = MatrixM { unMatrixM :: ExceptT MatrixError (ReaderT ClientSession m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError MatrixError
           , MonadIO
           , MonadThrow
           , MonadCatch
           , MonadMask
           , MonadReader ClientSession
           ) via (ExceptT MatrixError (ReaderT ClientSession m))

instance MonadTrans MatrixM where
  lift = MatrixM . lift . lift

-- | Interpret MatrixM into your inner monad. Wraps the calls that
-- interacts with the Matrix API.
runMatrixM :: ClientSession -> MatrixM m a -> m (Either MatrixError a)
runMatrixM session = flip runReaderT session . runExceptT . unMatrixM

-- | Run Matrix actions in 'IO'.
runMatrixIO :: ClientSession -> MatrixM IO a -> IO (Either MatrixError a)
runMatrixIO = runMatrixM
    
-- | Retry a network action
retryWithLog ::
  (MonadMask m, MonadIO m) =>
  -- | Maximum number of retry
  Int ->
  -- | A log function, can be used to measure errors
  (Text -> m ()) ->
  -- | The action to retry
  MatrixM m a ->
  MatrixM m a
retryWithLog limit logRetry action =
  Retry.recovering 
    (Retry.exponentialBackoff backoff <> Retry.limitRetries limit)
    [handler, rateLimitHandler]
    (const (checkAction))
  where
    checkAction =
      action `catchError` \case
        MatrixError "M_LIMIT_EXCEEDED" err delayMS -> do
          -- Reponse contains a retry_after_ms
          lift $ logRetry $ "RateLimit: " <> err <> " (delay: " <> pack (show delayMS) <> ")"
          liftIO $ threadDelay $ fromMaybe 5_000 delayMS * 1000
          throw MatrixRateLimit
        e -> throwError e
    backoff = 1000000 -- 1sec
    rateLimitHandler _ = Handler $ \case
      MatrixRateLimit -> pure True
    -- Log network error
    handler (RetryStatus num _ _) = Handler $ \case
      HTTP.HttpExceptionRequest req ctx -> do
        let url = decodeUtf8 (HTTP.host req) <> ":" <> pack (show (HTTP.port req)) <> decodeUtf8 (HTTP.path req)
            arg = decodeUtf8 $ HTTP.queryString req
            loc = if num == 0 then url <> arg else url
        lift $ logRetry $
          "NetworkFailure: "
            <> pack (show num)
            <> "/5 "
            <> loc
            <> " failed: "
            <> pack (show ctx)
        pure True
      HTTP.InvalidUrlException _ _ -> pure False

retry :: (MonadIO m, MonadMask m) => MatrixM m a -> MatrixM m a
retry = retryWithLog 7 (liftIO . hPutStrLn stderr)
