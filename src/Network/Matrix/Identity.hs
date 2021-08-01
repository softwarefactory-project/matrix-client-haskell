{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the Identity service API
-- https://matrix.org/docs/spec/identity_service/r0.3.0.html
module Network.Matrix.Identity
  ( -- * Client
    IdentitySession,
    MatrixToken (..),
    getTokenFromEnv,
    createIdentitySession,

    -- * API
    MatrixIO,
    MatrixError (..),
    retry,
    retryWithLog,

    -- * User data
    UserID (..),
    getIdentityTokenOwner,

    -- * Association lookup
    HashDetails (..),
    hashDetails,
    Identity (..),
    identityLookup,
    HashedAddress,
    IdentityLookupRequest,
    IdentityLookupResponse,
    identitiesLookup,
    mkIdentityLookupRequest,
    toHashedAddress,
    lookupIdentity,
  )
where

import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), Value (Object, String), encode, object, (.:), (.=))
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy.Base64.URL (encodeBase64Unpadded)
import Data.Digest.Pure.SHA (bytestringDigest, sha256)
import qualified Data.HashMap.Strict as HM
import Data.List (lookup)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import qualified Network.HTTP.Client as HTTP
import Network.Matrix.Internal

-- $setup
-- >>> import Data.Aeson (decode)

-- | The session record, use 'createSession' to create it.
data IdentitySession = IdentitySession
  { baseUrl :: Text,
    token :: MatrixToken,
    manager :: HTTP.Manager
  }

-- | 'createSession' creates the session record.
createIdentitySession ::
  -- | The matrix identity base url, e.g. "https://matrix.org"
  Text ->
  -- | The user identity token
  MatrixToken ->
  IO IdentitySession
createIdentitySession baseUrl' token' = IdentitySession baseUrl' token' <$> mkManager

mkRequest :: IdentitySession -> Bool -> Text -> IO HTTP.Request
mkRequest IdentitySession {..} = mkRequest' baseUrl token

doRequest :: FromJSON a => IdentitySession -> HTTP.Request -> MatrixIO a
doRequest IdentitySession {..} = doRequest' manager

-- | 'getIdentityTokenOwner' gets information about the owner of a given access token.
getIdentityTokenOwner :: IdentitySession -> MatrixIO UserID
getIdentityTokenOwner session =
  doRequest session =<< mkRequest session True "/_matrix/identity/v2/account"

data HashDetails = HashDetails
  { hdAlgorithms :: NonEmpty Text,
    hdPepper :: Text
  }
  deriving (Show, Eq)

instance FromJSON HashDetails where
  parseJSON (Object v) = HashDetails <$> v .: "algorithms" <*> v .: "lookup_pepper"
  parseJSON _ = mzero

hashDetails :: IdentitySession -> MatrixIO HashDetails
hashDetails session =
  doRequest session =<< mkRequest session True "/_matrix/identity/v2/hash_details"

-- | Use 'identityLookup' to lookup a single identity, otherwise uses the full 'identitiesLookup'.
identityLookup :: IdentitySession -> HashDetails -> Identity -> MatrixIO (Maybe UserID)
identityLookup session hd ident = do
  fmap toUserIDM <$> identitiesLookup session ilr
  where
    toUserIDM = lookupIdentity address
    address = toHashedAddress hd ident
    ilr = mkIdentityLookupRequest hd [address]

data IdentityLookupRequest = IdentityLookupRequest
  { ilrHash :: Text,
    ilrPepper :: Text,
    ilrAddresses :: [HashedAddress]
  }
  deriving (Show, Eq)

newtype HashedAddress = HashedAddress Text deriving (Show, Eq)

-- | A newtype wrapper to decoded nested list
--
-- >>> decode "{\"mappings\": {\"hash\": \"user\"}}" :: Maybe IdentityLookupResponse
-- Just (IdentityLookupResponse [(HashedAddress "hash",UserID "user")])
newtype IdentityLookupResponse = IdentityLookupResponse [(HashedAddress, UserID)]
  deriving (Show)

instance FromJSON IdentityLookupResponse where
  parseJSON (Object v) = do
    mappings <- v .: "mappings"
    case mappings of
      (Object kv) -> pure . IdentityLookupResponse $ mapMaybe toTuple (HM.toList kv)
      _ -> mzero
    where
      toTuple (k, String s) = Just (HashedAddress k, UserID s)
      toTuple _ = Nothing
  parseJSON _ = mzero

identitiesLookup :: IdentitySession -> IdentityLookupRequest -> MatrixIO IdentityLookupResponse
identitiesLookup session ilr = do
  request <- mkRequest session True "/_matrix/identity/v2/lookup"
  doRequest
    session
    ( request
        { HTTP.method = "POST",
          HTTP.requestBody = HTTP.RequestBodyLBS body
        }
    )
  where
    getAddr (HashedAddress x) = x
    body =
      encode $
        object
          [ "addresses" .= map getAddr (ilrAddresses ilr),
            "algorithm" .= ilrHash ilr,
            "pepper" .= ilrPepper ilr
          ]

-- | Hash encoding for lookup
-- >>> encodeSHA256 "alice@example.com email matrixrocks"
-- "4kenr7N9drpCJ4AfalmlGQVsOn3o2RHjkADUpXJWZUc"
encodeSHA256 :: Text -> Text
encodeSHA256 = toStrict . encodeBase64Unpadded . bytestringDigest . sha256 . fromStrict . encodeUtf8

data Identity = Email Text | Msisdn Text deriving (Show, Eq)

toHashedAddress :: HashDetails -> Identity -> HashedAddress
toHashedAddress hd ident = HashedAddress $ encodeSHA256 $ val <> " " <> hdPepper hd
  where
    val = case ident of
      Email x -> x <> " email"
      Msisdn x -> x <> " msisdn"

mkIdentityLookupRequest :: HashDetails -> [HashedAddress] -> IdentityLookupRequest
mkIdentityLookupRequest hd = IdentityLookupRequest hash (hdPepper hd)
  where
    hash =
      if "sha256" `elem` hdAlgorithms hd
        then "sha256"
        else error "Only sha256 is supported"

lookupIdentity :: HashedAddress -> IdentityLookupResponse -> Maybe UserID
lookupIdentity x (IdentityLookupResponse xs) = Data.List.lookup x xs
