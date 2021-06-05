{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the Identity service API
-- https://matrix.org/docs/spec/identity_service/r0.3.0.html
module Network.Matrix.Identity
  ( -- * Client
    IdentitySession,
    MatrixToken (..),
    createSession,

    -- * User data
    getTokenOwner,
    WhoAmI (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import qualified Network.HTTP.Client as HTTP
import Network.Matrix.Internal

-- | The session record, use 'createSession' to create it.
data IdentitySession = IdentitySession
  { baseUrl :: Text,
    token :: MatrixToken,
    manager :: HTTP.Manager
  }

-- | 'createSession' creates the session record.
createSession ::
  -- | The matrix identity base url, e.g. "https://matrix.org"
  Text ->
  -- | The user identity token
  MatrixToken ->
  IO IdentitySession
createSession baseUrl' token' = IdentitySession baseUrl' token' <$> mkManager

mkRequest :: IdentitySession -> Bool -> Text -> IO HTTP.Request
mkRequest IdentitySession {..} = mkRequest' baseUrl token

doRequest :: FromJSON a => IdentitySession -> HTTP.Request -> MatrixIO a
doRequest IdentitySession {..} = doRequest' manager

-- | 'getTokenOwner' gets information about the owner of a given access token.
getTokenOwner :: IdentitySession -> MatrixIO WhoAmI
getTokenOwner session =
  doRequest session =<< mkRequest session True "/_matrix/client/r0/account/whoami"
