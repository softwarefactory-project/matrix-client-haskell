{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the client-server API
-- https://matrix.org/docs/spec/client_server/r0.6.1
module Network.Matrix.Client
  ( -- * Client
    ClientSession,
    MatrixToken (..),
    createSession,

    -- * User data
    getTokenOwner,
    WhoAmI (..),
  )
where

import Data.Aeson (FromJSON, eitherDecode)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Network.HTTP.Client as HTTP
import Network.Matrix.Internal

-- | The session record, use 'createSession' to create it.
data ClientSession = ClientSession
  { baseUrl :: Text,
    token :: MatrixToken,
    manager :: HTTP.Manager
  }

-- | 'createSession' creates the session record.
createSession ::
  -- | The matrix client-server base url, e.g. "https://matrix.org"
  Text ->
  -- | The user token
  MatrixToken ->
  IO ClientSession
createSession baseUrl' token' = ClientSession baseUrl' token' <$> mkManager

mkRequest :: ClientSession -> Bool -> Text -> IO HTTP.Request
mkRequest ClientSession {..} = mkRequest' baseUrl token

doRequest :: FromJSON a => ClientSession -> HTTP.Request -> MatrixIO a
doRequest ClientSession {..} = doRequest' manager

-- | 'getTokenOwner' gets information about the owner of a given access token.
getTokenOwner :: ClientSession -> MatrixIO WhoAmI
getTokenOwner session =
  doRequest session =<< mkRequest session True "/_matrix/client/r0/account/whoami"
