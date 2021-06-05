{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the client-server API
-- https://matrix.org/docs/spec/client_server/r0.6.1
module Network.Matrix.Client
  ( -- * Client
    ClientSession,
    MatrixToken (..),
    getTokenFromEnv,
    createSession,

    -- * API
    MatrixIO,
    MatrixError (..),

    -- * User data
    UserID (..),
    getTokenOwner,

    -- * Room participation
    TxnID (..),
    sendMessage,
    module Network.Matrix.Events,

    -- * Room membership
    RoomID (..),
    getJoinedRooms,
    joinRoomById,
  )
where

import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), Value (Object), encode, (.:))
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Network.HTTP.Client as HTTP
import Network.Matrix.Events
import Network.Matrix.Internal

-- $setup
-- >>> import Data.Aeson (decode)

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
getTokenOwner :: ClientSession -> MatrixIO UserID
getTokenOwner session =
  doRequest session =<< mkRequest session True "/_matrix/client/r0/account/whoami"

newtype TxnID = TxnID Text deriving (Show, Eq)

sendMessage :: ClientSession -> RoomID -> Event -> TxnID -> MatrixIO EventID
sendMessage session (RoomID roomId) event (TxnID txnId) = do
  request <- mkRequest session True path
  doRequest
    session
    ( request
        { HTTP.method = "PUT",
          HTTP.requestBody = HTTP.RequestBodyLBS $ encode event
        }
    )
  where
    path = "/_matrix/client/r0/rooms/" <> roomId <> "/send/" <> eventId <> "/" <> txnId
    eventId = eventType event

newtype RoomID = RoomID Text deriving (Show, Eq, Hashable)

instance FromJSON RoomID where
  parseJSON (Object v) = RoomID <$> v .: "room_id"
  parseJSON _ = mzero

-- | A newtype wrapper to decoded nested list
--
-- >>> decode "{\"joined_rooms\": [\"!foo:example.com\"]}" :: Maybe JoinedRooms
-- Just (JoinedRooms {unRooms = [RoomID "!foo:example.com"]})
newtype JoinedRooms = JoinedRooms {unRooms :: [RoomID]} deriving (Show)

instance FromJSON JoinedRooms where
  parseJSON (Object v) = do
    rooms <- v .: "joined_rooms"
    pure . JoinedRooms $ RoomID <$> rooms
  parseJSON _ = mzero

getJoinedRooms :: ClientSession -> MatrixIO [RoomID]
getJoinedRooms session = do
  request <- mkRequest session True "/_matrix/client/r0/joined_rooms"
  response <- doRequest session request
  pure $ unRooms <$> response

joinRoomById :: ClientSession -> RoomID -> MatrixIO RoomID
joinRoomById session (RoomID roomId) = do
  request <- mkRequest session True $ "/_matrix/client/r0/rooms/" <> roomId <> "/join"
  doRequest session (request {HTTP.method = "POST"})
