{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Matrix room related data types
module Network.Matrix.Room (RoomCreatePreset (..), RoomCreateRequest (..)) where

import Network.Matrix.Internal (aesonOptions)
import Data.Aeson (ToJSON (..), Value (..), genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | https://matrix.org/docs/spec/client_server/latest#post-matrix-client-r0-createroom
data RoomCreatePreset
    = PrivateChat
    | TrustedPrivateChat
    | PublicChat
    deriving (Eq, Show)

instance ToJSON RoomCreatePreset where
    toJSON preset = String $ case preset of
        PrivateChat -> "private_chat"
        TrustedPrivateChat -> "trusted_private_chat"
        PublicChat -> "public_chat"

data RoomCreateRequest = RoomCreateRequest
    { rcrPreset :: RoomCreatePreset
    , rcrRoomAliasName :: Text
    , rcrName :: Text
    , rcrTopic :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON RoomCreateRequest where
    toJSON = genericToJSON aesonOptions
