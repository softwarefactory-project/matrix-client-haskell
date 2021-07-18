{-# LANGUAGE OverloadedStrings #-}

-- | Matrix event data type
module Network.Matrix.Events
  ( MessageText (..),
    RoomMessage (..),
    Event (..),
    EventID (..),
    eventType,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), object, (.:), (.:?), (.=))
import Data.Aeson.Types (Pair)
import Data.Text (Text)

data MessageText = MessageText
  { mtBody :: Text,
    mtFormat :: Maybe Text,
    mtFormattedBody :: Maybe Text
  }
  deriving (Show, Eq)

instance FromJSON MessageText where
  parseJSON (Object v) =
    MessageText <$> v .: "body" <*> v .:? "format" <*> v .:? "formatted_body"
  parseJSON _ = mzero

messageTextAttr :: MessageText -> [Pair]
messageTextAttr msg =
  ["body" .= mtBody msg] <> format <> formattedBody
  where
    omitNull k vM = maybe [] (\v -> [k .= v]) vM
    format = omitNull "format" $ mtFormat msg
    formattedBody = omitNull "formatted_body" $ mtFormattedBody msg

data RoomMessage
  = RoomMessageText MessageText
  | RoomMessageEmote MessageText
  | RoomMessageNotice MessageText
  deriving (Show, Eq)

roomMessageType :: RoomMessage -> Text
roomMessageType roomMessage = case roomMessage of
  RoomMessageText _ -> "m.text"
  RoomMessageEmote _ -> "m.emote"
  RoomMessageNotice _ -> "m.notice"

instance ToJSON RoomMessage where
  toJSON msg =
    let msgtype = roomMessageType msg
        attr = case msg of
          RoomMessageText mt -> messageTextAttr mt
          RoomMessageEmote mt -> messageTextAttr mt
          RoomMessageNotice mt -> messageTextAttr mt
     in object (["msgtype" .= msgtype] <> attr)

instance FromJSON RoomMessage where
  parseJSON o@(Object v) = do
    msgType <- v .: "msgtype"
    case (msgType :: Text) of
      "m.text" -> RoomMessageText <$> parseJSON o
      "m.emote" -> RoomMessageEmote <$> parseJSON o
      "m.notice" -> RoomMessageNotice <$> parseJSON o
      _ -> mzero
  parseJSON _ = mzero

data Event
  = EventRoomMessage RoomMessage
  | EventUnknown Value
  deriving (Eq, Show)

instance ToJSON Event where
  toJSON event = case event of
    EventRoomMessage msg -> toJSON msg
    EventUnknown _ -> error $ "Event is not implemented: " <> show event

instance FromJSON Event where
  parseJSON v = EventRoomMessage <$> parseJSON v <|> pure (EventUnknown v)

eventType :: Event -> Text
eventType event = case event of
  EventRoomMessage _ -> "m.room.message"
  EventUnknown _ -> error $ "Event is not implemented: " <> show event

newtype EventID = EventID Text deriving (Show)

instance FromJSON EventID where
  parseJSON (Object v) = EventID <$> v .: "event_id"
  parseJSON _ = mzero
