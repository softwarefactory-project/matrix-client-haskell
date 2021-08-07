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
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (Object), object, (.:), (.:?), (.=))
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

roomMessageAttr :: RoomMessage -> [Pair]
roomMessageAttr roomMsg =
  let msgtype = roomMessageType roomMsg
      attr = case roomMsg of
        RoomMessageText mt -> messageTextAttr mt
        RoomMessageEmote mt -> messageTextAttr mt
        RoomMessageNotice mt -> messageTextAttr mt
   in ["msgtype" .= msgtype] <> attr

roomMessageType :: RoomMessage -> Text
roomMessageType roomMessage = case roomMessage of
  RoomMessageText _ -> "m.text"
  RoomMessageEmote _ -> "m.emote"
  RoomMessageNotice _ -> "m.notice"

instance ToJSON RoomMessage where
  toJSON msg = object $ roomMessageAttr msg

instance FromJSON RoomMessage where
  parseJSON o@(Object v) = do
    msgType <- v .: "msgtype"
    case (msgType :: Text) of
      "m.text" -> RoomMessageText <$> parseJSON o
      "m.emote" -> RoomMessageEmote <$> parseJSON o
      "m.notice" -> RoomMessageNotice <$> parseJSON o
      _ -> mzero
  parseJSON _ = mzero

data RelatedMessage = RelatedMessage
  { rmMessage :: RoomMessage,
    rmRelatedTo :: EventID
  }
  deriving (Show, Eq)

data Event
  = EventRoomMessage RoomMessage
  | -- | A reply defined by the parent event id and the reply message
    EventRoomReply EventID RoomMessage
  | -- | An edit defined by the original message and the new message
    EventRoomEdit (EventID, RoomMessage) RoomMessage
  | EventUnknown Object
  deriving (Eq, Show)

instance ToJSON Event where
  toJSON event = case event of
    EventRoomMessage msg -> toJSON msg
    EventRoomReply eventID msg ->
      let replyAttr =
            [ "m.relates_to"
                .= object
                  [ "m.in_reply_to" .= toJSON eventID
                  ]
            ]
       in object $ replyAttr <> roomMessageAttr msg
    EventRoomEdit (EventID eventID, msg) newMsg ->
      let editAttr =
            [ "m.relates_to"
                .= object
                  [ "rel_type" .= ("m.replace" :: Text),
                    "event_id" .= eventID
                  ],
              "m.new_content" .= object (roomMessageAttr newMsg)
            ]
       in object $ editAttr <> roomMessageAttr msg
    EventUnknown v -> Object v

instance FromJSON Event where
  parseJSON (Object content) =
    parseRelated <|> parseMessage <|> pure (EventUnknown content)
    where
      parseMessage = EventRoomMessage <$> parseJSON (Object content)
      parseRelated = do
        relateM <- content .: "m.relates_to"
        case relateM of
          Object relate -> parseReply relate <|> parseReplace relate
          _ -> mzero
      parseReply relate =
        EventRoomReply <$> relate .: "m.in_reply_to" <*> parseJSON (Object content)
      parseReplace relate = do
        rel_type <- relate .: "rel_type"
        if rel_type == ("m.replace" :: Text)
          then do
            ev <- EventID <$> relate .: "event_id"
            msg <- parseJSON (Object content)
            EventRoomEdit (ev, msg) <$> content .: "m.new_content"
          else mzero
  parseJSON _ = mzero

eventType :: Event -> Text
eventType event = case event of
  EventRoomMessage _ -> "m.room.message"
  EventRoomReply _ _ -> "m.room.message"
  EventRoomEdit _ _ -> "m.room.message"
  EventUnknown _ -> error $ "Event is not implemented: " <> show event

newtype EventID = EventID {unEventID :: Text} deriving (Show, Eq, Ord)

instance FromJSON EventID where
  parseJSON (Object v) = EventID <$> v .: "event_id"
  parseJSON _ = mzero

instance ToJSON EventID where
  toJSON (EventID v) = object ["event_id" .= v]
