{-# LANGUAGE OverloadedStrings #-}

-- | Matrix event data type
module Network.Matrix.Events (
    MessageTextType (..),
    MessageText (..),
    RoomMessage (..),
    Event (..),
    EventID (..),
    Annotation (..),
    eventType,
)
where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (Object, String), object, (.:), (.:?), (.=))
import Data.Aeson.Types (Pair)
import Data.Text (Text)

data MessageTextType
    = TextType
    | EmoteType
    | NoticeType
    deriving (Eq, Show)

instance FromJSON MessageTextType where
    parseJSON (String name) = case name of
        "m.text" -> pure TextType
        "m.emote" -> pure EmoteType
        "m.notice" -> pure NoticeType
        _ -> mzero
    parseJSON _ = mzero

instance ToJSON MessageTextType where
    toJSON mt = String $ case mt of
        TextType -> "m.text"
        EmoteType -> "m.emote"
        NoticeType -> "m.notice"

data MessageText = MessageText
    { mtBody :: Text
    , mtType :: MessageTextType
    , mtFormat :: Maybe Text
    , mtFormattedBody :: Maybe Text
    }
    deriving (Show, Eq)

instance FromJSON MessageText where
    parseJSON (Object v) =
        MessageText
            <$> v .: "body"
            <*> v .: "msgtype"
            <*> v .:? "format"
            <*> v .:? "formatted_body"
    parseJSON _ = mzero

messageTextAttr :: MessageText -> [Pair]
messageTextAttr msg =
    ["body" .= mtBody msg, "msgtype" .= mtType msg] <> format <> formattedBody
  where
    omitNull k = maybe [] (\v -> [k .= v])
    format = omitNull "format" $ mtFormat msg
    formattedBody = omitNull "formatted_body" $ mtFormattedBody msg

reactionAttr :: [Pair]
reactionAttr = ["msg_type" .= ("m.reaction" :: Text)]

instance ToJSON MessageText where
    toJSON = object . messageTextAttr

newtype RoomMessage
    = RoomMessageText MessageText
    deriving (Show, Eq)

roomMessageAttr :: RoomMessage -> [Pair]
roomMessageAttr rm = case rm of
    RoomMessageText mt -> messageTextAttr mt

instance ToJSON RoomMessage where
    toJSON msg = case msg of
        RoomMessageText mt -> toJSON mt

instance FromJSON RoomMessage where
    parseJSON x = RoomMessageText <$> parseJSON x

data RelatedMessage = RelatedMessage
    { rmMessage :: RoomMessage
    , rmRelatedTo :: EventID
    }
    deriving (Show, Eq)

data Event
    = -- | [`m.room.message`](https://spec.matrix.org/v1.17/client-server-api/#mroommessage)
      EventRoomMessage RoomMessage
    | -- | A reply defined by the parent event id and the reply message
      EventRoomReply EventID RoomMessage
    | -- | An edit defined by the original message and the new message
      EventRoomEdit (EventID, RoomMessage) RoomMessage
    | -- [`m.reaction`](https://spec.matrix.org/v1.17/client-server-api/#mreaction)
      EventReaction EventID Annotation
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
                            [ "rel_type" .= ("m.replace" :: Text)
                            , "event_id" .= eventID
                            ]
                    , "m.new_content" .= object (roomMessageAttr newMsg)
                    ]
             in object $ editAttr <> roomMessageAttr msg
        EventReaction (EventID eventID) (Annotation annotationText) ->
            let attr =
                    [ "m.relates_to"
                        .= object
                            [ "rel_type" .= ("m.annotation" :: Text)
                            , "event_id" .= eventID
                            , "key" .= annotationText
                            ]
                    ]
             in object $ attr <> reactionAttr
        EventUnknown v -> Object v

instance FromJSON Event where
    parseJSON (Object content) =
        parseRelated <|> parseMessage <|> pure (EventUnknown content)
      where
        parseMessage = EventRoomMessage <$> parseJSON (Object content)
        -- https://spec.matrix.org/v1.17/client-server-api/#forming-relationships-between-events
        parseRelated = do
            relateM <- content .: "m.relates_to"
            case relateM of
                Object relate ->
                    parseReply relate
                        <|> parseByRelType relate
                _ -> mzero
        -- rich replies is a special kind of a relationship not using rel_type
        -- https://spec.matrix.org/v1.17/client-server-api/#rich-replies
        parseReply relate =
            EventRoomReply <$> relate .: "m.in_reply_to" <*> parseJSON (Object content)
        -- relationships using rel_type
        parseByRelType relate = do
            rel_type <- relate .: "rel_type"
            case (rel_type :: Text) of
                -- https://spec.matrix.org/v1.17/client-server-api/#event-replacements
                "m.replace" -> do
                    ev <- EventID <$> relate .: "event_id"
                    msg <- parseJSON (Object content)
                    EventRoomEdit (ev, msg) <$> content .: "m.new_content"
                -- https://spec.matrix.org/v1.17/client-server-api/#mannotation-relationship-type
                "m.annotation" -> do
                    ev <- EventID <$> relate .: "event_id"
                    annotation <- Annotation <$> relate .: "key"
                    pure $ EventReaction ev annotation
                _ -> mzero
    parseJSON _ = mzero

eventType :: Event -> Text
eventType event = case event of
    EventRoomMessage _ -> "m.room.message"
    EventRoomReply _ _ -> "m.room.message"
    EventRoomEdit _ _ -> "m.room.message"
    EventReaction _ _ -> "m.reaction" -- https://spec.matrix.org/latest/client-server-api/#mreaction
    EventUnknown _ -> error $ "Event is not implemented: " <> show event

newtype Annotation = Annotation {unAnnotation :: Text} deriving (Show, Eq, Ord)

newtype EventID = EventID {unEventID :: Text} deriving (Show, Eq, Ord)

instance FromJSON EventID where
    parseJSON (Object v) = EventID <$> v .: "event_id"
    parseJSON _ = mzero

instance ToJSON EventID where
    toJSON (EventID v) = object ["event_id" .= v]
