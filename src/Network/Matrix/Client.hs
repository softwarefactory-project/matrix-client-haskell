{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
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
    retry,
    retryWithLog,

    -- * User data
    UserID (..),
    getTokenOwner,

    -- * Room management
    RoomCreatePreset (..),
    RoomCreateRequest (..),
    createRoom,

    -- * Room participation
    TxnID (..),
    sendMessage,
    mkReply,
    module Network.Matrix.Events,

    -- * Room membership
    RoomID (..),
    getJoinedRooms,
    joinRoom,
    joinRoomById,
    leaveRoomById,

    -- * Filter
    EventFormat (..),
    EventFilter (..),
    defaultEventFilter,
    eventFilterAll,
    RoomEventFilter (..),
    defaultRoomEventFilter,
    roomEventFilterAll,
    StateFilter (..),
    defaultStateFilter,
    stateFilterAll,
    RoomFilter (..),
    defaultRoomFilter,
    Filter (..),
    defaultFilter,
    FilterID (..),
    messageFilter,
    createFilter,
    getFilter,

    -- * Events
    sync,
    getTimelines,
    syncPoll,
    Author (..),
    Presence (..),
    RoomEvent (..),
    RoomSummary (..),
    TimelineSync (..),
    JoinedRoomSync (..),
    SyncResult (..),
    SyncResultRoom (..),
  )
where

import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object, String), encode, genericParseJSON, genericToJSON, object, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map, foldrWithKey)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.URI (urlEncode)
import Network.Matrix.Events
import Network.Matrix.Internal
import Network.Matrix.Room

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

-- | A workaround data type to handle room create error being reported with a {message: "error"} response
data CreateRoomResponse = CreateRoomResponse
  { crrMessage :: Maybe Text,
    crrID :: Maybe Text
  }

instance FromJSON CreateRoomResponse where
  parseJSON (Object o) = CreateRoomResponse <$> o .:? "message" <*> o .:? "room_id"
  parseJSON _ = mzero

createRoom :: ClientSession -> RoomCreateRequest -> MatrixIO RoomID
createRoom session rcr = do
  request <- mkRequest session True "/_matrix/client/r0/createRoom"
  toRoomID
    <$> doRequest
      session
      ( request
          { HTTP.method = "POST",
            HTTP.requestBody = HTTP.RequestBodyLBS $ encode rcr
          }
      )
  where
    toRoomID :: Either MatrixError CreateRoomResponse -> Either MatrixError RoomID
    toRoomID resp = case resp of
      Left err -> Left err
      Right crr -> case (crrID crr, crrMessage crr) of
        (Just roomID, _) -> pure $ RoomID roomID
        (_, Just message) -> Left $ MatrixError "UNKNOWN" message Nothing
        _ -> Left $ MatrixError "UNKOWN" "" Nothing

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

newtype RoomID = RoomID Text deriving (Show, Eq, Ord, Hashable)

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

-- | Note that this API takes either a room ID or alias, unlike 'joinRoomById'
joinRoom :: ClientSession -> Text -> MatrixIO RoomID
joinRoom session roomName = do
  request <- mkRequest session True $ "/_matrix/client/r0/join/" <> roomNameUrl
  doRequest session (request {HTTP.method = "POST"})
  where
    roomNameUrl = decodeUtf8 . urlEncode True . encodeUtf8 $ roomName

joinRoomById :: ClientSession -> RoomID -> MatrixIO RoomID
joinRoomById session (RoomID roomId) = do
  request <- mkRequest session True $ "/_matrix/client/r0/rooms/" <> roomId <> "/join"
  doRequest session (request {HTTP.method = "POST"})

leaveRoomById :: ClientSession -> RoomID -> MatrixIO ()
leaveRoomById session (RoomID roomId) = do
  request <- mkRequest session True $ "/_matrix/client/r0/rooms/" <> roomId <> "/leave"
  fmap ensureEmptyObject <$> doRequest session (request {HTTP.method = "POST"})
  where
    ensureEmptyObject :: Value -> ()
    ensureEmptyObject value = case value of
      Object xs | xs == mempty -> ()
      _anyOther -> error $ "Unknown leave response: " <> show value

-------------------------------------------------------------------------------
-- https://matrix.org/docs/spec/client_server/latest#post-matrix-client-r0-user-userid-filter
newtype FilterID = FilterID Text deriving (Show, Eq, Hashable)

instance FromJSON FilterID where
  parseJSON (Object v) = FilterID <$> v .: "filter_id"
  parseJSON _ = mzero

data EventFormat = Client | Federation deriving (Show, Eq)

instance ToJSON EventFormat where
  toJSON ef = case ef of
    Client -> "client"
    Federation -> "federation"

instance FromJSON EventFormat where
  parseJSON v = case v of
    (String "client") -> pure Client
    (String "federation") -> pure Federation
    _ -> mzero

data EventFilter = EventFilter
  { efLimit :: Maybe Int,
    efNotSenders :: Maybe [Text],
    efNotTypes :: Maybe [Text],
    efSenders :: Maybe [Text],
    efTypes :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

defaultEventFilter :: EventFilter
defaultEventFilter = EventFilter Nothing Nothing Nothing Nothing Nothing

-- | A filter that should match nothing
eventFilterAll :: EventFilter
eventFilterAll = defaultEventFilter {efLimit = Just 0, efNotTypes = Just ["*"]}

aesonOptions :: Aeson.Options
aesonOptions = (aesonPrefix snakeCase) {Aeson.omitNothingFields = True}

instance ToJSON EventFilter where
  toJSON = genericToJSON aesonOptions

instance FromJSON EventFilter where
  parseJSON = genericParseJSON aesonOptions

data RoomEventFilter = RoomEventFilter
  { refLimit :: Maybe Int,
    refNotSenders :: Maybe [Text],
    refNotTypes :: Maybe [Text],
    refSenders :: Maybe [Text],
    refTypes :: Maybe [Text],
    refLazyLoadMembers :: Maybe Bool,
    refIncludeRedundantMembers :: Maybe Bool,
    refNotRooms :: Maybe [Text],
    refRooms :: Maybe [Text],
    refContainsUrl :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

defaultRoomEventFilter :: RoomEventFilter
defaultRoomEventFilter = RoomEventFilter Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | A filter that should match nothing
roomEventFilterAll :: RoomEventFilter
roomEventFilterAll = defaultRoomEventFilter {refLimit = Just 0, refNotTypes = Just ["*"]}

instance ToJSON RoomEventFilter where
  toJSON = genericToJSON aesonOptions

instance FromJSON RoomEventFilter where
  parseJSON = genericParseJSON aesonOptions

data StateFilter = StateFilter
  { sfLimit :: Maybe Int,
    sfNotSenders :: Maybe [Text],
    sfNotTypes :: Maybe [Text],
    sfSenders :: Maybe [Text],
    sfTypes :: Maybe [Text],
    sfLazyLoadMembers :: Maybe Bool,
    sfIncludeRedundantMembers :: Maybe Bool,
    sfNotRooms :: Maybe [Text],
    sfRooms :: Maybe [Text],
    sfContains_url :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

defaultStateFilter :: StateFilter
defaultStateFilter = StateFilter Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

stateFilterAll :: StateFilter
stateFilterAll = defaultStateFilter {sfLimit = Just 0, sfNotTypes = Just ["*"]}

instance ToJSON StateFilter where
  toJSON = genericToJSON aesonOptions

instance FromJSON StateFilter where
  parseJSON = genericParseJSON aesonOptions

data RoomFilter = RoomFilter
  { rfNotRooms :: Maybe [Text],
    rfRooms :: Maybe [Text],
    rfEphemeral :: Maybe RoomEventFilter,
    rfIncludeLeave :: Maybe Bool,
    rfState :: Maybe StateFilter,
    rfTimeline :: Maybe RoomEventFilter,
    rfAccountData :: Maybe RoomEventFilter
  }
  deriving (Show, Eq, Generic)

defaultRoomFilter :: RoomFilter
defaultRoomFilter = RoomFilter Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON RoomFilter where
  toJSON = genericToJSON aesonOptions

instance FromJSON RoomFilter where
  parseJSON = genericParseJSON aesonOptions

data Filter = Filter
  { filterEventFields :: Maybe [Text],
    filterEventFormat :: Maybe EventFormat,
    filterPresence :: Maybe EventFilter,
    filterAccountData :: Maybe EventFilter,
    filterRoom :: Maybe RoomFilter
  }
  deriving (Show, Eq, Generic)

defaultFilter :: Filter
defaultFilter = Filter Nothing Nothing Nothing Nothing Nothing

-- | A filter to keep all the messages
messageFilter :: Filter
messageFilter =
  defaultFilter
    { filterPresence = Just eventFilterAll,
      filterAccountData = Just eventFilterAll,
      filterRoom = Just roomFilter
    }
  where
    roomFilter =
      defaultRoomFilter
        { rfEphemeral = Just roomEventFilterAll,
          rfState = Just stateFilterAll,
          rfTimeline = Just timelineFilter,
          rfAccountData = Just roomEventFilterAll
        }
    timelineFilter =
      defaultRoomEventFilter
        { refTypes = Just ["m.room.message"]
        }

instance ToJSON Filter where
  toJSON = genericToJSON aesonOptions

instance FromJSON Filter where
  parseJSON = genericParseJSON aesonOptions

-- | Upload a new filter definition to the homeserver
-- https://matrix.org/docs/spec/client_server/latest#post-matrix-client-r0-user-userid-filter
createFilter ::
  -- | The client session, use 'createSession' to get one.
  ClientSession ->
  -- | The userID, use 'getTokenOwner' to get it.
  UserID ->
  -- | The filter definition, use 'defaultFilter' to create one or use the 'messageFilter' example.
  Filter ->
  -- | The function returns a 'FilterID' suitable for the 'sync' function.
  MatrixIO FilterID
createFilter session (UserID userID) body = do
  request <- mkRequest session True path
  doRequest
    session
    ( request
        { HTTP.method = "POST",
          HTTP.requestBody = HTTP.RequestBodyLBS $ encode body
        }
    )
  where
    path = "/_matrix/client/r0/user/" <> userID <> "/filter"

getFilter :: ClientSession -> UserID -> FilterID -> MatrixIO Filter
getFilter session (UserID userID) (FilterID filterID) =
  doRequest session =<< mkRequest session True path
  where
    path = "/_matrix/client/r0/user/" <> userID <> "/filter/" <> filterID

-------------------------------------------------------------------------------
-- https://matrix.org/docs/spec/client_server/latest#get-matrix-client-r0-sync
newtype Author = Author Text
  deriving (Show, Eq)
  deriving newtype (FromJSON, ToJSON)

data RoomEvent = RoomEvent
  { reContent :: Event,
    reType :: Text,
    reEventId :: EventID,
    reSender :: Author
  }
  deriving (Show, Eq, Generic)

data RoomSummary = RoomSummary
  { rsJoinedMemberCount :: Maybe Int,
    rsInvitedMemberCount :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data TimelineSync = TimelineSync
  { tsEvents :: Maybe [RoomEvent],
    tsLimited :: Maybe Bool,
    tsPrevBatch :: Maybe Text
  }
  deriving (Show, Eq, Generic)

data JoinedRoomSync = JoinedRoomSync
  { jrsSummary :: Maybe RoomSummary,
    jrsTimeline :: TimelineSync
  }
  deriving (Show, Eq, Generic)

data Presence = Offline | Online | Unavailable deriving (Eq)

instance Show Presence where
  show = \case
    Offline -> "offline"
    Online -> "online"
    Unavailable -> "unavailable"

instance ToJSON Presence where
  toJSON ef = String . pack . show $ ef

instance FromJSON Presence where
  parseJSON v = case v of
    (String "offline") -> pure Offline
    (String "online") -> pure Online
    (String "unavailable") -> pure Unavailable
    _ -> mzero

data SyncResult = SyncResult
  { srNextBatch :: Text,
    srRooms :: Maybe SyncResultRoom
  }
  deriving (Show, Eq, Generic)

newtype SyncResultRoom = SyncResultRoom
  { srrJoin :: Map Text JoinedRoomSync
  }
  deriving (Show, Eq, Generic)

unFilterID :: FilterID -> Text
unFilterID (FilterID x) = x

-------------------------------------------------------------------------------
-- https://matrix.org/docs/spec/client_server/latest#forming-relationships-between-events

headMaybe :: [a] -> Maybe a
headMaybe xs = case xs of
  [] -> Nothing
  (x : _) -> Just x

tail' :: [a] -> [a]
tail' xs = case xs of
  [] -> []
  (_ : rest) -> rest

-- | An helper to create a reply body
--
-- >>> let sender = Author "foo@matrix.org"
-- >>> addReplyBody sender "Hello" "hi"
-- "> <foo@matrix.org> Hello\n\nhi"
--
-- >>> addReplyBody sender "" "hey"
-- "> <foo@matrix.org>\n\nhey"
--
-- >>> addReplyBody sender "a multi\nline" "resp"
-- "> <foo@matrix.org> a multi\n> line\n\nresp"
addReplyBody :: Author -> Text -> Text -> Text
addReplyBody (Author author) old reply =
  let oldLines = Text.lines old
      headLine = "> <" <> author <> ">" <> maybe "" (mappend " ") (headMaybe oldLines)
      newBody = [headLine] <> map (mappend "> ") (tail' oldLines) <> [""] <> [reply]
   in Text.dropEnd 1 $ Text.unlines newBody

addReplyFormattedBody :: RoomID -> EventID -> Author -> Text -> Text -> Text
addReplyFormattedBody (RoomID roomID) (EventID eventID) (Author author) old reply =
  Text.unlines
    [ "<mx-reply>",
      "  <blockquote>",
      "    <a href=\"https://matrix.to/#/" <> roomID <> "/" <> eventID <> "\">In reply to</a>",
      "    <a href=\"https://matrix.to/#/" <> author <> "\">" <> author <> "</a>",
      "    <br />",
      "    " <> old,
      "  </blockquote>",
      "</mx-reply>",
      reply
    ]

-- | Convert body by encoding HTML special char
--
-- >>> toFormattedBody "& <test>"
-- "&amp; &lt;test&gt;"
toFormattedBody :: Text -> Text
toFormattedBody = Text.concatMap char
  where
    char x = case x of
      '<' -> "&lt;"
      '>' -> "&gt;"
      '&' -> "&amp;"
      _ -> Text.singleton x

-- | Prepare a reply event
mkReply ::
  -- | The destination room, must match the original event
  RoomID ->
  -- | The original event
  RoomEvent ->
  -- | The reply message
  MessageText ->
  -- | The event to send
  Event
mkReply room re mt =
  let getFormattedBody mt' = fromMaybe (toFormattedBody $ mtBody mt') (mtFormattedBody mt')
      eventID = reEventId re
      author = reSender re
      updateText oldMT =
        oldMT
          { mtFormat = Just "org.matrix.custom.html",
            mtBody = addReplyBody author (mtBody oldMT) (mtBody mt),
            mtFormattedBody =
              Just $
                addReplyFormattedBody
                  room
                  eventID
                  author
                  (getFormattedBody oldMT)
                  (getFormattedBody mt)
          }

      newMessage = case reContent re of
        EventRoomMessage (RoomMessageText oldMT) -> updateText oldMT
        EventRoomReply _ (RoomMessageText oldMT) -> updateText oldMT
        EventRoomEdit _ (RoomMessageText oldMT) -> updateText oldMT
        EventUnknown x -> error $ "Can't reply to " <> show x
   in EventRoomReply eventID (RoomMessageText newMessage)

sync :: ClientSession -> Maybe FilterID -> Maybe Text -> Maybe Presence -> Maybe Int -> MatrixIO SyncResult
sync session filterM sinceM presenceM timeoutM = do
  request <- mkRequest session True "/_matrix/client/r0/sync"
  doRequest session (HTTP.setQueryString qs request)
  where
    toQs name = \case
      Nothing -> []
      Just v -> [(name, Just . encodeUtf8 $ v)]
    qs =
      toQs "filter" (unFilterID <$> filterM)
        <> toQs "since" sinceM
        <> toQs "set_presence" (pack . show <$> presenceM)
        <> toQs "timeout" (pack . show <$> timeoutM)

syncPoll ::
  -- | The client session, use 'createSession' to get one.
  ClientSession ->
  -- | A sync filter, use 'createFilter' to get one.
  Maybe FilterID ->
  -- | A since value, get it from a previous sync result using the 'srNextBatch' field.
  Maybe Text ->
  -- | Set the session presence.
  Maybe Presence ->
  -- | Your callback to handle sync result.
  (SyncResult -> IO ()) ->
  -- | This function does not return unless there is an error.
  MatrixIO ()
syncPoll session filterM sinceM presenceM cb = go sinceM
  where
    go since = do
      syncResultE <- retry $ sync session filterM since presenceM (Just 10_000)
      case syncResultE of
        Left err -> pure (Left err)
        Right sr -> cb sr >> go (Just (srNextBatch sr))

-- | Extract room events from a sync result
getTimelines :: SyncResult -> [(RoomID, NonEmpty RoomEvent)]
getTimelines sr = foldrWithKey getEvents [] joinedRooms
  where
    getEvents :: Text -> JoinedRoomSync -> [(RoomID, NonEmpty RoomEvent)] -> [(RoomID, NonEmpty RoomEvent)]
    getEvents roomID jrs acc = case tsEvents (jrsTimeline jrs) of
      Just (x : xs) -> (RoomID roomID, x :| xs) : acc
      _ -> acc
    joinedRooms = maybe mempty srrJoin (srRooms sr)

-------------------------------------------------------------------------------
-- Derived JSON instances
instance ToJSON RoomEvent where
  toJSON RoomEvent {..} =
    object
      [ "content" .= reContent,
        "type" .= reType,
        "event_id" .= unEventID reEventId,
        "sender" .= reSender
      ]

instance FromJSON RoomEvent where
  parseJSON (Object o) = do
    eventId <- o .: "event_id"
    RoomEvent <$> o .: "content" <*> o .: "type" <*> pure (EventID eventId) <*> o .: "sender"
  parseJSON _ = mzero

instance ToJSON RoomSummary where
  toJSON = genericToJSON aesonOptions

instance FromJSON RoomSummary where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON TimelineSync where
  toJSON = genericToJSON aesonOptions

instance FromJSON TimelineSync where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON JoinedRoomSync where
  toJSON = genericToJSON aesonOptions

instance FromJSON JoinedRoomSync where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON SyncResult where
  toJSON = genericToJSON aesonOptions

instance FromJSON SyncResult where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON SyncResultRoom where
  toJSON = genericToJSON aesonOptions

instance FromJSON SyncResultRoom where
  parseJSON = genericParseJSON aesonOptions
