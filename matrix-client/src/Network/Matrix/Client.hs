{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | This module contains the client-server API
-- https://matrix.org/docs/spec/client_server/r0.6.1
module Network.Matrix.Client
  ( -- * Client
    ClientSession,
    LoginCredentials (..),
    MatrixToken (..),
    Username (..),
    DeviceId (..),
    InitialDeviceDisplayName (..),
    LoginSecret (..),
    LoginResponse (..),
    getTokenFromEnv,
    createSession,
    createSessionWithManager,
    login,
    loginToken,
    loginTokenWithManager,
    logout,

    -- * API
    MatrixM,
    MatrixIO,
    runMatrixM,
    runMatrixIO,
    MatrixError (..),
    retry,
    retryWithLog,

    -- * User data
    UserID (..),
    getTokenOwner,

    -- * Room Events
    EventType (..),
    MRCreate (..),
    MRCanonicalAlias (..),
    MRGuestAccess (..),
    MRHistoryVisibility (..),
    MRName (..),
    MRTopic (..),
    PaginatedRoomMessages (..),
    StateKey (..),
    StateEvent (..),
    StateContent (..),
    getRoomEvent,
    getRoomMembers,
    getRoomState,
    getRoomStateEvent,
    getRoomMessages,
    redact,
    sendRoomStateEvent,

    -- * Room management
    RoomCreatePreset (..),
    RoomCreateRequest (..),
    createRoom,

    -- * Room participation
    ResolvedRoomAlias (..),
    TxnID (..),
    sendMessage,
    mkReply,
    module Network.Matrix.Events,
    setRoomAlias,
    setRoomVisibility,
    resolveRoomAlias,
    deleteRoomAlias,
    getRoomAliases,

    -- * Room membership
    RoomID (..),
    RoomAlias (..),
    banUser,
    checkRoomVisibility,
    forgetRoom,
    getJoinedRooms,
    getPublicRooms,
    getPublicRooms',
    inviteToRoom,
    joinRoom,
    joinRoomById,
    leaveRoomById,
    kickUser,
    knockOnRoom,
    unbanUser,

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

    -- * Account data
    AccountData (accountDataType),
    getAccountData,
    getAccountData',
    setAccountData,
    setAccountData',

    -- * Events
    sync,
    getTimelines,
    syncPoll,
    Author (..),
    Presence (..),
    RoomEvent (..),
    RoomSummary (..),
    TimelineSync (..),
    InvitedRoomSync (..),
    JoinedRoomSync (..),
    SyncResult (..),
    SyncResultRoom (..),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object, String), encode, genericParseJSON, genericToJSON, object, withObject, withText, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Aeson.Types (Parser)
import Data.Bifunctor (bimap)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map, foldrWithKey)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.URI (urlEncode)
import Network.Matrix.Events
import Network.Matrix.Internal
  ( ClientSession (..),
    DeviceId (..),
    InitialDeviceDisplayName (..),
    LoginResponse (..),
    LoginSecret (..),
    MatrixError (..),
    MatrixIO,
    MatrixM,
    MatrixToken (..),
    UserID (..),
    Username (..),
    createSession,
    createSessionWithManager,
    doRequest,
    doRequest',
    getTokenFromEnv,
    mkLoginRequest',
    mkLogoutRequest',
    mkManager,
    mkRequest,
    retry,
    retryWithLog,
    runMatrixIO,
    runMatrixM,
  )
import Network.Matrix.Room
  ( RoomCreatePreset (..),
    RoomCreateRequest (..),
  )
import qualified Network.URI as URI

-- $setup
-- >>> import Data.Aeson (decode)

data LoginCredentials = LoginCredentials
  { lUsername :: Username,
    lLoginSecret :: LoginSecret,
    lBaseUrl :: T.Text,
    lDeviceId :: Maybe DeviceId,
    lInitialDeviceDisplayName :: Maybe InitialDeviceDisplayName
  }

mkLoginRequest :: LoginCredentials -> IO HTTP.Request
mkLoginRequest LoginCredentials {..} =
  mkLoginRequest' lBaseUrl lDeviceId lInitialDeviceDisplayName lUsername lLoginSecret

-- | 'login' allows you to generate a session token.
login :: LoginCredentials -> IO (Either MatrixError ClientSession)
login = fmap (fmap fst) . loginToken

-- | 'loginToken' allows you to generate a session token and recover the Matrix auth token.
loginToken :: LoginCredentials -> IO (Either MatrixError (ClientSession, MatrixToken))
loginToken cred = do
  manager <- mkManager
  loginTokenWithManager manager cred

-- | 'loginTokenWithManager' allows you to generate a session token with a custom http manager and recover the Matrix auth token.
loginTokenWithManager :: HTTP.Manager -> LoginCredentials -> IO (Either MatrixError (ClientSession, MatrixToken))
loginTokenWithManager manager cred = do
  req <- mkLoginRequest cred
  resp <- doRequest' manager req
  pure $ case resp of
    Left e -> Left e
    Right (LoginResponse {..}) ->
      let token = MatrixToken lrAccessToken
       in Right (ClientSession (lBaseUrl cred) token manager, token)

mkLogoutRequest :: ClientSession -> IO HTTP.Request
mkLogoutRequest ClientSession {..} = mkLogoutRequest' baseUrl token

-- | 'logout' allows you to destroy a session token.
logout :: ClientSession -> MatrixIO ()
logout session = do
  req <- liftIO $ mkLogoutRequest session
  void $ doRequest @Value req
  pure ()

-- | 'getTokenOwner' gets information about the owner of a given access token.
getTokenOwner :: MatrixIO UserID
getTokenOwner = do
  request <- mkRequest True "/_matrix/client/r0/account/whoami"
  doRequest request

-- | A workaround data type to handle room create error being reported with a {message: "error"} response
data CreateRoomResponse = CreateRoomResponse
  { crrMessage :: Maybe T.Text,
    crrID :: Maybe T.Text
  }

instance FromJSON CreateRoomResponse where
  parseJSON (Object o) = CreateRoomResponse <$> o .:? "message" <*> o .:? "room_id"
  parseJSON _ = mzero

-------------------------------------------------------------------------------
-- Room Event API Calls https://spec.matrix.org/v1.1/client-server-api/#getting-events-for-a-room

getRoomEvent :: RoomID -> EventID -> MatrixIO RoomEvent
getRoomEvent (RoomID rid) (EventID eid) = do
  request <- mkRequest True $ "/_matrix/client/v3/rooms/" <> rid <> "/event/" <> eid
  doRequest request

data User = User {userDisplayName :: T.Text, userAvatarUrl :: Maybe T.Text}
  deriving (Show)

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    userDisplayName <- o .: "display_name"
    userAvatarUrl <- o .:? "avatar_url"
    pure $ User {..}

-- | Unexported newtype to grant us a 'FromJSON' instance.
newtype JoinedUsers = JoinedUsers (Map UserID User)

instance FromJSON JoinedUsers where
  parseJSON = withObject "JoinedUsers" $ \o -> do
    users <- o .: "joined"
    pure $ JoinedUsers users

-- | This API returns a map of MXIDs to member info objects for
-- members of the room. The current user must be in the room for it to
-- work.
-- https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3roomsroomidjoined_members
getRoomMembers :: RoomID -> MatrixIO (Map UserID User)
getRoomMembers (RoomID rid) = do
  request <- mkRequest True $ "/_matrix/client/v3/rooms/" <> rid <> "/joined_members"
  fmap coerce $ doRequest @JoinedUsers request

newtype StateKey = StateKey T.Text
  deriving stock (Show)
  deriving newtype (FromJSON)

newtype EventType = EventType T.Text
  deriving stock (Show)
  deriving newtype (FromJSON)

data MRCreate = MRCreate {mrcCreator :: UserID, mrcRoomVersion :: Integer}
  deriving (Show)

instance FromJSON MRCreate where
  parseJSON = withObject "RoomCreate" $ \o -> do
    mrcCreator <- o .: "creator"
    mrcRoomVersion <- o .: "room_version"
    pure $ MRCreate {..}

newtype MRName = MRName {mrnName :: T.Text}
  deriving (Show)

instance FromJSON MRName where
  parseJSON = withObject "RoomName" $ \o ->
    MRName <$> (o .: "name")

newtype MRCanonicalAlias = MRCanonicalAlias {mrcAlias :: T.Text}
  deriving (Show)

instance FromJSON MRCanonicalAlias where
  parseJSON = withObject "RoomCanonicalAlias" $ \o ->
    MRCanonicalAlias <$> (o .: "alias")

newtype MRGuestAccess = MRGuestAccess {mrGuestAccess :: T.Text}
  deriving (Show)

instance FromJSON MRGuestAccess where
  parseJSON = withObject "GuestAccess" $ \o ->
    MRGuestAccess <$> (o .: "guest_access")

newtype MRHistoryVisibility = MRHistoryVisibility {mrHistoryVisibility :: T.Text}
  deriving (Show)

instance FromJSON MRHistoryVisibility where
  parseJSON = withObject "HistoryVisibility" $ \o ->
    MRHistoryVisibility <$> (o .: "history_visibility")

newtype MRTopic = MRTopic {mrTopic :: T.Text}
  deriving (Show)

instance FromJSON MRTopic where
  parseJSON = withObject "RoomTopic" $ \o ->
    MRTopic <$> (o .: "topic")

data StateContent
  = StRoomCreate MRCreate
  | -- | StRoomMember MRMember
    -- | StRoomPowerLevels MRPowerLevels
    -- | StRoomJoinRules MRJoinRules
    StRoomCanonicalAlias MRCanonicalAlias
  | StRoomGuestAccess MRGuestAccess
  | StRoomHistoryVisibility MRHistoryVisibility
  | StRoomName MRName
  | StRoomTopic MRTopic
  | StOther Value
  --- | StSpaceParent MRSpaceParent
  deriving (Show)

pStRoomCreate :: Value -> Parser StateContent
pStRoomCreate v = StRoomCreate <$> parseJSON v

pStRoomCanonicAlias :: Value -> Parser StateContent
pStRoomCanonicAlias v = StRoomCanonicalAlias <$> parseJSON v

pStRoomGuestAccess :: Value -> Parser StateContent
pStRoomGuestAccess v = StRoomGuestAccess <$> parseJSON v

pStRoomHistoryVisibility :: Value -> Parser StateContent
pStRoomHistoryVisibility v = StRoomHistoryVisibility <$> parseJSON v

pStRoomName :: Value -> Parser StateContent
pStRoomName v = StRoomName <$> parseJSON v

pStRoomTopic :: Value -> Parser StateContent
pStRoomTopic v = StRoomTopic <$> parseJSON v

pStRoomOther :: Value -> Parser StateContent
pStRoomOther v = StOther <$> parseJSON v

instance FromJSON StateContent where
  parseJSON v =
    pStRoomCreate v
      <|> pStRoomCanonicAlias v
      <|> pStRoomGuestAccess v
      <|> pStRoomHistoryVisibility v
      <|> pStRoomName v
      <|> pStRoomTopic v
      <|> pStRoomOther v

-- TODO(SOLOMON): Should This constructor be in 'Event'?
data StateEvent = StateEvent
  { seContent :: StateContent,
    seEventId :: EventID,
    seOriginServerTimestamp :: Integer,
    sePreviousContent :: Maybe Value,
    seRoomId :: RoomID,
    seSender :: UserID,
    seStateKey :: StateKey,
    seEventType :: EventType,
    seUnsigned :: Maybe Value
  }
  deriving (Show)

instance FromJSON StateEvent where
  parseJSON = withObject "StateEvent" $ \o -> do
    seContent <- o .: "content"
    seEventId <- fmap EventID $ o .: "event_id"
    seOriginServerTimestamp <- o .: "origin_server_ts"
    sePreviousContent <- o .:? "previous_content"
    seRoomId <- fmap RoomID $ o .: "room_id"
    seSender <- fmap UserID $ o .: "sender"
    seStateKey <- o .: "state_key"
    seEventType <- o .: "type"
    seUnsigned <- o .:? "unsigned"
    pure $ StateEvent {..}

-- | Get the state events for the current state of a room.
-- https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3roomsroomidstate
getRoomState :: RoomID -> MatrixIO [StateEvent]
getRoomState (RoomID rid) = do
  request <- mkRequest True $ "/_matrix/client/v3/rooms/" <> rid <> "/state"
  doRequest request

-- | Looks up the contents of a state event in a room. If the user is
-- joined to the room then the state is taken from the current state
-- of the room. If the user has left the room then the state is taken
-- from the state of the room when they left.
-- https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3roomsroomidstateeventtypestatekey
getRoomStateEvent :: RoomID -> EventType -> StateKey -> MatrixIO StateEvent
getRoomStateEvent (RoomID rid) (EventType et) (StateKey key) = do
  request <- mkRequest True $ "/_matrix/client/v3/rooms/" <> rid <> "/state" <> et <> "/" <> key
  doRequest request

data Dir = F | B

renderDir :: Dir -> B.ByteString
renderDir F = "f"
renderDir B = "b"

data PaginatedRoomMessages = PaginatedRoomMessages
  { chunk :: [RoomEvent],
    -- | A token corresponding to the end of chunk.
    end :: Maybe T.Text,
    -- | A token corresponding to the start of chunk.
    start :: T.Text,
    -- | A list of state events relevant to showing the chunk.
    state :: [StateEvent]
  }
  deriving (Show)

instance FromJSON PaginatedRoomMessages where
  parseJSON = withObject "PaginatedRoomMessages" $ \o -> do
    chunk <- o .: "chunk"
    end <- o .:? "end"
    start <- o .: "start"
    state <- fmap (fromMaybe []) $ o .:? "state"
    pure $ PaginatedRoomMessages {..}

getRoomMessages ::
  RoomID ->
  -- | The direction to return events from.
  Dir ->
  -- | A 'RoomEventFilter' to filter returned events with.
  Maybe RoomEventFilter ->
  -- | The Since value to start returning events from.
  T.Text ->
  -- | The maximum number of events to return. Default: 10.
  Maybe Int ->
  -- | The token to stop returning events at.
  Maybe Int ->
  MatrixIO PaginatedRoomMessages
getRoomMessages (RoomID rid) dir roomFilter fromToken limit toToken = do
  request <- mkRequest True $ "/_matrix/client/v3/rooms/" <> rid <> "/messages"
  let dir' = "dir=" <> renderDir dir
      filter' = BL.toStrict . mappend "filter=" . encode <$> roomFilter
      from' = encodeUtf8 $ "from=" <> fromToken
      limit' = BL.toStrict . mappend "limit=" . encode <$> limit
      to' = BL.toStrict . mappend "from=" . encode <$> toToken
      queryString = mappend "?" $ mconcat $ intersperse "&" $ [dir', from'] <> catMaybes [to', limit', filter']
  doRequest $ request {HTTP.queryString = queryString}

-- | Send arbitrary state events to a room. These events will be overwritten if
-- <room id>, <event type> and <state key> all match.
-- https://spec.matrix.org/v1.1/client-server-api/#put_matrixclientv3roomsroomidstateeventtypestatekey
sendRoomStateEvent :: RoomID -> EventType -> StateKey -> Value -> MatrixIO EventID
sendRoomStateEvent (RoomID rid) (EventType et) (StateKey key) event = do
  request <- mkRequest True $ "/_matrix/client/v3/rooms/" <> escapeUriComponent rid <> "/state/" <> escapeUriComponent et <> "/" <> escapeUriComponent key
  doRequest $
    request
      { HTTP.method = "PUT",
        HTTP.requestBody = HTTP.RequestBodyLBS $ encode event
      }

newtype TxnID = TxnID T.Text deriving (Show, Eq)

-- | This endpoint is used to send a message event to a room.
-- https://spec.matrix.org/v1.1/client-server-api/#put_matrixclientv3roomsroomidsendeventtypetxnid
sendMessage :: RoomID -> Event -> TxnID -> MatrixIO EventID
sendMessage (RoomID roomId) event (TxnID txnId) = do
  request <- mkRequest True path
  doRequest
    ( request
        { HTTP.method = "PUT",
          HTTP.requestBody = HTTP.RequestBodyLBS $ encode event
        }
    )
  where
    path = "/_matrix/client/r0/rooms/" <> roomId <> "/send/" <> eventId <> "/" <> txnId
    eventId = eventType event

redact :: RoomID -> EventID -> TxnID -> T.Text -> MatrixIO EventID
redact (RoomID rid) (EventID eid) (TxnID txnid) reason = do
  request <- mkRequest True $ "/_matrix/client/v3/rooms/" <> rid <> "/redact/" <> eid <> "/" <> txnid
  let body = object ["reason" .= String reason]
  doRequest $
    request
      { HTTP.method = "PUT",
        HTTP.requestBody = HTTP.RequestBodyLBS $ encode body
      }

-------------------------------------------------------------------------------
-- Room API Calls https://spec.matrix.org/v1.1/client-server-api/#rooms-1

-- | Create a new room with various configuration options.
-- https://spec.matrix.org/v1.1/client-server-api/#post_matrixclientv3createroom
createRoom :: RoomCreateRequest -> MatrixIO RoomID
createRoom rcr = do
  request <- mkRequest True "/_matrix/client/v3/createRoom"
  resp <-
    doRequest $
      request
        { HTTP.method = "POST",
          HTTP.requestBody = HTTP.RequestBodyLBS $ encode rcr
        }
  toRoomID resp
  where
    toRoomID :: CreateRoomResponse -> MatrixIO RoomID
    toRoomID crr =
      case (crrID crr, crrMessage crr) of
        (Just roomID, _) -> pure $ RoomID roomID
        (_, Just message) -> throwError $ MatrixError "UNKNOWN" message Nothing
        _ -> throwError $ MatrixError "UNKOWN" "" Nothing

newtype RoomAlias = RoomAlias T.Text deriving (Show, Eq, Ord, Hashable)

data ResolvedRoomAlias = ResolvedRoomAlias
  { roomAlias :: RoomAlias,
    -- | The room ID for this room alias.
    roomID :: RoomID,
    -- | A list of servers that are aware of this room alias.
    servers :: [T.Text]
  }
  deriving (Show)

-- | Boilerplate data type for an aeson instance
data RoomAliasMetadata = RoomAliasMetadata
  { ramRoomID :: RoomID,
    ramServers :: [T.Text]
  }

instance FromJSON RoomAliasMetadata where
  parseJSON = withObject "ResolvedRoomAlias" $ \o -> do
    ramRoomID <- fmap RoomID $ o .: "room_id"
    ramServers <- o .: "servers"
    pure $ RoomAliasMetadata {..}

-- | Requests that the server resolve a room alias to a room ID.
-- https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3directoryroomroomalias
resolveRoomAlias :: RoomAlias -> MatrixIO ResolvedRoomAlias
resolveRoomAlias r@(RoomAlias alias) = do
  request <- mkRequest True $ "/_matrix/client/v3/directory/room/" <> escapeUriComponent alias
  RoomAliasMetadata {..} <- doRequest $ request {HTTP.method = "GET"}
  pure $ ResolvedRoomAlias r ramRoomID ramServers

-- | Create a mapping of room alias to room ID.
-- https://spec.matrix.org/v1.1/client-server-api/#put_matrixclientv3directoryroomroomalias
setRoomAlias :: RoomAlias -> RoomID -> MatrixIO ()
setRoomAlias (RoomAlias alias) (RoomID roomId) = do
  request <- mkRequest True $ "/_matrix/client/v3/directory/room/" <> escapeUriComponent alias
  doRequest $
    request
      { HTTP.method = "PUT",
        HTTP.requestBody = HTTP.RequestBodyLBS $ encode $ object [("room_id" .= roomId)]
      }

-- | Delete a mapping of room alias to room ID.
-- https://spec.matrix.org/v1.1/client-server-api/#delete_matrixclientv3directoryroomroomalias
deleteRoomAlias :: RoomAlias -> MatrixIO ()
deleteRoomAlias (RoomAlias alias) = do
  request <- mkRequest True $ "/_matrix/client/v3/directory/room/" <> escapeUriComponent alias
  doRequest $ request {HTTP.method = "DELETE"}

data ResolvedAliases = ResolvedAliases [RoomAlias]

instance FromJSON ResolvedAliases where
  parseJSON = withObject "ResolvedAliases" $ \o -> do
    aliases <- o .: "aliases"
    pure $ ResolvedAliases (RoomAlias <$> aliases)

-- | Get a list of aliases maintained by the local server for the given room.
-- https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3roomsroomidaliases
getRoomAliases :: RoomID -> MatrixIO [RoomAlias]
getRoomAliases (RoomID rid) = do
  request <- mkRequest True $ "/_matrix/client/v3/rooms/" <> rid <> "/aliases"
  ResolvedAliases aliases <- doRequest $ request {HTTP.method = "GET"}
  pure aliases

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

-- | Returns a list of the user’s current rooms.
-- https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3joined_rooms
getJoinedRooms :: MatrixIO [RoomID]
getJoinedRooms = do
  request <- mkRequest True "/_matrix/client/r0/joined_rooms"
  response <- doRequest request
  pure $ unRooms response

newtype RoomID = RoomID T.Text deriving (Show, Eq, Ord, Hashable)

instance FromJSON RoomID where
  parseJSON (Object v) = RoomID <$> v .: "room_id"
  parseJSON _ = mzero

-- | Invites a user to participate in a particular room. They do not
-- start participating in the room until they actually join the room.
-- https://spec.matrix.org/v1.1/client-server-api/#post_matrixclientv3roomsroomidinvite
inviteToRoom :: RoomID -> UserID -> Maybe T.Text -> MatrixIO ()
inviteToRoom (RoomID rid) (UserID uid) reason = do
  request <- mkRequest True $ "/_matrix/client/v3/rooms/" <> rid <> "/invite"
  let body = object $ [("user_id", toJSON uid)] <> catMaybes [fmap (("reason",) . toJSON) reason]
  doRequest $
    request
      { HTTP.method = "POST",
        HTTP.requestBody = HTTP.RequestBodyLBS $ encode body
      }

-- | Note that this API takes either a room ID or alias, unlike 'joinRoomById'
-- https://spec.matrix.org/v1.1/client-server-api/#post_matrixclientv3joinroomidoralias
joinRoom :: T.Text -> MatrixIO RoomID
joinRoom roomName = do
  request <- mkRequest True $ "/_matrix/client/r0/join/" <> roomNameUrl
  doRequest (request {HTTP.method = "POST"})
  where
    roomNameUrl = decodeUtf8 . urlEncode True . encodeUtf8 $ roomName

-- | Starts a user participating in a particular room, if that user is
-- allowed to participate in that room.
-- https://spec.matrix.org/v1.1/client-server-api/#post_matrixclientv3roomsroomidjoin
joinRoomById :: RoomID -> MatrixIO RoomID
joinRoomById (RoomID roomId) = do
  request <- mkRequest True $ "/_matrix/client/r0/rooms/" <> roomId <> "/join"
  doRequest (request {HTTP.method = "POST"})

-- | This API “knocks” on the room to ask for permission to join, if
-- the user is allowed to knock on the room.
-- https://spec.matrix.org/v1.1/client-server-api/#post_matrixclientv3knockroomidoralias
knockOnRoom :: Either RoomID RoomAlias -> [T.Text] -> Maybe T.Text -> MatrixIO RoomID
knockOnRoom room servers reason = do
  request <- mkRequest True $ " /_matrix/client/v3/knock/" <> indistinct (bimap coerce coerce room)
  let body = object $ catMaybes [fmap (("reason",) . toJSON) reason]
  doRequest $
    request
      { HTTP.method = "POST",
        HTTP.requestBody = HTTP.RequestBodyLBS $ encode body,
        HTTP.queryString = encodeUtf8 $ "?server_name=" <> mconcat (intersperse "," servers)
      }

ensureEmptyObject :: Value -> ()
ensureEmptyObject value = case value of
  Object xs | xs == mempty -> ()
  _anyOther -> error $ "Unknown leave response: " <> show value

-- | Stops remembering a particular room.
-- https://spec.matrix.org/v1.1/client-server-api/#post_matrixclientv3roomsroomidforget
forgetRoom :: RoomID -> MatrixIO ()
forgetRoom (RoomID roomId) = do
  request <- mkRequest True $ "/_matrix/client/v3/rooms/" <> roomId <> "/forget"
  resp <- doRequest (request {HTTP.method = "POST"})
  pure $ ensureEmptyObject resp

-- | Stop participating in a particular room.
-- https://spec.matrix.org/v1.1/client-server-api/#post_matrixclientv3roomsroomidleave
leaveRoomById :: RoomID -> MatrixIO ()
leaveRoomById (RoomID roomId) = do
  request <- mkRequest True $ "/_matrix/client/r0/rooms/" <> roomId <> "/leave"
  resp <- doRequest (request {HTTP.method = "POST"})
  pure $ ensureEmptyObject resp

-- | Kick a user from the room.
-- https://spec.matrix.org/v1.1/client-server-api/#post_matrixclientv3roomsroomidkick
kickUser :: RoomID -> UserID -> Maybe T.Text -> MatrixIO ()
kickUser (RoomID roomId) (UserID uid) reason = do
  request <- mkRequest True $ "/_matrix/client/v3/rooms/" <> roomId <> "/kick"
  let body = object $ [("user_id", toJSON uid)] <> catMaybes [fmap (("reason",) . toJSON) reason]
  resp <-
    doRequest $
      request
        { HTTP.method = "POST",
          HTTP.requestBody = HTTP.RequestBodyLBS $ encode body
        }
  pure $ ensureEmptyObject resp

-- | Ban a user in the room. If the user is currently in the room, also kick them.
-- https://spec.matrix.org/v1.1/client-server-api/#post_matrixclientv3roomsroomidban
banUser :: RoomID -> UserID -> Maybe T.Text -> MatrixIO ()
banUser (RoomID roomId) (UserID uid) reason = do
  request <- mkRequest True $ "/_matrix/client/v3/rooms/" <> roomId <> "/ban"
  let body = object $ [("user_id", toJSON uid)] <> catMaybes [fmap (("reason",) . toJSON) reason]
  resp <-
    doRequest $
      request
        { HTTP.method = "POST",
          HTTP.requestBody = HTTP.RequestBodyLBS $ encode body
        }
  pure $ ensureEmptyObject resp

-- | Unban a user from the room. This allows them to be invited to the
-- room, and join if they would otherwise be allowed to join according
-- to its join rules.
-- https://spec.matrix.org/v1.1/client-server-api/#post_matrixclientv3roomsroomidunban
unbanUser :: RoomID -> UserID -> Maybe T.Text -> MatrixIO ()
unbanUser (RoomID roomId) (UserID uid) reason = do
  request <- mkRequest True $ "/_matrix/client/v3/rooms/" <> roomId <> "/unban"
  let body = object $ [("user_id", toJSON uid)] <> catMaybes [fmap (("reason",) . toJSON) reason]
  resp <-
    doRequest $
      request
        { HTTP.method = "POST",
          HTTP.requestBody = HTTP.RequestBodyLBS $ encode body
        }
  pure $ ensureEmptyObject resp

data Visibility = Public | Private
  deriving (Show)

instance ToJSON Visibility where
  toJSON = \case
    Public -> String "public"
    Private -> String "private"

instance FromJSON Visibility where
  parseJSON = withText "Visibility" $ \case
    "public" -> pure Public
    "private" -> pure Private
    _ -> mzero

newtype GetVisibility = GetVisibility {getVisibility :: Visibility}

instance FromJSON GetVisibility where
  parseJSON = withObject "GetVisibility" $ \o -> do
    getVisibility <- o .: "visibility"
    pure $ GetVisibility {..}

-- | Gets the visibility of a given room on the server’s public room directory.
-- https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3directorylistroomroomid
checkRoomVisibility :: RoomID -> MatrixIO Visibility
checkRoomVisibility (RoomID rid) = do
  request <- mkRequest True $ "/_matrix/client/v3/directory/list/room/" <> rid
  resp <- doRequest request
  pure $ getVisibility resp

-- | Sets the visibility of a given room in the server’s public room directory.
-- https://spec.matrix.org/v1.1/client-server-api/#put_matrixclientv3directorylistroomroomid
setRoomVisibility :: RoomID -> Visibility -> MatrixIO ()
setRoomVisibility (RoomID rid) visibility = do
  request <- mkRequest True $ "/_matrix/client/v3/directory/list/room/" <> rid
  let body = object $ [("visibility", toJSON visibility)]
  resp <-
    doRequest $
      request
        { HTTP.method = "PUT",
          HTTP.requestBody = HTTP.RequestBodyLBS $ encode body
        }
  pure $ ensureEmptyObject resp

-- | A pagination token from a previous request, allowing clients to
-- get the next (or previous) batch of rooms. The direction of
-- pagination is specified solely by which token is supplied, rather
-- than via an explicit flag.
newtype PaginationChunk = PaginationChunk {getChunk :: T.Text}
  deriving stock (Show)
  deriving newtype (ToJSON, FromJSON)

data Room = Room
  { aliases :: Maybe [T.Text],
    avatarUrl :: Maybe T.Text,
    canonicalAlias :: Maybe T.Text,
    guestCanJoin :: Bool,
    joinRule :: Maybe T.Text,
    name :: Maybe T.Text,
    numJoinedMembers :: Int,
    roomId :: RoomID,
    topic :: Maybe T.Text,
    worldReadable :: Bool
  }
  deriving (Show)

instance FromJSON Room where
  parseJSON = withObject "Room" $ \o -> do
    aliases <- o .:? "aliases"
    avatarUrl <- o .:? "avatar_url"
    canonicalAlias <- o .:? "canonical_alias"
    guestCanJoin <- o .: "guest_can_join"
    joinRule <- o .:? "join_rule"
    name <- o .:? "name"
    numJoinedMembers <- o .: "num_joined_members"
    roomId <- fmap RoomID $ o .: "room_id"
    topic <- o .:? "topic"
    worldReadable <- o .: "world_readable"
    pure $ Room {..}

data PublicRooms = PublicRooms
  { prChunk :: [Room],
    prNextBatch :: Maybe PaginationChunk,
    prPrevBatch :: Maybe PaginationChunk,
    prTotalRoomCountEstimate :: Maybe Int
  }
  deriving (Show)

instance FromJSON PublicRooms where
  parseJSON = withObject "PublicRooms" $ \o -> do
    prChunk <- o .: "chunk"
    prNextBatch <- o .:? "next_batch"
    prPrevBatch <- o .:? "prev_batch"
    prTotalRoomCountEstimate <- o .:? "total_room_count_estimate"
    pure $ PublicRooms {..}

-- | Lists the public rooms on the server.
-- https://spec.matrix.org/v1.1/client-server-api/#get_matrixclientv3publicrooms
getPublicRooms :: Maybe Int -> Maybe PaginationChunk -> MatrixIO PublicRooms
getPublicRooms limit chunk = do
  request <- mkRequest True "/_matrix/client/v3/publicRooms"
  let since = fmap (mappend "since=" . getChunk) chunk
      limit' = fmap (mappend "limit=" . tshow) limit
      queryString = encodeUtf8 $ mconcat $ intersperse "&" $ catMaybes [since, limit']
  doRequest $
    request {HTTP.queryString = queryString}

newtype ThirdPartyInstanceId = ThirdPartyInstanceId T.Text
  deriving (FromJSON, ToJSON)

-- | Lists the public rooms on the server, with optional filter.
-- https://spec.matrix.org/v1.1/client-server-api/#post_matrixclientv3publicrooms
getPublicRooms' :: Maybe Int -> Maybe PaginationChunk -> Maybe T.Text -> Maybe Bool -> Maybe ThirdPartyInstanceId -> MatrixIO PublicRooms
getPublicRooms' limit chunk searchTerm includeAllNetworks thirdPartyId = do
  request <- mkRequest True "/_matrix/client/v3/publicRooms"
  let filter' = object $ catMaybes [fmap (("generic_search_term",) . toJSON) searchTerm]
      since = fmap (("since",) . toJSON) chunk
      limit' = fmap (("limit",) . toJSON) limit
      includeAllNetworks' = fmap (("include_all_networks",) . toJSON) includeAllNetworks
      thirdPartyId' = fmap (("third_party_instance_id",) . toJSON) thirdPartyId
      body = object $ [("filter", filter')] <> catMaybes [since, limit', includeAllNetworks', thirdPartyId']
  doRequest $
    request
      { HTTP.method = "POST",
        HTTP.requestBody = HTTP.RequestBodyLBS $ encode body
      }

-------------------------------------------------------------------------------
-- https://matrix.org/docs/spec/client_server/latest#post-matrix-client-r0-user-userid-filter
newtype FilterID = FilterID T.Text deriving (Show, Eq, Hashable)

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
    efNotSenders :: Maybe [T.Text],
    efNotTypes :: Maybe [T.Text],
    efSenders :: Maybe [T.Text],
    efTypes :: Maybe [T.Text]
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
    refNotSenders :: Maybe [T.Text],
    refNotTypes :: Maybe [T.Text],
    refSenders :: Maybe [T.Text],
    refTypes :: Maybe [T.Text],
    refLazyLoadMembers :: Maybe Bool,
    refIncludeRedundantMembers :: Maybe Bool,
    refNotRooms :: Maybe [T.Text],
    refRooms :: Maybe [T.Text],
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
    sfNotSenders :: Maybe [T.Text],
    sfNotTypes :: Maybe [T.Text],
    sfSenders :: Maybe [T.Text],
    sfTypes :: Maybe [T.Text],
    sfLazyLoadMembers :: Maybe Bool,
    sfIncludeRedundantMembers :: Maybe Bool,
    sfNotRooms :: Maybe [T.Text],
    sfRooms :: Maybe [T.Text],
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
  { rfNotRooms :: Maybe [T.Text],
    rfRooms :: Maybe [T.Text],
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
  { filterEventFields :: Maybe [T.Text],
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
  -- | The userID, use 'getTokenOwner' to get it.
  UserID ->
  -- | The filter definition, use 'defaultFilter' to create one or use the 'messageFilter' example.
  Filter ->
  -- | The function returns a 'FilterID' suitable for the 'sync' function.
  MatrixIO FilterID
createFilter (UserID userID) body = do
  request <- mkRequest True path
  doRequest
    ( request
        { HTTP.method = "POST",
          HTTP.requestBody = HTTP.RequestBodyLBS $ encode body
        }
    )
  where
    path = "/_matrix/client/r0/user/" <> userID <> "/filter"

getFilter :: UserID -> FilterID -> MatrixIO Filter
getFilter (UserID userID) (FilterID filterID) =
  doRequest =<< mkRequest True path
  where
    path = "/_matrix/client/r0/user/" <> userID <> "/filter/" <> filterID

-------------------------------------------------------------------------------
-- https://matrix.org/docs/spec/client_server/latest#get-matrix-client-r0-sync
newtype Author = Author {unAuthor :: T.Text}
  deriving (Show, Eq)
  deriving newtype (FromJSON, ToJSON)

data RoomEvent = RoomEvent
  { reContent :: Event,
    reType :: T.Text,
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
    tsPrevBatch :: Maybe T.Text
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
  toJSON ef = String . tshow $ ef

instance FromJSON Presence where
  parseJSON v = case v of
    (String "offline") -> pure Offline
    (String "online") -> pure Online
    (String "unavailable") -> pure Unavailable
    _ -> mzero

data SyncResult = SyncResult
  { srNextBatch :: T.Text,
    srRooms :: Maybe SyncResultRoom
  }
  deriving (Show, Eq, Generic)

data SyncResultRoom = SyncResultRoom
  { srrJoin :: Maybe (Map T.Text JoinedRoomSync),
    srrInvite :: Maybe (Map T.Text InvitedRoomSync)
  }
  deriving (Show, Eq, Generic)

data InvitedRoomSync = InvitedRoomSync
  deriving (Show, Eq, Generic)

unFilterID :: FilterID -> T.Text
unFilterID (FilterID x) = x

-------------------------------------------------------------------------------
-- https://matrix.org/docs/spec/client_server/latest#forming-relationships-between-events

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
addReplyBody :: Author -> T.Text -> T.Text -> T.Text
addReplyBody (Author author) old reply =
  let oldLines = T.lines old
      headLine = "> <" <> author <> ">" <> maybe "" (mappend " ") (headMaybe oldLines)
      newBody = [headLine] <> map (mappend "> ") (tail' oldLines) <> [""] <> [reply]
   in T.dropEnd 1 $ T.unlines newBody

addReplyFormattedBody :: RoomID -> EventID -> Author -> T.Text -> T.Text -> T.Text
addReplyFormattedBody (RoomID roomID) (EventID eventID) (Author author) old reply =
  T.unlines
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
toFormattedBody :: T.Text -> T.Text
toFormattedBody = T.concatMap char
  where
    char x = case x of
      '<' -> "&lt;"
      '>' -> "&gt;"
      '&' -> "&amp;"
      _ -> T.singleton x

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

sync :: MonadIO m => Maybe FilterID -> Maybe T.Text -> Maybe Presence -> Maybe Int -> MatrixM m SyncResult
sync filterM' sinceM presenceM timeoutM = do
  request <- mkRequest True "/_matrix/client/r0/sync"
  doRequest (HTTP.setQueryString qs request)
  where
    toQs name = \case
      Nothing -> []
      Just v -> [(name, Just . encodeUtf8 $ v)]
    qs =
      toQs "filter" (unFilterID <$> filterM')
        <> toQs "since" sinceM
        <> toQs "set_presence" (tshow <$> presenceM)
        <> toQs "timeout" (tshow <$> timeoutM)

syncPoll ::
  (MonadMask m, MonadIO m) =>
  -- | A sync filter, use 'createFilter' to get one.
  Maybe FilterID ->
  -- | A since value, get it from a previous sync result using the 'srNextBatch' field.
  Maybe T.Text ->
  -- | Set the session presence.
  Maybe Presence ->
  -- | Your callback to handle sync result.
  (SyncResult -> m ()) ->
  -- | This function does not return unless there is an error.
  MatrixM m ()
syncPoll filterM' sinceM presenceM cb = go sinceM
  where
    go since = do
      sr <- retry $ sync filterM' since presenceM (Just 10_000)
      lift (cb sr)
      go (Just (srNextBatch sr))

-- | Extract room events from a sync result
getTimelines :: SyncResult -> [(RoomID, NonEmpty RoomEvent)]
getTimelines sr = foldrWithKey getEvents [] joinedRooms
  where
    getEvents :: T.Text -> JoinedRoomSync -> [(RoomID, NonEmpty RoomEvent)] -> [(RoomID, NonEmpty RoomEvent)]
    getEvents roomID jrs acc = case tsEvents (jrsTimeline jrs) of
      Just (x : xs) -> (RoomID roomID, x :| xs) : acc
      _ -> acc
    joinedRooms = fromMaybe mempty $ srRooms sr >>= srrJoin

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

instance ToJSON InvitedRoomSync where
  toJSON _ = object []

instance FromJSON InvitedRoomSync where
  parseJSON _ = pure InvitedRoomSync

instance ToJSON SyncResult where
  toJSON = genericToJSON aesonOptions

instance FromJSON SyncResult where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON SyncResultRoom where
  toJSON = genericToJSON aesonOptions

instance FromJSON SyncResultRoom where
  parseJSON = genericParseJSON aesonOptions

getAccountData' :: (FromJSON a) => UserID -> T.Text -> MatrixIO a
getAccountData' userID t =
  mkRequest True (accountDataPath userID t) >>= doRequest

setAccountData' :: (ToJSON a) => UserID -> T.Text -> a -> MatrixIO ()
setAccountData' userID t value = do
  request <- mkRequest True $ accountDataPath userID t
  void $
    ( doRequest $
        request
          { HTTP.method = "PUT",
            HTTP.requestBody = HTTP.RequestBodyLBS $ encode value
          } ::
        MatrixIO Aeson.Object
    )

accountDataPath :: UserID -> T.Text -> T.Text
accountDataPath (UserID userID) t =
  "/_matrix/client/r0/user/" <> userID <> "/account_data/" <> t

class (FromJSON a, ToJSON a) => AccountData a where
  accountDataType :: proxy a -> T.Text

getAccountData :: forall a. (AccountData a) => UserID -> MatrixIO a
getAccountData userID =
  getAccountData' userID $
    accountDataType (Proxy :: Proxy a)

setAccountData :: forall a. (AccountData a) => UserID -> a -> MatrixIO ()
setAccountData userID =
  setAccountData' userID $
    accountDataType (Proxy :: Proxy a)

-------------------------------------------------------------------------------
-- Utils

headMaybe :: [a] -> Maybe a
headMaybe xs = case xs of
  [] -> Nothing
  (x : _) -> Just x

tail' :: [a] -> [a]
tail' xs = case xs of
  [] -> []
  (_ : rest) -> rest

indistinct :: Either x x -> x
indistinct = id `either` id

tshow :: Show a => a -> T.Text
tshow = T.pack . show

escapeUriComponent :: T.Text -> T.Text
escapeUriComponent = T.pack . URI.escapeURIString URI.isUnreserved . T.unpack
