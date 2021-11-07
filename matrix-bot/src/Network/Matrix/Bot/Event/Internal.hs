module Network.Matrix.Bot.Event.Internal
    ( BotEvent(..)
    , extractBotEvents
    ) where

import qualified Data.Map.Strict       as M
import qualified Data.Text             as T
import           Network.Matrix.Client
                     ( JoinedRoomSync(JoinedRoomSync, jrsTimeline)
                     , RoomEvent
                     , RoomID(RoomID)
                     , SyncResult(SyncResult, srRooms)
                     , SyncResultRoom(SyncResultRoom, srrInvite, srrJoin)
                     , TimelineSync(TimelineSync, tsEvents, tsPrevBatch)
                     )

-- | An event that a matrix bot might want to react to. This doesn't
-- necessarily correspond to on e event as defined by the matrix
-- protocol, but is simply the unit of information the framework
-- passes to event handlers.
data BotEvent = BotRoomTimelineReceivedEvent RoomID (Maybe T.Text)
                -- ^ A new timeline for a room was received along with a
                -- sync token to retrieve previous history of the
                -- room. This is mostly interesting for bots that are
                -- interested in the history of rooms before the current
                -- sync started.
              | BotRoomEvent RoomID RoomEvent
                -- ^ A message has been received in a room.
              | BotInvitationEvent RoomID
                -- ^ An invitation to a room has been received.
    deriving ( Show )

extractBotEvents :: SyncResult -> [BotEvent]
extractBotEvents SyncResult{srRooms = Nothing} = []
extractBotEvents SyncResult{srRooms = Just SyncResultRoom{..}} =
    invites ++ joined
  where
    invites = maybe [] (map (BotInvitationEvent . RoomID) . M.keys) srrInvite

    joined =
        maybe []
              (M.foldrWithKey (\roomID JoinedRoomSync{jrsTimeline} ->
                               (timelineEvents (RoomID roomID) jrsTimeline ++))
                              [])
              srrJoin

    timelineEvents roomID TimelineSync{..} =
        BotRoomTimelineReceivedEvent roomID tsPrevBatch
        : map (BotRoomEvent roomID) (concat tsEvents)
