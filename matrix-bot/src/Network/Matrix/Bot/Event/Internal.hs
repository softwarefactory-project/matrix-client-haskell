module Network.Matrix.Bot.Event.Internal ( BotEvent(..)
                                         , extractBotEvents
                                         ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Network.Matrix.Client ( JoinedRoomSync( JoinedRoomSync
                                             , jrsTimeline
                                             )
                             , RoomEvent
                             , RoomID(RoomID)
                             , SyncResult( SyncResult
                                         , srRooms
                                         )
                             , SyncResultRoom( SyncResultRoom
                                             , srrInvite
                                             , srrJoin
                                             )
                             , TimelineSync (TimelineSync
                                            , tsEvents
                                            , tsPrevBatch
                                            )
                             )

data BotEvent = BotRoomTimelineReceivedEvent RoomID (Maybe T.Text)
              | BotRoomEvent RoomID RoomEvent
              | BotInvitationEvent RoomID
              deriving (Show)

extractBotEvents :: SyncResult -> [BotEvent]
extractBotEvents SyncResult{srRooms=Nothing} = []
extractBotEvents SyncResult{srRooms=Just SyncResultRoom{..}} =
  invites ++ joined
  where invites = maybe [] (map (BotInvitationEvent . RoomID) . M.keys) srrInvite
        joined = maybe [] (
          M.foldrWithKey (\roomID JoinedRoomSync{jrsTimeline} ->
                             (timelineEvents (RoomID roomID) jrsTimeline ++)) []) srrJoin
        timelineEvents roomID TimelineSync{..} =
          BotRoomTimelineReceivedEvent roomID tsPrevBatch
          : map (BotRoomEvent roomID) (concat tsEvents)
