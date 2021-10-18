{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Matrix.Bot.Handler ( BotEvent(..)
                                  , BotEventHandler
                                  , withAsyncEventRouter
                                  ) where

import Control.Concurrent.Async.Lifted (withAsync)
import Control.Concurrent.Chan ( Chan
                               , newChan
                               , readChan
                               , writeChan
                               )
import Control.Monad (forever)
import Control.Monad.IO.Class ( liftIO
                              )
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

import Network.Matrix.Bot.State

data BotEvent = BotRoomTimelineReceivedEvent RoomID (Maybe T.Text)
              | BotRoomEvent RoomID RoomEvent
              | BotInvitationEvent RoomID
              deriving (Show)

type BotEventHandler = forall m. (MatrixBotSynced m) => BotEvent -> m ()

withAsyncEventRouter :: (MatrixBotSynced m)
                     => BotEventHandler
                     -> ((SyncResult -> m ()) -> m a)
                     -> m a
withAsyncEventRouter handler ac = do
  syncResultChan <- liftIO newChan
  withAsync (asyncEventRouter syncResultChan handler) $
    const $ ac $ liftIO . writeChan syncResultChan

asyncEventRouter :: (MatrixBotSynced m)
                 => Chan SyncResult
                 -> BotEventHandler
                 -> m a
asyncEventRouter srChan handler =
  forever $ liftIO (readChan srChan) >>= mapM_ handler . extractBotEvents

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
