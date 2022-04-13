module Network.Matrix.Bot
  ( -- * Running a Matrix bot
    MatrixBot (..),
    runMatrixBot,

    -- * Handling events
    BotEventRouter,
    customRouter,
    routeSyncEvent,
    routeAsyncEvent,

    -- * Inspecting events
    BotEvent (..),

    -- * Misc
    MonadMatrixBot (..),
    MonadMatrixBotBase,
    MonadResyncableMatrixBot (..),
  )
where

import Control.Monad (forever)
import Network.Matrix.Bot.ErrorHandling
import Network.Matrix.Bot.Event
import Network.Matrix.Bot.EventGroup
import Network.Matrix.Bot.Router
import Network.Matrix.Bot.State
import Network.Matrix.Bot.Sync
import Network.Matrix.Client

data MatrixBot = forall r.
  MatrixBot
  { initializeBotEnv :: forall m. (MonadMatrixBotBase m) => m r,
    botRouter ::
      forall m.
      ( MonadMatrixBotBase m,
        MonadResyncableMatrixBot m,
        MonadEventGroupManager m
      ) =>
      r ->
      BotEventRouter m
  }

runMatrixBot :: ClientSession -> MatrixBot -> IO ()
runMatrixBot session MatrixBot {..} = do
  userID <-
    retry (getTokenOwner session)
      >>= dieOnLeft "Could not determine own MXID"
  initialSyncToken <-
    retry (getInitialSyncToken session userID)
      >>= dieOnLeft "Could not retrieve saved sync token"
  runMatrixBotT session userID initialSyncToken $ do
    r <- initializeBotEnv
    forever $ syncLoop (botRouter r) >>= logOnLeft "Error while syncing"
