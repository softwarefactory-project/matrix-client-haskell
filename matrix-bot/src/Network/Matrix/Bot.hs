{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Matrix.Bot ( MatrixBotOptions(..)
                          , matrixBot
                          ) where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Network.Matrix.Client

import Network.Matrix.Bot.Async
import Network.Matrix.Bot.ErrorHandling
import Network.Matrix.Bot.Router
import Network.Matrix.Bot.State
import Network.Matrix.Bot.Sync

data MatrixBotOptions = forall r. MatrixBotOptions
     { initializeBotEnv :: forall m. (MatrixBotBase m) => m r
     , botRouter :: forall m. (MatrixBotBase m, MonadResyncableMatrixBot m, IsSyncGroupManager m)
                 => r -> BotEventRouter m
     }

matrixBot :: ClientSession
          -> MatrixBotOptions
          -> IO ()
matrixBot session MatrixBotOptions{..} = do
  userID <- retry (getTokenOwner session) >>= dieOnLeft "Could not determine own MXID"
  initialSyncToken <- retry (getInitialSyncToken session userID)
    >>= dieOnLeft "Could not retrieve saved sync token"
  liftIO $ print initialSyncToken
  runMatrixBot session userID initialSyncToken $ do
    r <- initializeBotEnv
    forever $ syncLoop (botRouter r) >>= logOnLeft "Error while syncing"
