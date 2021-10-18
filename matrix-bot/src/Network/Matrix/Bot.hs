{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Network.Matrix.Bot ( matrixBot
                          , startSync
                          ) where

import Network.Matrix.Client

import Network.Matrix.Bot.ErrorHandling
import Network.Matrix.Bot.Handler
import Network.Matrix.Bot.State
import Network.Matrix.Bot.Sync

matrixBot :: ClientSession
          -> (forall m. (MatrixBotBase m) => m ())
          -> IO ()
matrixBot session body = do
  userID <- retry (getTokenOwner session) >>= dieOnLeft "Could not determine own MXID"
  runHasSessionT session userID body

startSync :: (MatrixBotBase m)
          => BotEventHandler
          -> m ()
startSync handler = syncLoop handler >>= dieOnLeft "Error during sync"
