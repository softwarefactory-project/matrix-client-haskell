{-# LANGUAGE OverloadedStrings #-}
-- | The matrix-bot entrypoint
module MatrixBot.Main where

import Control.Monad.IO.Class ( MonadIO
                              , liftIO
                              )
import Control.Monad.Trans.Reader ( ask
                                  , runReaderT
                                  )
import qualified Data.Text as T
import Network.Matrix.Client ( createSession
                             , getTokenFromEnv
                             )
import System.Environment (getArgs)

import Network.Matrix.Bot
import Network.Matrix.Bot.Async
import Network.Matrix.Bot.Event
import Network.Matrix.Bot.Router

main :: IO ()
main = do
  [homeserver] <- getArgs
  token <- getTokenFromEnv "MATRIX_BOT_TOKEN"
  session <- createSession (T.pack homeserver) token
  matrixBot session $ MatrixBotOptions (`runReaderT` (42 :: Int)) $
    withExtraState (pure Nothing) $ customRouter $ \e -> do
    asyncGroup <- getAsyncGroup
    routeAsyncEvent asyncGroup e
    routeSyncEvent (\e' -> liftIO (print e') >> ask >>= liftIO . print) e
  pure ()

getAsyncGroup :: (IsEventRouter m s n, MonadIO n) => m (SyncGroup n BotEvent)
getAsyncGroup = newSyncGroup go
  where go getNextEvent = getNextEvent >>= maybe (pure ()) (liftIO . print . fst)
