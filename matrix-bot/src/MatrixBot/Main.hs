{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The matrix-bot entrypoint
module MatrixBot.Main where

import Control.Monad.IO.Class ( MonadIO
                              , liftIO
                              )
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader ( ask
                                  , runReaderT
                                  )
import Control.Monad.Trans.State ( StateT
                                 , evalStateT
                                 , get
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
import Network.Matrix.Bot.State

main :: IO ()
main = do
  [homeserver] <- getArgs
  token <- getTokenFromEnv "MATRIX_BOT_TOKEN"
  session <- createSession (T.pack homeserver) token
  matrixBot session $ MatrixBotOptions (`runReaderT` (42 :: Int)) $
    hoistRouter initializeRouterState $ customRouter $ \e -> do
    asyncGroup <- lift get
    routeSyncGroupEvent asyncGroup e
    routeAsyncEvent (\() -> liftIO $ putStrLn "Async!") ()
    routeAsyncEvent (\() -> liftIO $ fail "Async fail!") ()
    routeSyncEvent (\e' -> liftIO (print e') >> ask >>= liftIO . print) e
  pure ()

initializeRouterState :: (IsSyncGroupManager m, MonadIO (MatrixBotBaseLevel m))
                      => StateT (SyncGroup (MatrixBotBaseLevel m) BotEvent) m a -> m a
initializeRouterState a = mkAsyncGroup >>= evalStateT a

mkAsyncGroup :: (IsSyncGroupManager m, MonadIO (MatrixBotBaseLevel m))
             => m (SyncGroup (MatrixBotBaseLevel m) BotEvent)
mkAsyncGroup = newSyncGroup $ syncGroupHandler id $ liftIO . print
