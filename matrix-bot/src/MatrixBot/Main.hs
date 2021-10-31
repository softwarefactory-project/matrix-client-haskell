{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The matrix-bot entrypoint
module MatrixBot.Main where

import Control.Monad.Catch (MonadCatch)
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
    withSyncStartedAt Nothing $ pure ()
    (asyncGroup, failGroup) <- lift get
    routeSyncGroupEvent asyncGroup e
    routeSyncGroupEvent failGroup e
    routeAsyncEvent (\() -> liftIO $ putStrLn "Async!") ()
    routeAsyncEvent (\() -> liftIO $ fail "Async fail!") ()
    routeSyncEvent (\() -> liftIO (putStrLn "Sync!") >> ask >>= liftIO . print) ()
    routeSyncEvent (\() -> liftIO $ fail "Sync fail!") ()
  pure ()

initializeRouterState :: ( IsSyncGroupManager m
                         , MonadIO (MatrixBotBaseLevel m)
                         , MonadCatch (MatrixBotBaseLevel m)
                         , MonadResyncableMatrixBot (MatrixBotBaseLevel m)
                         )
                      => StateT (SyncGroup (MatrixBotBaseLevel m) BotEvent, SyncGroup (MatrixBotBaseLevel m) BotEvent) m a -> m a
initializeRouterState a = do
  succGroup <- mkAsyncGroup
  failGroup <- mkFailGroup
  evalStateT a (succGroup, failGroup)

mkAsyncGroup :: ( IsSyncGroupManager m
                , MonadIO (MatrixBotBaseLevel m)
                , MonadCatch (MatrixBotBaseLevel m)
                , MonadResyncableMatrixBot (MatrixBotBaseLevel m)
                )
             => m (SyncGroup (MatrixBotBaseLevel m) BotEvent)
mkAsyncGroup = newSyncGroup $ syncGroupHandler id $ const $ liftIO $ putStrLn "Sync group!"

mkFailGroup :: ( IsSyncGroupManager m
               , MonadIO (MatrixBotBaseLevel m)
               , MonadCatch (MatrixBotBaseLevel m)
               , MonadResyncableMatrixBot (MatrixBotBaseLevel m)
               , Show e
               )
            => m (SyncGroup (MatrixBotBaseLevel m) e)
mkFailGroup = newSyncGroup $ syncGroupHandler id $ \e -> liftIO $ print e >> putStrLn "Async will fail!" >> fail "sync group fail!"
