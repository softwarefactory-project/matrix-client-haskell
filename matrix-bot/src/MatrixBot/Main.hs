{-# LANGUAGE OverloadedStrings #-}

-- | The matrix-bot entrypoint
module MatrixBot.Main where

import           Control.Monad.IO.Class    ( liftIO )
import qualified Data.Text                 as T
import           Network.Matrix.Client     ( createSession, getTokenFromEnv )
import           System.Environment        ( getArgs )

import           Network.Matrix.Bot
import           Network.Matrix.Bot.Async
import           Network.Matrix.Bot.Router

main :: IO ()
main = do
    [ homeserver ] <- getArgs
    token <- getTokenFromEnv "MATRIX_BOT_TOKEN"
    session <- createSession (T.pack homeserver) token
    runMatrixBot session $ MatrixBot
        { initializeBotEnv = pure (42 :: Int)
        , botRouter        = \r ->
              customRouter initRouterState $ \s@(asyncGroup, failGroup) e -> do
                  withSyncStartedAt Nothing $ pure ()
                  routeSyncGroupEvent asyncGroup e
                  routeSyncGroupEvent failGroup e
                  routeAsyncEvent (\() -> liftIO $ putStrLn "Async!") ()
                  routeAsyncEvent (\() -> liftIO $ fail "Async fail!") ()
                  routeSyncEvent (\() -> liftIO (putStrLn "Sync!")
                                  >> liftIO (print r))
                                 ()
                  routeSyncEvent (\() -> liftIO $ fail "Sync fail!") ()
                  pure s
        }
    pure ()

initRouterState
    :: (MonadSyncGroupManager m) => m (SyncGroup BotEvent, SyncGroup BotEvent)
initRouterState = (,) <$> mkAsyncGroup <*> mkFailGroup

mkAsyncGroup :: (MonadSyncGroupManager m) => m (SyncGroup BotEvent)
mkAsyncGroup = newSyncGroup $ syncGroupHandler (pure ()) $
    \_ _ -> liftIO $ putStrLn "Sync group!"

mkFailGroup :: (MonadSyncGroupManager m, Show e) => m (SyncGroup e)
mkFailGroup = newSyncGroup $ syncGroupHandler (pure ()) $ \_ e ->
    liftIO $ print e >> putStrLn "Async will fail!" >> fail "sync group fail!"
