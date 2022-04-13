{-# LANGUAGE OverloadedStrings #-}

-- | The matrix-bot entrypoint
module MatrixBot.Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Network.Matrix.Bot
import Network.Matrix.Bot.EventGroup
import Network.Matrix.Bot.Router
import Network.Matrix.Client
  ( createSession,
    getTokenFromEnv,
  )
import System.Environment (getArgs)

main :: IO ()
main = do
  [homeserver] <- getArgs
  token <- getTokenFromEnv "MATRIX_BOT_TOKEN"
  session <- createSession (T.pack homeserver) token
  runMatrixBot session $
    MatrixBot
      { initializeBotEnv = pure (42 :: Int),
        botRouter = \r ->
          customRouter initRouterState $ \s@(asyncGroup, failGroup) e -> do
            withSyncStartedAt Nothing $ pure ()
            routeGroupEvent asyncGroup e
            routeGroupEvent failGroup e
            routeAsyncEvent (\() -> liftIO $ putStrLn "Async!") ()
            routeAsyncEvent (\() -> liftIO $ fail "Async fail!") ()
            routeSyncEvent
              ( \() ->
                  liftIO (putStrLn "Sync!")
                    >> liftIO (print r)
              )
              ()
            routeSyncEvent (\() -> liftIO $ fail "Sync fail!") ()
            pure s
      }
  pure ()

initRouterState ::
  (MonadEventGroupManager m) =>
  m (EventGroup BotEvent, EventGroup BotEvent)
initRouterState = (,) <$> mkAsyncGroup <*> mkFailGroup

mkAsyncGroup :: (MonadEventGroupManager m) => m (EventGroup BotEvent)
mkAsyncGroup = newEventGroup $
  groupHandler (pure ()) $
    \_ _ -> liftIO $ putStrLn "Sync group!"

mkFailGroup :: (MonadEventGroupManager m, Show e) => m (EventGroup e)
mkFailGroup = newEventGroup $
  groupHandler (pure ()) $ \_ e ->
    liftIO $ print e >> putStrLn "Async will fail!" >> fail "sync group fail!"
