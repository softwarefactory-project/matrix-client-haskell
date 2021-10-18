{-# LANGUAGE OverloadedStrings #-}
-- | The matrix-bot entrypoint
module MatrixBot.Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Network.Matrix.Client ( createSession
                             , getTokenFromEnv
                             )
import System.Environment (getArgs)

import Network.Matrix.Bot

main :: IO ()
main = do
  [homeserver] <- getArgs
  token <- getTokenFromEnv "MATRIX_BOT_TOKEN"
  session <- createSession (T.pack homeserver) token
  matrixBot session $ startSync $ liftIO . print
  pure ()
