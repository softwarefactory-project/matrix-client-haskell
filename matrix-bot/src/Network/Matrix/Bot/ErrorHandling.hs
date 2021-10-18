module Network.Matrix.Bot.ErrorHandling ( dieOnLeft
                                        , logOnLeft
                                        ) where

import Control.Monad.IO.Class ( MonadIO
                              , liftIO
                              )
import System.Exit (die)
import System.IO ( hPutStrLn
                 , stderr
                 )

dieOnLeft :: (Show e, MonadIO m) => String -> Either e a -> m a
dieOnLeft _ (Right x) = pure x
dieOnLeft general (Left specific) =
  liftIO $ die $ general ++ ": " ++ show specific

logOnLeft :: (Show e, MonadIO m) => String -> Either e a -> m ()
logOnLeft _ (Right _) = pure ()
logOnLeft general (Left specific) =
  liftIO $ hPutStrLn stderr $ general ++ ": " ++ show specific