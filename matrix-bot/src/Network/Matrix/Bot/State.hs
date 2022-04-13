module Network.Matrix.Bot.State
  ( MonadMatrixBotBase,
    MonadMatrixBot (..),
    MonadResyncableMatrixBot (..),
    MatrixBotT,
    runMatrixBotT,
  )
where

import Control.Monad.Catch
  ( MonadCatch,
    MonadMask,
    MonadThrow,
  )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader
  ( ReaderT,
    asks,
    local,
    runReaderT,
  )
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT)
import qualified Data.Text as T
import Network.Matrix.Client
  ( ClientSession,
    UserID,
  )

type MonadMatrixBotBase m = (MonadMatrixBot m, MonadIO m, MonadMask m)

class (Monad m) => MonadMatrixBot m where
  clientSession :: m ClientSession
  default clientSession ::
    (m ~ m' n, MonadTrans m', MonadMatrixBot n) => m ClientSession
  clientSession = lift clientSession

  myUserID :: m UserID
  default myUserID :: (m ~ m' n, MonadTrans m', MonadMatrixBot n) => m UserID
  myUserID = lift myUserID

  syncedSince :: m (Maybe T.Text)
  default syncedSince ::
    (m ~ m' n, MonadTrans m', MonadMatrixBot n) =>
    m (Maybe T.Text)
  syncedSince = lift syncedSince

instance MonadMatrixBot m => MonadMatrixBot (ReaderT r m)

instance MonadMatrixBot m => MonadMatrixBot (StateT s m)

instance (MonadMatrixBot m, Monoid w) => MonadMatrixBot (WriterT w m)

class (MonadMatrixBot m) => MonadResyncableMatrixBot m where
  withSyncStartedAt :: Maybe T.Text -> m a -> m a
  default withSyncStartedAt ::
    (m ~ m' n, MFunctor m', MonadResyncableMatrixBot n) =>
    Maybe T.Text ->
    m a ->
    m a
  withSyncStartedAt syncToken = hoist $ withSyncStartedAt syncToken

instance MonadResyncableMatrixBot m => MonadResyncableMatrixBot (ReaderT r m)

instance MonadResyncableMatrixBot m => MonadResyncableMatrixBot (StateT r m)

instance
  (MonadResyncableMatrixBot m, Monoid w) =>
  MonadResyncableMatrixBot (WriterT w m)

data MatrixBotEnv = MatrixBotEnv
  { mbeUserID :: UserID,
    mbeSession :: ClientSession,
    mbeSyncedSince :: Maybe T.Text
  }

newtype MatrixBotT m a = MatrixBotT (ReaderT MatrixBotEnv m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadTrans,
      MonadIO,
      MonadCatch,
      MonadMask,
      MonadThrow,
      MonadUnliftIO
    )

runMatrixBotT ::
  ClientSession -> UserID -> Maybe T.Text -> MatrixBotT m a -> m a
runMatrixBotT mbeSession mbeUserID mbeSyncedSince (MatrixBotT ac) =
  runReaderT ac MatrixBotEnv {..}

instance Monad m => MonadMatrixBot (MatrixBotT m) where
  clientSession = MatrixBotT $ asks mbeSession

  myUserID = MatrixBotT $ asks mbeUserID

  syncedSince = MatrixBotT $ asks mbeSyncedSince

instance Monad m => MonadResyncableMatrixBot (MatrixBotT m) where
  withSyncStartedAt syncStart (MatrixBotT ac) =
    MatrixBotT $
      local (\env -> env {mbeSyncedSince = syncStart}) ac
