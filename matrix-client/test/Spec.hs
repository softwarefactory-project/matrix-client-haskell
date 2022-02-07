{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The matrix client specification tests
module Main (main) where

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Either (isLeft)
import Data.Text (Text, pack)
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Network.Matrix.Client
import Network.Matrix.Internal
import System.Environment (lookupEnv)
import Test.Hspec
import Control.Monad.Except

main :: IO ()
main = do
  env <- fmap (fmap pack) <$> traverse lookupEnv ["HOMESERVER_URL", "PRIMARY_TOKEN", "SECONDARY_TOKEN"]
  runIntegration <- case env of
    [Just url, Just tok1, Just tok2] -> do
      sess1 <- createSession url (MatrixToken tok1)
      sess2 <- createSession url (MatrixToken tok2)
      pure $ integration sess1 sess2
    _ -> do
      putStrLn "Skipping integration test"
      pure $ pure mempty
  hspec (parallel $ spec >> runIntegration)

integration :: ClientSession -> ClientSession -> Spec
integration sess1 sess2 = do
  describe "integration tests" $ do
    it "create room" $ do
      resp <- runMatrixM sess1 $ do
        createRoom
          ( RoomCreateRequest
              { rcrPreset = PublicChat,
                rcrRoomAliasName = "test",
                rcrName = "matrix-client-haskell-test",
                rcrTopic = "Testing matrix-client-haskell"
              }
          )
      case resp of
        Left err -> meError err `shouldBe` "Alias already exists"
        Right (RoomID roomID') -> roomID' `shouldSatisfy` (/= mempty)
    it "join room" $ do 
      resp <- runMatrixM sess1 $joinRoom "#test:localhost"
      case resp of
        Left err -> error (show err)
        Right (RoomID roomID') -> roomID' `shouldSatisfy` (/= mempty)
      resp' <- runMatrixM sess2 $ joinRoom "#test:localhost"
      case resp' of
        Left err -> error (show err)
        Right (RoomID roomID') -> roomID' `shouldSatisfy` (/= mempty)
    it "send message and reply" $ do
      result <- runMatrixM sess2 $ do
        -- Flush previous events
        sr <- sync Nothing Nothing Nothing Nothing
        [room] <- getJoinedRooms 
        let msg body = RoomMessageText $ MessageText body TextType Nothing Nothing
        let since = srNextBatch sr
        eventID <- sendMessage room (EventRoomMessage $ msg "Hello") (TxnID since)
        reply <- sendMessage room (EventRoomReply eventID $ msg "Hi!") (TxnID since)
        pure (reply, eventID)
      case result of
        Left err -> error (show err)
        Right (reply, eventID) -> reply `shouldNotBe` eventID
  it "does not retry on success" $
    checkPause (<=) $ do
      res <- runMatrixM sess1 $ retry (pure True)
      res `shouldBe` pure True
  it "does not retry on regular failure" $
    checkPause (<=) $ do
      let resp = MatrixError "test" "error" Nothing
      res <- runMatrixM sess1 $ retry (throwError resp :: MatrixIO Int)
      res `shouldBe` Left resp
  it "retry on rate limit failure" $
    checkPause (>=) $ do
      let resp = MatrixError "M_LIMIT_EXCEEDED" "error" (Just 1000)
      (runMatrixM sess1 $ retryWithLog 1 (const $ pure ()) (throwError resp))
        `shouldThrow` rateLimitSelector
  where
    rateLimitSelector :: MatrixException -> Bool
    rateLimitSelector MatrixRateLimit = True
    checkPause op action = do
      MkSystemTime start' _ <- getSystemTime
      void action
      MkSystemTime end' _ <- getSystemTime
      (end' - start') `shouldSatisfy` (`op` 1)

spec :: Spec
spec = describe "unit tests" $ do
  it "decode unknown" $
    (decodeResp "" :: Either String (Either MatrixError String))
      `shouldSatisfy` isLeft
  it "decode error" $
    (decodeResp "{\"errcode\": \"TEST\", \"error\":\"a error\"}" :: Either String (Either MatrixError String))
      `shouldBe` (Right . Left $ MatrixError "TEST" "a error" Nothing)
  it "decode response" $
    decodeResp "{\"user_id\": \"@tristanc_:matrix.org\"}"
      `shouldBe` (Right . Right $ UserID "@tristanc_:matrix.org")
  it "decode reply" $ do
    resp <- decodeResp <$> BS.readFile "test/data/message-reply.json"
    case resp of
      Right (Right (EventRoomReply eventID (RoomMessageText message))) -> do
        eventID `shouldBe` EventID "$eventID"
        mtBody message `shouldBe` "> <@tristanc_:matrix.org> :hello\n\nHello there!"
      _ -> error $ show resp
  it "decode edit" $ do
    resp <- decodeResp <$> BS.readFile "test/data/message-edit.json"
    case resp of
      Right (Right (EventRoomEdit (eventID, RoomMessageText srcMsg) (RoomMessageText message))) -> do
        eventID `shouldBe` EventID "$eventID"
        mtBody srcMsg `shouldBe` " * > :typo"
        mtBody message `shouldBe` "> :hello"
      _ -> error $ show resp
  it "encode room message" $
    encodePretty (RoomMessageText (MessageText "Hello" TextType Nothing Nothing))
      `shouldBe` "{\"body\":\"Hello\",\"msgtype\":\"m.text\"}"
  where
    encodePretty =
      Aeson.encodePretty'
        ( Aeson.defConfig {Aeson.confIndent = Aeson.Spaces 0, Aeson.confCompare = compare @Text}
        )
