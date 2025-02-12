{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The matrix client specification tests
module Main (main) where

import Control.Monad (void)
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Either (isLeft)
import Data.Text (Text, pack)
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Network.Matrix.Client
import Network.Matrix.Internal
import System.Environment (lookupEnv)
import Test.Hspec

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
    hspec (parallel spec >> runIntegration)

integration :: ClientSession -> ClientSession -> Spec
integration sess1 sess2 = do
    describe "integration tests" $ do
        it "create room" $ do
            resp <-
                createRoom
                    sess1
                    ( RoomCreateRequest
                        { rcrPreset = PublicChat
                        , rcrRoomAliasName = "test"
                        , rcrName = "matrix-client-haskell-test"
                        , rcrTopic = "Testing matrix-client-haskell"
                        }
                    )
            case resp of
                Left err -> meError err `shouldBe` "Alias already exists"
                Right (RoomID room) -> room `shouldSatisfy` (/= mempty)
        it "join room" $ do
            resp <- joinRoom sess1 "#test:localhost"
            case resp of
                Left err -> error (show err)
                Right (RoomID room) -> room `shouldSatisfy` (/= mempty)
            resp' <- joinRoom sess2 "#test:localhost"
            case resp' of
                Left err -> error (show err)
                Right (RoomID room) -> room `shouldSatisfy` (/= mempty)
        it "send message and reply" $ do
            -- Flush previous events
            Right sr <- sync sess2 Nothing Nothing Nothing Nothing
            Right (room : _) <- getJoinedRooms sess1
            let msg body = RoomMessageText $ MessageText body TextType Nothing Nothing
            let since = srNextBatch sr
            Right eventID <- sendMessage sess1 room (EventRoomMessage $ msg "Hello") (TxnID since)
            Right reply <- sendMessage sess2 room (EventRoomReply eventID $ msg "Hi!") (TxnID since)
            reply `shouldNotBe` eventID

        it "invite private room" $ do
            Right room <- createRoom sess1 $ RoomCreateRequest PrivateChat "private" "private-test" "A test"
            Right user <- getTokenOwner sess2
            Right inviteResult <- inviteToRoom sess1 room user (Just "Welcome!")
            inviteResult `shouldBe` ()

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
    it "does not retry on success" $
        checkPause (<=) $ do
            let resp = Right True
            res <- retry (pure resp)
            res `shouldBe` resp
    it "does not retry on regular failre" $
        checkPause (<=) $ do
            let resp = Left $ MatrixError "test" "error" Nothing
            res <- (retry (pure resp) :: MatrixIO Int)
            res `shouldBe` resp
    it "retry on rate limit failure" $
        checkPause (>=) $ do
            let resp = Left $ MatrixError "M_LIMIT_EXCEEDED" "error" (Just 1000)
            (retryWithLog 1 (const $ pure ()) (pure resp) :: MatrixIO Int)
                `shouldThrow` rateLimitSelector
  where
    rateLimitSelector :: MatrixException -> Bool
    rateLimitSelector MatrixRateLimit = True
    checkPause op action = do
        MkSystemTime startTS _ <- getSystemTime
        void action
        MkSystemTime endTS _ <- getSystemTime
        (endTS - startTS) `shouldSatisfy` (`op` 1)
    encodePretty =
        Aeson.encodePretty'
            ( Aeson.defConfig{Aeson.confIndent = Aeson.Spaces 0, Aeson.confCompare = compare @Text}
            )
