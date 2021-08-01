{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The matrix client specification tests
module Main (main) where

import Control.Monad (void)
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Text (Text)
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Network.Matrix.Client
import Network.Matrix.Internal
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "unit tests" $ do
  it "decode unknown" $
    (decodeResp "" :: Maybe (Either MatrixError String))
      `shouldBe` Nothing
  it "decode error" $
    (decodeResp "{\"errcode\": \"TEST\", \"error\":\"a error\"}" :: Maybe (Either MatrixError String))
      `shouldBe` (Just . Left $ MatrixError "TEST" "a error" Nothing)
  it "decode response" $
    decodeResp "{\"user_id\": \"@tristanc_:matrix.org\"}"
      `shouldBe` (Just . Right $ UserID "@tristanc_:matrix.org")
  it "encode room message" $
    encodePretty (RoomMessageText (MessageText "Hello" Nothing Nothing))
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
      MkSystemTime start _ <- getSystemTime
      void action
      MkSystemTime end _ <- getSystemTime
      (end - start) `shouldSatisfy` (`op` 1)
    encodePretty =
      Aeson.encodePretty'
        ( Aeson.defConfig {Aeson.confIndent = Aeson.Spaces 0, Aeson.confCompare = compare @Text}
        )
