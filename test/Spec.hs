{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The matrix client specification tests
module Main (main) where

import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Text (Text)
import Network.Matrix.Client
import Network.Matrix.Internal
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "unit tests" $ do
  it "decode error" $
    (decodeResp "{\"errcode\": \"TEST\", \"error\":\"a error\"}" :: Either MatrixError String)
      `shouldBe` Left (MatrixError "TEST" "a error" Nothing)
  it "decode response" $
    decodeResp "{\"user_id\": \"@tristanc_:matrix.org\"}"
      `shouldBe` Right (UserID "@tristanc_:matrix.org")
  it "encode room message" $
    encodePretty (RoomMessageText (MessageText "Hello" Nothing Nothing))
      `shouldBe` "{\"body\":\"Hello\",\"msgtype\":\"m.text\"}"
  where
    encodePretty =
      Aeson.encodePretty'
        ( Aeson.defConfig {Aeson.confIndent = Aeson.Spaces 0, Aeson.confCompare = compare @Text}
        )
