{-# LANGUAGE OverloadedStrings #-}

-- | The matrix client specification tests
module Main (main) where

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
      `shouldBe` Right (WhoAmI "@tristanc_:matrix.org")
