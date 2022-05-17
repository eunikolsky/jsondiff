{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module LibSpec where

import Data.Aeson
import System.FilePath
import Test.Hspec
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Lib

spec :: Spec
spec = describe "diff" $ do
  context "when translation file is empty" $
    it "returns the english file" $ do
      let empty = "{}"
      english <- B.readFile $ takeDirectory __FILE__ </> "files/en.json"
      actual <- diff english empty
      actual `shouldBe` english

  context "when translation file is also english" $
    it "returns empty output" $ do
      english <- B.readFile $ takeDirectory __FILE__ </> "files/en.json"
      (Just actual) <- decode . BL.fromStrict <$> diff english english
      let (Just expected) = decode "{}" :: Maybe Value
      actual `shouldBe` expected
