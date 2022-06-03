{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module LibSpec where

import Data.Aeson
import Test.Hspec
import Text.RawString.QQ (r)
import qualified Data.ByteString.Lazy as BL

import Lib

spec :: Spec
spec = describe "diff" $ parallel $ do
  context "when translation file is empty" $
    it "returns the english file" $ do
      let empty = "{}"
      let (Just actual) = decode $ diff english empty
          (Just expected) = decode english :: Maybe Value
      actual `shouldBe` expected

  context "when translation file is also english" $
    it "returns empty output" $ do
      let (Just actual) = decode $ diff english english
          (Just expected) = decode "{}" :: Maybe Value
      actual `shouldBe` expected

  context "when translation file is not empty" $
    it "returns the keys missing from the translation file" $ do
      let (Just actual) = decode $ diff english translation
          (Just expected) = decode expectedDiff :: Maybe Value
      actual `shouldBe` expected

english :: BL.ByteString
english = [r|
{
  "main": {
    "no": "no",
    "save": "Save",
    "yes": "yes"
  },
  "group": {
    "inside": {
      "group": {
        "cancel": "Cancel"
      }
    },
    "remove": "remove"
  },
  "root": "Root"
}
|]

translation :: BL.ByteString
translation = [r|
{
  "main": {
    "save": "1234"
  },
  "group": {
    "remove": "abcd"
  }
}
|]

expectedDiff :: BL.ByteString
expectedDiff = [r|
{
  "main": {
    "no": "no",
    "yes": "yes"
  },
  "group": {
    "inside": {
      "group": {
        "cancel": "Cancel"
      }
    }
  },
  "root": "Root"
}
|]
