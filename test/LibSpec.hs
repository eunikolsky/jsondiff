{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module LibSpec where

import Data.Aeson
import System.FilePath
import Test.Hspec
import qualified Data.ByteString.Lazy as BL

import Lib

spec :: Spec
spec = describe "diff" $ do
  context "when translation file is empty" $
    it "returns the english file" $ do
      let empty = "{}"
      english <- BL.readFile $ takeDirectory __FILE__ </> "files/en.json"
      let (Just actual) = decode $ diff english empty
          (Just expected) = decode english :: Maybe Value
      actual `shouldBe` expected

  context "when translation file is also english" $
    it "returns empty output" $ do
      english <- BL.readFile $ takeDirectory __FILE__ </> "files/en.json"
      let (Just actual) = decode $ diff english english
          (Just expected) = decode "{}" :: Maybe Value
      actual `shouldBe` expected

  context "when translation file is not empty" $
    it "returns the keys missing from the translation file" $ do
      english <- BL.readFile $ takeDirectory __FILE__ </> "files/en.json"
      translation <- BL.readFile $ takeDirectory __FILE__ </> "files/translation.json"
      let (Just actual) = decode $ diff english translation
      (Just expected) <- decodeFileStrict $ takeDirectory __FILE__ </> "files/diff.json" :: IO (Maybe Value)
      actual `shouldBe` expected
