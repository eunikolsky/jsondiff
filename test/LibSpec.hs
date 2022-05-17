{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module LibSpec where

import qualified Data.ByteString as B
import System.FilePath
import Test.Hspec

import Lib

spec :: Spec
spec = describe "diff" $ do
  context "when translation file is empty" $
    it "returns the english file" $ do
      let empty = "{}"
      english <- B.readFile $ takeDirectory __FILE__ </> "files/en.json"
      actual <- diff english empty
      actual `shouldBe` english
