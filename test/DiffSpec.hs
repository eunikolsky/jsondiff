{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module DiffSpec where

import Data.Aeson
import Data.List
import Data.Maybe
import Test.Hspec
import Text.RawString.QQ (r)
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE

import Diff
import Lib
import Types

spec :: Spec
spec = describe "findChanges" $ parallel $ do
  it "detects no changes" $ do
    let { diffMap = DiffMap . M.fromList . fmap (, NoChange) $
      [ JKey ["key"]
      , JKey ["nested", "object", "key"]
      , JKey ["nested", "object", "array"]
      ]
    }
    findChanges old current `shouldSatisfy` contains diffMap

  it "detects value changes" $ do
    let { diffMap = DiffMap . M.fromList $
      [ (JKey ["author"], ValueChange (JValueString "Adam Ford") (JValueString "Zaphod Beeblebrox"))
      , (JKey ["nested", "object", "the_answer"], ValueChange (JValueString "0") (JValueString "42"))
      , (JKey ["nested", "vehicles"], ValueChange (JValueArray ["bus"]) (JValueArray ["bicycle", "bus"]))
      ]
    }
    findChanges old current `shouldSatisfy` contains diffMap

  it "detects removed values" $ do
    let { diffMap = DiffMap . M.fromList $
      [ (JKey ["will_remove"], MissingValue (JValueString ""))
      , (JKey ["nested", "object", "more", "array"], MissingValue (JValueArray ["baz"]))
      ]
    }
    findChanges old current `shouldSatisfy` contains diffMap

  it "detects moved values" $ do
    let { diffMap = DiffMap . M.fromList $
      [ (JKey ["old_key"], MovedValue . NE.fromList $ [JKey ["nested", "object", "earth"]])
      , (JKey ["nested", "object", "move_me"], MovedValue . NE.fromList $ [JKey ["root", "move_me"]])
      ]
    }
    findChanges old current `shouldSatisfy` contains diffMap

  it "detects moved values into multiple new keys" $ do
    let { diffMap = DiffMap . M.fromList $
      [ (JKey ["original"], MovedValue . NE.fromList $ [JKey ["nested", "object", "original_here"], JKey ["root", "and_here"]])
      ]
    }
    findChanges old current `shouldSatisfy` contains diffMap

  it "ignores new values" $ do
    let { newKeys =
      [ JKey ["new", "nested", "object"]
      , JKey ["root", "ignore_me"]
      ]
    }
    findChanges old current `shouldNotSatisfy` containsKeys newKeys

-- | Checks whether @expected@ is a subset of @actual@ (verifying keys and values).
contains :: DiffMap -> DiffMap -> Bool
expected `contains` actual = M.null $ M.differenceWith notSame (unDiffMap expected) (unDiffMap actual)
  where notSame x y = if x /= y then Just x else Nothing

containsKeys :: [JKey] -> DiffMap -> Bool
expected `containsKeys` actual = null $ expected \\ M.keys (unDiffMap actual)

old :: OldEnglish
old = values . fromJust . decode $ [r|
{
  "key": "value",
  "author": "Adam Ford",
  "will_remove": "",
  "old_key": "Earth",
  "original": "original",
  "nested": {
    "object": {
      "key": "value",
      "array": [ "foo", "bar" ],
      "the_answer": "0",
      "more": {
        "array": [ "baz" ]
      },
      "move_me": "!"
    },
    "vehicles": [ "bus" ]
  }
}
|]

current :: CurrentEnglish
current = values . fromJust . decode $ [r|
{
  "key": "value",
  "author": "Zaphod Beeblebrox",
  "nested": {
    "object": {
      "key": "value",
      "array": [ "foo", "bar" ],
      "the_answer": "42",
      "earth": "Earth",
      "original_here": "original"
    },
    "vehicles": [ "bicycle", "bus" ]
  },
  "root": {
    "move_me": "!",
    "and_here": "original",
    "ignore_me": [ "ignore" ]
  },
  "new": {
    "nested": {
      "object": "new"
    }
  }
}
|]
