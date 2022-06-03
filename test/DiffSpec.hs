{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module DiffSpec where

import Control.Monad.Writer
import Data.Aeson
import Data.List
import Data.Maybe
import Test.Hspec
import Text.RawString.QQ (r)
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import Diff
import Lib
import Types

spec :: Spec
spec = describe "diffSpec" $ parallel $ do
  findChangesSpec
  applyChangesSpec

findChangesSpec :: Spec
findChangesSpec = describe "findChanges" $ do
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

-- | Checks whether @expected@ is a subset of @actual@ (verifying keys and values).
contains' :: Eq v => JKeyMap v -> JKeyMap v -> Bool
expected `contains'` actual = M.null $ M.differenceWith notSame expected actual
  where notSame x y = if x /= y then Just x else Nothing

containsKeys :: [JKey] -> DiffMap -> Bool
expected `containsKeys` actual = null $ expected \\ M.keys (unDiffMap actual)

containsKeys' :: [JKey] -> JKeyMap v -> Bool
expected `containsKeys'` actual = null $ expected \\ M.keys actual

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

applyChangesSpec :: Spec
applyChangesSpec = describe "applyChanges" $ do
  it "applies unchanged keys as is" $ do
    let { diffMap = DiffMap . M.fromList . fmap (, NoChange) $
      [ JKey ["author"]
      , JKey ["nested", "object", "key"]
      , JKey ["nested", "object", "array"]
      ]
    }
    let updated = M.restrictKeys newTranslations (S.fromList . M.keys . unDiffMap $ diffMap)
    (fst . runWriter $ applyChanges currentTranslations newTranslations diffMap) `shouldSatisfy` contains' updated

  it "overwrites values for already translated keys" $ do
    let { diffMap = DiffMap . M.fromList . fmap (, NoChange) $
      [ JKey ["existing"]
      , JKey ["nested", "object", "existing"]
      ]
    }
    let updated = M.restrictKeys newTranslations (S.fromList . M.keys . unDiffMap $ diffMap)
    (fst . runWriter $ applyChanges currentTranslations newTranslations diffMap) `shouldSatisfy` contains' updated

  it "ignores keys for changed values" $ do
    let { diffMap = DiffMap . M.fromList . fmap (, ValueChange (JValueString "old") (JValueString "new")) $
      [ JKey ["author"]
      , JKey ["nested", "object", "key"]
      , JKey ["nested", "object", "array"]
      ]
    }
    let keys = M.keys . unDiffMap $ diffMap
    (fst . runWriter $ applyChanges currentTranslations newTranslations diffMap) `shouldNotSatisfy` containsKeys' keys

  it "ignores values for removed keys" $ do
    let { diffMap = DiffMap . M.fromList . fmap (, MissingValue (JValueString "old")) $
      [ JKey ["author"]
      , JKey ["nested", "object", "key"]
      , JKey ["nested", "object", "array"]
      ]
    }
    let keys = M.keys . unDiffMap $ diffMap
    (fst . runWriter $ applyChanges currentTranslations newTranslations diffMap) `shouldNotSatisfy` containsKeys' keys

  it "applies values for keys moved to other positions" $ do
    let { diffMap = DiffMap . M.fromList $
      [ (JKey ["author"], MovedValue . NE.fromList $ [JKey ["nested", "object", "existing"], JKey ["other"]])
      , (JKey ["nested", "object", "array"], MovedValue . NE.fromList $ [JKey ["key"], JKey ["nested", "new"]])
      ]
    }
    let updated = values . fromJust . decode $ [r|
{
  "key": [ "FOO", "BAR" ],
  "existing": "existing",
  "nested": {
    "object": {
      "existing": "Translated Author"
    },
    "new": [ "FOO", "BAR" ]
  },
  "other": "Translated Author"
}
    |]
    (fst . runWriter $ applyChanges currentTranslations newTranslations diffMap) `shouldBe` updated

  it "ignores unexpected keys" $ do
    let keys = [JKey ["ignore_me"], JKey ["nested", "object", "unexpected"]]
    let diffMap = DiffMap M.empty
    (fst . runWriter $ applyChanges currentTranslations newTranslations diffMap) `shouldNotSatisfy` containsKeys' keys

currentTranslations :: CurrentTranslations
currentTranslations = values . fromJust . decode $ [r|
{
  "key": "VALUE",
  "existing": "existing",
  "nested": {
    "object": {
      "existing": "e"
    }
  }
}
|]

newTranslations :: NewTranslations
newTranslations = values . fromJust . decode $ [r|
{
  "author": "Translated Author",
  "existing": "new existing",
  "nested": {
    "object": {
      "key": "42",
      "array": [ "FOO", "BAR" ],
      "existing": "E",
      "unexpected": "unexpected"
    }
  },
  "ignore_me": "true"
}
|]
