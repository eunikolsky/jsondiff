{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Path
  , JKey(..)
  , JValue(..)
  , JKeyMap
  , JKeyValues
  ) where

import qualified Data.Map.Strict as M (Map)
import qualified Data.Text as T

-- | Key path to the value, e.g. @["solar_system", "planet", "earth"]@.
type Path = [T.Text]

newtype JKey = JKey { unJKey :: Path }
  deriving (Eq, Ord)

instance Show JKey where
  show = T.unpack . T.intercalate "." . unJKey

data JValue
  = JValueString T.Text
  -- note: we support only (leaf) arrays of strings
  | JValueArray [T.Text]
  deriving Eq

instance Show JValue where
  show (JValueString s) = T.unpack . wrapIn "\"" $ s
  show (JValueArray a) = T.unpack . wrapBetween "[" "]" . T.intercalate ", " . fmap (wrapIn "\"") $ a

wrapBetween :: T.Text -> T.Text -> T.Text -> T.Text
wrapBetween l r s = l <> s <> r

wrapIn :: T.Text -> T.Text -> T.Text
wrapIn l = wrapBetween l l

type JKeyMap = M.Map JKey

-- | Representation of a JSON translation file.
type JKeyValues = JKeyMap JValue

{-
{
  "key": {
    "a": "b"
  }
}

Object
  [ ("key", Object
      [ ("a", String "b")
      ]
    )
  ]

[ (["key", "a"], "b") ]
-}
