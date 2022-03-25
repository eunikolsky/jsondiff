module Lib where

import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- | Key path to the value, e.g. @["solar_system", "planet", "earth"]@.
type Path = [String]
newtype JKey = JKey { unJKey :: Path }
  deriving (Show)
-- TODO a value can also be a list of strings now.
newtype JValue = JValue { unJValue :: T.Text }
  deriving (Show)

-- | Representation of a JSON translation file.
type JKeyValues = M.Map JKey JValue

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

[ ("key.a", "b") ]
-}

values :: Value -> JKeyValues
values = go []
  where
    go :: Path -> Value -> JKeyValues
    go keyPath (String s) = M.singleton (JKey keyPath) (JValue s)
    -- go keys (Object hashMap) =
