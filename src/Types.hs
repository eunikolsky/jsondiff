module Types where

import qualified Data.Map.Strict as M (Map)
import qualified Data.Text as T (Text)

-- | Key path to the value, e.g. @["solar_system", "planet", "earth"]@.
type Path = [String]

newtype JKey = JKey { unJKey :: Path }
  deriving (Eq, Ord)

instance Show JKey where
  show = show . unJKey

-- TODO a value can also be a list of strings now.
newtype JValue = JValue { unJValue :: T.Text }
  deriving Eq

instance Show JValue where
  show = show . unJValue

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
