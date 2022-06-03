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
instance Show JValue where
  show = show . unJValue

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

[ (["key", "a"], "b") ]
-}
