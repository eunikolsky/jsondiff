module Lib where

import Data.Aeson
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- | Key path to the value, e.g. @["solar_system", "planet", "earth"]@.
type Path = [String]

newtype JKey = JKey { unJKey :: Path }
  deriving (Eq, Ord)

instance Show JKey where
  show (JKey l) = show l

-- TODO a value can also be a list of strings now.
newtype JValue = JValue { unJValue :: T.Text }
instance Show JValue where
  show (JValue l) = show l

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

values :: Value -> JKeyValues
values = go []
  where
    go :: Path -> Value -> JKeyValues
    go keyPath (String s) = M.singleton (JKey . reverse $ keyPath) (JValue s)
    go keyPath (Object hashMap) = AKM.foldMapWithKey folder hashMap
      where
        folder :: Key -> Value -> JKeyValues
        folder key = go (AK.toString key : keyPath)
    go keyPath (Array _) = unexpectedType "array" keyPath
    go keyPath (Number _) = unexpectedType "number" keyPath
    go keyPath (Bool _) = unexpectedType "bool" keyPath
    go keyPath Null = unexpectedType "null" keyPath

unexpectedType :: String -> Path -> a
unexpectedType jtype keyPath = error $ mconcat ["Unexpected ", jtype, " at ", show $ reverse keyPath]

unValues :: JKeyValues -> Value
unValues = Object . M.foldlWithKey' folder AKM.empty
  where
    folder :: Object -> JKey -> JValue -> Object
    folder acc (JKey keyPath) value = iter acc keyPath value

    iter :: Object -> Path -> JValue -> Object
    iter object (key:keys) value =
      let { newObject =
        case AKM.lookup (AK.fromString key) object of
          Just (Object nestedObject) -> iter nestedObject keys value
          Just _ -> error $ mconcat ["Unexpected type at ", show key]
          Nothing -> unfoldJKeyValue (JKey keys) value
      }
      in AKM.insert (AK.fromString key) (Object newObject) object
    iter _ [] _ = error "No keys left"

{-

unValues [ (["main", "yes"], "YES"), (["main", "no"], "NO") ]

folder empty ["main", "no"] "NO" = iter empty ["main", "no"] "NO"
= { newObject = Nothing -> [ ("no", "NO") ] in
    [ ("main", [ ("no", "NO") ]) ] }

folder [ ("main", [ ("no", "NO") ]) ] ["main", "yes"] "YES" = iter …
= { newObject = Just [ ("no", "NO" ) ] -> iter [ ("no", "NO") ] ["yes"] "YES"
  = { newObject = Nothing -> [ ("yes", "YES") ] in
      [ ("no", "NO"), ("yes", "YES") ]
    } in

  }

-}

unfoldJKeyValue :: JKey -> JValue -> Object
unfoldJKeyValue (JKey keyPath) (JValue value) = foldr iter AKM.empty keyPath
  where
    iter :: String -> Object -> Object
    iter x acc = AKM.singleton
      (AK.fromString x)
      (if AKM.null acc then String value else Object acc)
