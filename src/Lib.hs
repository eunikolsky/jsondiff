module Lib where

import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as BL
import Data.List (singleton)
import Data.Map.Strict ((\\))
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V (fromList)
import GHC.Stack (HasCallStack)

import Types

-- | Calculates the difference between two JSON objects and returns it in a JSON format,
-- that is returning only those keys in the first object that are not present in the second one.
diff :: BL.ByteString -> BL.ByteString -> BL.ByteString
diff english translation = jq . unValues $ forceDecode english \\ forceDecode translation
  where
    forceDecode :: BL.ByteString -> JKeyValues
    forceDecode = values . fromJust . decode

values :: Value -> JKeyValues
values = go []
  where
    go :: Path -> Value -> JKeyValues
    go keyPath (String s) = M.singleton (JKey . reverse $ keyPath) (JValueString s)
    go keyPath (Object hashMap) = AKM.foldMapWithKey folder hashMap
      where
        folder :: Key -> Value -> JKeyValues
        folder key = go (AK.toText key : keyPath)
    go keyPath (Array a) = M.singleton (JKey. reverse $ keyPath) (JValueArray $ foldr folder [] a)
      where
        folder :: Value -> [T.Text] -> [T.Text]
        folder (String s) = (s :)
        folder (Object _) = unexpectedType "object in array" keyPath
        folder (Array _) = unexpectedType "array in array" keyPath
        folder (Number _) = unexpectedType "number in array" keyPath
        folder (Bool _) = unexpectedType "bool in array" keyPath
        folder Null = unexpectedType "null in array" keyPath
    go keyPath (Number _) = unexpectedType "number" keyPath
    go keyPath (Bool _) = unexpectedType "bool" keyPath
    go keyPath Null = unexpectedType "null" keyPath

    unexpectedType :: HasCallStack => String -> Path -> a
    unexpectedType jtype keyPath = error $ mconcat ["Unexpected ", jtype, " at ", show $ reverse keyPath]

unValues :: HasCallStack => JKeyValues -> Value
unValues = Object
  . foldr combine AKM.empty
  . M.foldMapWithKey (\k -> singleton . unfoldJKeyValue k)
  where
    combine :: Object -> Object -> Object
    combine = AKM.unionWith $ \v1 v2 -> case (v1, v2) of
      (Object o1, Object o2) -> Object $ combine o1 o2
      (Object o1, _) -> Object o1  -- Prefer object over non-object
      (_, Object o2) -> Object o2  -- Prefer object over non-object
      _ -> v1  -- Both non-objects, prefer first (arbitrary choice)

unfoldJKeyValue :: JKey -> JValue -> Object
unfoldJKeyValue (JKey keyPath) value = foldr iter AKM.empty keyPath
  where
    iter :: T.Text -> Object -> Object
    iter x acc = AKM.singleton
      (AK.fromText x)
      (if AKM.null acc then jsonValue value else Object acc)

    jsonValue :: JValue -> Value
    jsonValue (JValueString s) = String s
    jsonValue (JValueArray a) = Array . V.fromList $ String <$> a

-- | Pretty-prints JSON @Value@ as @jq@ does.
jq :: Value -> BL.ByteString
jq = AP.encodePretty' jqConfig
  where
    jqConfig :: AP.Config
    jqConfig = AP.Config
      { AP.confIndent = AP.Spaces 2
      , AP.confCompare = compare
      , AP.confNumFormat = AP.Generic
      , AP.confTrailingNewline = True
      }
