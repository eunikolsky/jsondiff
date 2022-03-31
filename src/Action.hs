module Action where

import qualified Data.Map.Strict as M
import qualified Data.Text as T (Text)

import Types

data Action
  -- | Copy the key-value as is.
  = CopyAsIs
  -- | Ignore the key because its value has changed from @old@ to @new@.
  | IgnoreValueChange JValue JValue
  deriving Show

newtype ActionMap = ActionMap { unActionMap :: JKeyMap Action }
  deriving Show

type OldEnglish = JKeyValues
type CurrentEnglish = JKeyValues
type CurrentTranslations = JKeyValues
type NewTranslations = JKeyValues
type UpdatedTranslations = JKeyValues

findChanges :: OldEnglish -> CurrentEnglish -> ActionMap
findChanges oldEnglish = ActionMap
  . M.intersectionWith determineAction oldEnglish

  where
    determineAction old current = if old == current then CopyAsIs else IgnoreValueChange old current

type ActionResultsWith a = ([T.Text], a)

applyChanges :: CurrentTranslations -> NewTranslations -> ActionMap -> ActionResultsWith UpdatedTranslations
applyChanges = undefined
