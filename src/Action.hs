module Action where

import qualified Data.Map.Strict as M

import Types

data Action
  -- | Copy the key-value as is.
  = CopyAsIs
  deriving Show

newtype ActionMap = ActionMap { unActionMap :: JKeyMap Action }
  deriving Show

type OldEnglish = JKeyValues
type CurrentEnglish = JKeyValues
type CurrentTranslations = JKeyValues
type NewTranslations = JKeyValues
type UpdatedTranslations = JKeyValues

findChanges :: OldEnglish -> CurrentEnglish -> ActionMap
findChanges oldEnglish currentEnglish = copyAsIsActions
  where
    copyAsIsActions :: ActionMap
    copyAsIsActions = ActionMap . collectJust $ M.intersectionWith equalValues oldEnglish currentEnglish

    equalValues old current = if old == current then Just CopyAsIs else Nothing
    collectJust = M.mapMaybe id

type ActionResultsWith a = ([String], a)

applyChanges :: CurrentTranslations -> NewTranslations -> ActionMap -> ActionResultsWith UpdatedTranslations
applyChanges = undefined
