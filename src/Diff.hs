module Diff where

import qualified Data.Map.Strict as M
import qualified Data.Text as T (Text)

import Types

data Difference
  -- | There is no change in this key-value.
  = NoChange
  -- | The value at this key has changed from @old@ to @new@.
  | ValueChange JValue JValue
  deriving Show

newtype DiffMap = DiffMap { unDiffMap :: JKeyMap Difference }
  deriving Show

type OldEnglish = JKeyValues
type CurrentEnglish = JKeyValues
type CurrentTranslations = JKeyValues
type NewTranslations = JKeyValues
type UpdatedTranslations = JKeyValues

findChanges :: OldEnglish -> CurrentEnglish -> DiffMap
findChanges oldEnglish = DiffMap
  . M.intersectionWith determineDiff oldEnglish

  where
    determineDiff old current = if old == current then NoChange else ValueChange old current

type ActionResultsWith a = ([T.Text], a)

applyChanges :: CurrentTranslations -> NewTranslations -> DiffMap -> ActionResultsWith UpdatedTranslations
applyChanges = undefined
