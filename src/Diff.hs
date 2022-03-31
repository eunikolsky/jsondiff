{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Diff where

import Control.Monad.Writer
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as M

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

newtype IgnoredOutdatedValues = IgnoredOutdatedValues (JKeyMap (JValue, JValue))
  deriving (Monoid, Semigroup, Show)

type DiffResultsWith = Writer IgnoredOutdatedValues

applyChanges :: CurrentTranslations -> NewTranslations -> DiffMap -> DiffResultsWith UpdatedTranslations
applyChanges current new (DiffMap diffs) = foldrWithKeyM applyDifference current new
  where
    applyDifference :: JKey -> JValue -> UpdatedTranslations -> DiffResultsWith UpdatedTranslations
    applyDifference k v acc = case M.lookup k diffs of
      Just NoChange -> pure $ M.insert k v acc
      Just (ValueChange oldValue currentValue) -> do
        tell $ IgnoredOutdatedValues $ M.singleton k (oldValue, currentValue)
        pure acc
      Nothing -> pure acc

-- 1 : 2 : 3 : []
-- 1 `f` (2 `f` (3 `f` []))
foldrWithKeyM :: Monad m => (k -> a -> b -> m b) -> b -> M.Map k a -> m b
foldrWithKeyM f z = foldrM (uncurry f) z . M.toAscList
