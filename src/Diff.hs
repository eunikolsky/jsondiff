module Diff where

import Control.Monad.Writer
import qualified Data.Bifunctor
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M

import Diff.Warnings
import Types

data Difference
  -- | There is no change in this key-value.
  = NoChange
  -- | The value at this key has changed from @old@ to @new@.
  | ValueChange JValue JValue
  -- | The value at this key has been removed.
  | MissingValue JValue
  -- | The value at this key has been moved to 1+ new @keys@ (there can be more
  -- than one key due to duplicate values).
  | MovedValue (NonEmpty JKey)
  deriving Show

newtype DiffMap = DiffMap { unDiffMap :: JKeyMap Difference }
  deriving Show

type OldEnglish = JKeyValues
type CurrentEnglish = JKeyValues
type CurrentTranslations = JKeyValues
type NewTranslations = JKeyValues
type UpdatedTranslations = JKeyValues

keysForValue :: CurrentEnglish -> JValue -> Maybe (NonEmpty JKey)
-- TODO repetitive conversion to list is probably ineffective for big JSONs when
-- there are a lot of moved values
keysForValue currentEnglish value = nonEmpty . fmap fst . filter ((== value) . snd) . M.toList $ currentEnglish

findChanges :: OldEnglish -> CurrentEnglish -> DiffMap
findChanges oldEnglish currentEnglish = DiffMap
  $ M.merge whenMissingCurrent whenMissingOld whenMatched oldEnglish currentEnglish

  where
    -- | Was in old, missing in current => either MissingValue or MovedValue
    whenMissingCurrent = M.mapMissing . const $ \old ->
      maybe (MissingValue old) MovedValue $ currentEnglish `keysForValue` old
    -- | Was in current, missing in old => ignore it, we don't care
    whenMissingOld = M.dropMissing
    -- | Present in both old and current => either NoChange or ValueChange
    whenMatched = M.zipWithMatched . const $ \old current ->
      if old == current then NoChange else ValueChange old current

type DiffResultsWith = Writer IgnoredValues

applyChanges :: CurrentTranslations -> NewTranslations -> DiffMap -> DiffResultsWith UpdatedTranslations
applyChanges current new (DiffMap diffs) = foldrWithKeyM applyDifference current new
  where
    applyDifference :: JKey -> JValue -> UpdatedTranslations -> DiffResultsWith UpdatedTranslations
    applyDifference k v acc = case M.lookup k diffs of
      Just NoChange -> pure $ M.insert k v acc
      Just (ValueChange oldValue currentValue) -> do
        recordOutdatedValue k oldValue currentValue
        pure acc
      Just (MissingValue currentValue) -> do
        recordMissingValue k currentValue
        pure acc
      Just (MovedValue newKeys) -> do
        recordMovedValue k newKeys
        pure $ foldr (`M.insert` v) acc newKeys
      Nothing -> pure acc

    recordOutdatedValue :: JKey -> JValue -> JValue -> DiffResultsWith ()
    recordOutdatedValue k oldValue newValue =
      tell . ignoredOutdatedValues $ M.singleton k (oldValue, newValue)

    recordMissingValue :: JKey -> JValue -> DiffResultsWith ()
    recordMissingValue k =
      tell . ignoredMissingValues . M.singleton k

    recordMovedValue :: JKey -> NonEmpty JKey -> DiffResultsWith ()
    recordMovedValue k =
      tell . updatedMovedValues . M.singleton k

-- | The function to purely integrate changes in translations.
mergeTranslations
  :: OldEnglish -> CurrentEnglish
  -> CurrentTranslations -> NewTranslations
  -> Writer (Maybe WarningsText) UpdatedTranslations
mergeTranslations oldEnglish currentEnglish currentTranslations newTranslations = do
  let diffMap = findChanges oldEnglish currentEnglish
  let updatedTranslations = applyChanges currentTranslations newTranslations diffMap
  mapWriterW combineWarnings updatedTranslations

-- | Maps over the acculumulated output of the @Writer@.
mapWriterW :: (w -> u) -> Writer w a -> Writer u a
mapWriterW f = mapWriter (Data.Bifunctor.second f)

-- 1 : 2 : 3 : []
-- 1 `f` (2 `f` (3 `f` []))
foldrWithKeyM :: Monad m => (k -> a -> b -> m b) -> b -> M.Map k a -> m b
foldrWithKeyM f z = foldrM (uncurry f) z . M.toAscList
