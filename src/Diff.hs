{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff where

import Control.Monad.Writer
import qualified Data.Bifunctor
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

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

formatIgnoredOutdatedValues :: IgnoredOutdatedValues -> T.Text
formatIgnoredOutdatedValues (IgnoredOutdatedValues ignoredOutdatedValues) =
  T.unlines $
    [ "These keys were ignored because their values changed since the translation was sent:" ]
    <> map formatIgnoredOutdatedValue (M.toAscList ignoredOutdatedValues)
  where
    formatIgnoredOutdatedValue (key, (JValue oldValue, JValue currentValue)) =
      T.unwords
        [ T.pack $ show key
        , ":", oldValue
        , "=>", currentValue
        ]

type WarningsText = T.Text

-- | The function to purely integrate changes in translations.
integrateChanges
  :: OldEnglish -> CurrentEnglish
  -> CurrentTranslations -> NewTranslations
  -> Writer WarningsText UpdatedTranslations
integrateChanges oldEnglish currentEnglish currentTranslations newTranslations = do
  let diffMap = findChanges oldEnglish currentEnglish
  let updatedTranslations = applyChanges currentTranslations newTranslations diffMap
  mapWriterW formatIgnoredOutdatedValues updatedTranslations

-- | Maps over the acculumulated output of the @Writer@.
mapWriterW :: (w -> u) -> Writer w a -> Writer u a
mapWriterW f = mapWriter (Data.Bifunctor.second f)

-- 1 : 2 : 3 : []
-- 1 `f` (2 `f` (3 `f` []))
foldrWithKeyM :: Monad m => (k -> a -> b -> m b) -> b -> M.Map k a -> m b
foldrWithKeyM f z = foldrM (uncurry f) z . M.toAscList
