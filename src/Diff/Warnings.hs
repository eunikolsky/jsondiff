{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Warnings
  ( IgnoredValues
  , WarningsText

  , combineWarnings
  , ignoredMissingValues
  , ignoredOutdatedValues
  , updatedMovedValues
  ) where

import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T

import Types

type WarningsText = T.Text

-- | A map of json key-values whose value has changed.
newtype IgnoredOutdatedValues = IgnoredOutdatedValues (JKeyMap (JValue, JValue))
  deriving (Monoid, Semigroup, Show)

-- | A map of json key-values that have been removed.
newtype IgnoredMissingValues = IgnoredMissingValues JKeyValues
  deriving (Monoid, Semigroup, Show)

-- | A map of json key-values that have been moved.
newtype UpdatedMovedValues = UpdatedMovedValues (JKeyMap (NonEmpty JKey))
  deriving (Monoid, Semigroup, Show)

-- | Contains changed and removed json key-values.
data IgnoredValues = IgnoredValues
  { ivOutdated :: IgnoredOutdatedValues
  , ivMissing :: IgnoredMissingValues
  , ivMoved :: UpdatedMovedValues
  }
  deriving Show

instance Semigroup IgnoredValues where
  (IgnoredValues outdated missing moved) <> (IgnoredValues outdated' missing' moved') =
    IgnoredValues
      { ivOutdated = outdated <> outdated'
      , ivMissing = missing <> missing'
      , ivMoved = moved <> moved'
      }

instance Monoid IgnoredValues where
  mempty = IgnoredValues
    { ivOutdated = mempty
    , ivMissing = mempty
    , ivMoved = mempty
    }

ignoredOutdatedValues :: JKeyMap (JValue, JValue) -> IgnoredValues
ignoredOutdatedValues m = let outdated = IgnoredOutdatedValues m
  in IgnoredValues { ivOutdated = outdated, ivMissing = mempty, ivMoved = mempty }

ignoredMissingValues :: JKeyValues -> IgnoredValues
ignoredMissingValues m = let missing = IgnoredMissingValues m
  in IgnoredValues { ivOutdated = mempty, ivMissing = missing, ivMoved = mempty }

updatedMovedValues :: JKeyMap (NonEmpty JKey) -> IgnoredValues
updatedMovedValues m = let moved = UpdatedMovedValues m
  in IgnoredValues { ivOutdated = mempty, ivMissing = mempty, ivMoved = moved }


combineWarnings :: IgnoredValues -> Maybe WarningsText
combineWarnings ignoredValues =
  fmap (T.intercalate "\n") . maybeNonEmpty $
    catMaybes
      [ formatIgnoredOutdatedValues $ ivOutdated ignoredValues,
        formatIgnoredMissingValues $ ivMissing ignoredValues,
        formatUpdatedMovedValues $ ivMoved ignoredValues
      ]

formatIgnoredOutdatedValues :: IgnoredOutdatedValues -> Maybe T.Text
formatIgnoredOutdatedValues (IgnoredOutdatedValues values)
  | M.null values = Nothing
  | otherwise = Just . T.intercalate "\n" $
    [ "These keys were ignored because their values changed since the translation was sent:" ]
    <> map formatIgnoredOutdatedValue (M.toAscList values)
  where
    formatIgnoredOutdatedValue (key, (oldValue, currentValue)) = T.pack $ mconcat
      [ show key , ": ", show oldValue , " => ", show currentValue ]

formatIgnoredMissingValues :: IgnoredMissingValues -> Maybe T.Text
formatIgnoredMissingValues (IgnoredMissingValues values)
  | M.null values = Nothing
  | otherwise = Just . T.intercalate "\n" $
    [ "These keys were ignored because they were removed since the translation was sent:" ]
    <> map formatIgnoredMissingValue (M.toAscList values)
  where
    formatIgnoredMissingValue (key, value) = T.pack $ mconcat
      [ show key , ": ", show value ]

formatUpdatedMovedValues :: UpdatedMovedValues -> Maybe T.Text
formatUpdatedMovedValues (UpdatedMovedValues values)
  | M.null values = Nothing
  | otherwise = Just . T.intercalate "\n" $
    [ "Values for these keys were applied at new keys:" ]
    <> map formatUpdatedMovedValue (M.toAscList values)
  where
    formatUpdatedMovedValue (key, newKeys) = T.pack $ mconcat
      [ show key , " => ", show $ toList newKeys ]

maybeNonEmpty :: [a] -> Maybe [a]
maybeNonEmpty [] = Nothing
maybeNonEmpty xs = Just xs
