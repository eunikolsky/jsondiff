{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Warnings
  ( IgnoredValues
  , WarningsText

  , combineWarnings
  , ignoredExtraValues
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
newtype IgnoredOutdatedValue = IgnoredOutdatedValue (JValue, JValue)
instance Show IgnoredOutdatedValue where
  show (IgnoredOutdatedValue v) = show v

-- | A map of json key-values that have been removed.
newtype IgnoredMissingValue = IgnoredMissingValue JValue
instance Show IgnoredMissingValue where
  show (IgnoredMissingValue v) = show v

-- | A map of json key-values that have been moved.
newtype UpdatedMovedValue = UpdatedMovedValue (NonEmpty JKey)
instance Show UpdatedMovedValue where
  show (UpdatedMovedValue v) = show v

-- | A map of json key-values that have been ignored because the key was not in the old English file.
newtype IgnoredExtraValue = IgnoredExtraValue JValue
instance Show IgnoredExtraValue where
  show (IgnoredExtraValue v) = show v

data IgnoredValue
  = IVOutdated IgnoredOutdatedValue
  | IVMissing IgnoredMissingValue
  | IVMoved UpdatedMovedValue
  | IVIgnored IgnoredExtraValue
  deriving Show

-- | Contains changed and removed json key-values.
newtype IgnoredValues = IgnoredValues (JKeyMap IgnoredValue)
  deriving (Monoid, Semigroup, Show)

ignoredOutdatedValues :: JKey -> (JValue, JValue) -> IgnoredValues
ignoredOutdatedValues key = IgnoredValues . M.singleton key . IVOutdated . IgnoredOutdatedValue

ignoredMissingValues :: JKey -> JValue -> IgnoredValues
ignoredMissingValues key = IgnoredValues . M.singleton key . IVMissing . IgnoredMissingValue

updatedMovedValues :: JKey -> NonEmpty JKey -> IgnoredValues
updatedMovedValues key = IgnoredValues . M.singleton key . IVMoved . UpdatedMovedValue

ignoredExtraValues :: JKey -> JValue -> IgnoredValues
ignoredExtraValues key = IgnoredValues . M.singleton key . IVIgnored . IgnoredExtraValue

combineWarnings :: IgnoredValues -> Maybe WarningsText
combineWarnings ignoredValues =
  fmap (T.intercalate "\n") . maybeNonEmpty $
    catMaybes
      [ formatIgnoredOutdatedValues $ ivOutdated ignoredValues
      , formatIgnoredMissingValues $ ivMissing ignoredValues
      , formatUpdatedMovedValues $ ivMoved ignoredValues
      , formatIgnoredExtraValues $ ivIgnored ignoredValues
      ]

ivOutdated :: IgnoredValues -> JKeyMap IgnoredOutdatedValue
ivOutdated (IgnoredValues m) = M.mapMaybe (\case
    IVOutdated v -> Just v
    _ -> Nothing
  ) m

ivMissing :: IgnoredValues -> JKeyMap IgnoredMissingValue
ivMissing (IgnoredValues m) = M.mapMaybe (\case
    IVMissing v -> Just v
    _ -> Nothing
  ) m

ivMoved :: IgnoredValues -> JKeyMap UpdatedMovedValue
ivMoved (IgnoredValues m) = M.mapMaybe (\case
    IVMoved v -> Just v
    _ -> Nothing
  ) m

ivIgnored :: IgnoredValues -> JKeyMap IgnoredExtraValue
ivIgnored (IgnoredValues m) = M.mapMaybe (\case
    IVIgnored v -> Just v
    _ -> Nothing
  ) m

formatIgnoredOutdatedValues :: JKeyMap IgnoredOutdatedValue -> Maybe T.Text
formatIgnoredOutdatedValues values
  | M.null values = Nothing
  | otherwise = Just . T.intercalate "\n" $
    [ "These keys were ignored because their values changed since the translation was sent:" ]
    <> map formatIgnoredOutdatedValue (M.toAscList values)
  where
    formatIgnoredOutdatedValue (key, IgnoredOutdatedValue (oldValue, currentValue)) = T.pack $ mconcat
      [ show key , ": ", show oldValue , " => ", show currentValue ]

formatIgnoredMissingValues :: JKeyMap IgnoredMissingValue -> Maybe T.Text
formatIgnoredMissingValues values
  | M.null values = Nothing
  | otherwise = Just . T.intercalate "\n" $
    [ "These keys were ignored because they were removed since the translation was sent:" ]
    <> map formatIgnoredMissingValue (M.toAscList values)
  where
    formatIgnoredMissingValue (key, value) = T.pack $ mconcat
      [ show key , ": ", show value ]

formatUpdatedMovedValues :: JKeyMap UpdatedMovedValue -> Maybe T.Text
formatUpdatedMovedValues values
  | M.null values = Nothing
  | otherwise = Just . T.intercalate "\n" $
    [ "Values for these keys were applied at new keys:" ]
    <> map formatUpdatedMovedValue (M.toAscList values)
  where
    formatUpdatedMovedValue (key, UpdatedMovedValue newKeys) = T.pack $ mconcat
      [ show key , " => ", show $ toList newKeys ]

formatIgnoredExtraValues :: JKeyMap IgnoredExtraValue -> Maybe T.Text
formatIgnoredExtraValues values
  | M.null values = Nothing
  | otherwise = Just . T.intercalate "\n" $
    [ "These keys were ignored because they were missing in the old English file:" ]
    <> map formatIgnoredExtraValue (M.toAscList values)
  where
    formatIgnoredExtraValue (key, value) = T.pack $ mconcat
      [ show key , ": ", show value ]

maybeNonEmpty :: [a] -> Maybe [a]
maybeNonEmpty [] = Nothing
maybeNonEmpty xs = Just xs
