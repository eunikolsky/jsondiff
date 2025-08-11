{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Writer (runWriter)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8 (putStr, readFile)
import Data.String (IsString)
import qualified Data.Text.IO as T (hPutStrLn)
import GHC.Stack (HasCallStack)
import Options.Applicative hiding (command)
import System.IO (stderr)

import Diff
import Lib
import Options
import Types

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (command <**> helper)
      ( fullDesc <> header "PoC tool to help with JSON localization files" )

run :: Command -> IO ()
run (Diff (DiffOptions { englishFile, translationFile })) =
  printDiff englishFile translationFile
run (Integrate (IntegrateOptions { oldEnglishFile, currentEnglishFile, currentTranslationFile, newTranslationFile })) =
  integrateTranslations oldEnglishFile currentEnglishFile currentTranslationFile newTranslationFile

printDiff :: FilePath -> FilePath -> IO ()
printDiff englishFile translationFile = do
   english <- BSL8.readFile englishFile
   translation <- BSL8.readFile translationFile
   BSL8.putStr $ diff english translation

integrateTranslations :: HasCallStack => FilePath -> FilePath -> FilePath -> FilePath -> IO ()
integrateTranslations oldEnglishFile currentEnglishFile currentTranslationFile newTranslationFile = do
  oldEnglish <- decodeFile oldEnglishFile
  currentEnglish <- decodeFile currentEnglishFile
  currentTranslation <- decodeFile currentTranslationFile
  newTranslation <- decodeFile newTranslationFile
  let (updatedTranslation, warnings) = runWriter $ mergeTranslations oldEnglish currentEnglish currentTranslation newTranslation
  BSL8.putStr . jq . unValues $ updatedTranslation
  mapM_ (T.hPutStrLn stderr . prependNewLine) warnings

prependNewLine :: (IsString s, Semigroup s) => s -> s
prependNewLine = ("\n" <>)

decodeFile :: FilePath -> IO JKeyValues
decodeFile file = do
  d <- decodeFileStrict' file
  pure $ case d of
    Nothing -> error $ "Failed to decode JSON file " ++ file
    Just x -> values x
