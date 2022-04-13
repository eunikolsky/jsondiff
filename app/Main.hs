{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8 (putStrLn)
import Data.Map.Strict ((\\))
import Options.Applicative hiding (command)

import Lib
import Options
import Types

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (command <**> helper)
      ( fullDesc <> header "PoC tool to help with JSON localization files" )

run :: Command -> IO ()
run (Diff (DiffOptions { currentEnglishFile, currentTranslationFile })) =
  printDiff currentEnglishFile currentTranslationFile

printDiff :: FilePath -> FilePath -> IO ()
printDiff file1 file2 = do
   (Just value1 :: Maybe JKeyValues) <- fmap values <$> decodeFileStrict' file1
   (Just value2 :: Maybe JKeyValues) <- fmap values <$> decodeFileStrict' file2
   let diff = value1 \\ value2
   BSL8.putStrLn . encode . unValues $ diff
