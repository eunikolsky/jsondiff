{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8 (putStrLn)
import Data.Map.Strict ((\\))
import System.Environment (getArgs)
import System.Exit (die)

import Lib
import Types

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file1, file2] -> printDiff file1 file2
    _ -> die "Usage: jsondiff <file1> <file2>"

printDiff :: FilePath -> FilePath -> IO ()
printDiff file1 file2 = do
   (Just value1 :: Maybe JKeyValues) <- fmap values <$> decodeFileStrict' file1
   (Just value2 :: Maybe JKeyValues) <- fmap values <$> decodeFileStrict' file2
   let diff = value1 \\ value2
   BSL8.putStrLn . encode . unValues $ diff
