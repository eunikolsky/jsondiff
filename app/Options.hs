module Options
  ( Command(..)
  , DiffOptions(..)
  , command
  ) where

import Options.Applicative hiding (command)
import qualified Options.Applicative as Opts (command)

newtype Command = Diff DiffOptions

data DiffOptions = DiffOptions
  { currentEnglishFile :: FilePath
  , currentTranslationFile :: FilePath
  }

diffCommand :: Parser Command
diffCommand = fmap Diff $ DiffOptions
  <$> strOption (long "english" <> help "Current English filepath")
  <*> strOption (long "translation" <> help "Current translation (e.g., Spanish) filepath")

command :: Parser Command
command = subparser $
  Opts.command  "diff" $ info diffCommand $ progDesc "Diff English and translation files"
