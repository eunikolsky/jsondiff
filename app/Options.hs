module Options
  ( Command(..)
  , DiffOptions(..)
  , Options.command
  ) where

import Options.Applicative

newtype Command = Diff DiffOptions

data DiffOptions = DiffOptions
  { currentEnglishFile :: FilePath
  , currentTranslationFile :: FilePath
  }

command :: Parser Command
command = fmap Diff $ DiffOptions
  <$> strOption (long "english" <> help "Current English filepath")
  <*> strOption (long "translation" <> help "Current translation (e.g., Spanish) filepath")
