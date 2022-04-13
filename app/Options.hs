module Options
  ( Command(..)
  , DiffOptions(..)
  , IntegrateOptions(..)
  , command
  ) where

import Options.Applicative hiding (command)
import qualified Options.Applicative as Opts (command)

data Command
  = Diff DiffOptions
  | Integrate IntegrateOptions

data DiffOptions = DiffOptions
  { englishFile :: FilePath
  , translationFile :: FilePath
  }

data IntegrateOptions = IntegrateOptions
  { oldEnglishFile :: FilePath
  , currentEnglishFile :: FilePath
  , currentTranslationFile :: FilePath
  , newTranslationFile :: FilePath
  }

diffCommand :: Parser Command
diffCommand = fmap Diff $ DiffOptions
  <$> strOption (long "english" <> help "Current English filepath")
  <*> strOption (long "translation" <> help "Current translation (e.g., Spanish) filepath")

integrateCommand :: Parser Command
integrateCommand = fmap Integrate $ IntegrateOptions
  <$> strOption (long "old-english" <> help "Old English filepath")
  <*> strOption (long "current-english" <> help "Current English filepath")
  <*> strOption (long "current-translation" <> help "Current translation (e.g., Spanish) filepath")
  <*> strOption (long "new-translation" <> help "New translation filepath")

command :: Parser Command
command = subparser
  $  Opts.command "diff" (info diffCommand $ progDesc "Diff English and translation files")
  <> Opts.command "integrate" (info integrateCommand $ progDesc "Integrate new translations")
