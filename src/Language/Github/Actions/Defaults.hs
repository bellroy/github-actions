{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Defaults
  ( Defaults (..),
    gen,
  )
where

import Control.Applicative (pure)
import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..), catMaybes)
import Data.Ord (Ord)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.Github.Actions.Shell (Shell)
import Language.Github.Actions.Shell qualified as Shell
import Text.Show (Show)

data Defaults = Defaults
  { runShell :: Maybe Shell,
    runWorkingDirectory :: Maybe Text
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON Defaults where
  parseJSON = Aeson.withObject "Defaults" $ \o -> do
    run <- o .: "run"
    runShell <- run .:? "shell"
    runWorkingDirectory <- run .:? "working-directory"
    pure Defaults {..}

instance ToJSON Defaults where
  toJSON Defaults {..} =
    Aeson.object
      [ "run"
          .= Aeson.object
            ( catMaybes
                [ ("shell" .=) <$> runShell,
                  ("working-directory" .=) <$> runWorkingDirectory
                ]
            )
      ]

gen :: (MonadGen m) => m Defaults
gen = do
  runShell <- Gen.maybe Shell.gen
  runWorkingDirectory <- Gen.maybe (Gen.text (Range.linear 1 5) Gen.alphaNum)
  pure Defaults {..}
