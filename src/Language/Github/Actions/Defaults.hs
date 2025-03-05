{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Defaults
  ( Defaults (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Language.Github.Actions.Shell (Shell)
import qualified Language.Github.Actions.Shell as Shell

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
