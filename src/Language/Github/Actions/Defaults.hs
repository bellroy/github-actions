{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Language.Github.Actions.Defaults
-- Description : Default settings for GitHub Actions workflows and jobs
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'Defaults' type for setting default values that apply
-- to all steps within a job or all jobs within a workflow.
--
-- Defaults allow you to specify common settings like shell type and working directory
-- that will be inherited by all steps unless explicitly overridden at the step level.
--
-- For more information about GitHub Actions defaults, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#defaults>
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

-- | Default settings for steps within a job or jobs within a workflow.
--
-- Defaults provide a convenient way to set common configuration that applies to
-- multiple steps without having to repeat the same settings everywhere.
--
-- Currently supports defaults for the 'run' command configuration:
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Defaults
-- import Language.Github.Actions.Shell
--
-- -- Set bash as default shell for all steps
-- bashDefaults :: Defaults
-- bashDefaults = Defaults
--  { runShell = Just (Bash Nothing)
--  , runWorkingDirectory = Nothing
--  }
--
-- -- Set working directory for all steps
-- workdirDefaults :: Defaults
-- workdirDefaults = Defaults
--  { runShell = Nothing
--  , runWorkingDirectory = Just "\/src"
--  }
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#defaults>
data Defaults = Defaults
  { -- | Default shell for run commands
    runShell :: Maybe Shell,
    -- | Default working directory for run commands
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

-- | Generate a random 'Defaults' for property-based testing.
--
-- This generator creates defaults with randomized properties suitable for testing
-- JSON serialization roundtrips and other property-based tests.
gen :: (MonadGen m) => m Defaults
gen = do
  runShell <- Gen.maybe Shell.gen
  runWorkingDirectory <- Gen.maybe (Gen.text (Range.linear 1 5) Gen.alphaNum)
  pure Defaults {..}
