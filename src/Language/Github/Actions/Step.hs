{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Language.Github.Actions.Step
-- Description : GitHub Actions step definition and serialization
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'Step' type for representing individual steps within GitHub Actions jobs.
-- Steps are the individual tasks that make up a job, such as checking out code, running commands,
-- or using pre-built actions.
--
-- A step can either run a command/script or use a pre-built action from the GitHub marketplace
-- or a custom action. Steps run sequentially within a job.
--
-- For more information about GitHub Actions step syntax, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idsteps>
module Language.Github.Actions.Step
  ( Step (..),
    gen,
    new,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), (.!=), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Language.Github.Actions.Shell (Shell)
import qualified Language.Github.Actions.Shell as Shell
import Language.Github.Actions.Step.Id (StepId)
import qualified Language.Github.Actions.Step.Id as StepId
import Language.Github.Actions.Step.With (StepWith)
import qualified Language.Github.Actions.Step.With as StepWith

-- | A step within a GitHub Actions job.
--
-- A step is an individual task within a job. Steps can run commands, scripts, or actions.
-- Each step runs in the runner environment and can use outputs from previous steps.
--
-- There are two main types of steps:
-- * Command steps that use 'run' to execute shell commands
-- * Action steps that use 'uses' to run a pre-built action
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Step
--
-- -- A command step
-- commandStep :: Step
-- commandStep = new
--  { name = Just "Run tests"
--  , run = Just "npm test"
--  }
--
-- -- An action step
-- actionStep :: Step
-- actionStep = new
--  { name = Just "Checkout code"
--  , uses = Just "actions/checkout\@v4"
--  }
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idsteps>
data Step = Step
  { -- | Whether to continue job execution if this step fails
    continueOnError :: Bool,
    -- | Environment variables for this step
    env :: Map Text Text,
    -- | Display name for the step
    name :: Maybe Text,
    -- | Command or script to run
    run :: Maybe Text,
    -- | Condition for running this step
    runIf :: Maybe Text,
    -- | Shell to use for running commands
    shell :: Maybe Shell,
    -- | Unique identifier for this step
    stepId :: Maybe StepId,
    -- | Timeout for the step in minutes
    timeoutMinutes :: Maybe Int,
    -- | Action to run (e.g., "actions/checkout@v4")
    uses :: Maybe Text,
    -- | Inputs for the action
    with :: Maybe StepWith,
    -- | Working directory for the step
    workingDirectory :: Maybe Text
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON Step where
  parseJSON = Aeson.withObject "Step" $ \o -> do
    continueOnError <- o .:? "continue-on-error" .!= False
    env <- o .:? "env" .!= mempty
    name <- o .:? "name"
    run <- o .:? "run"
    runIf <- o .:? "if"
    uses <- o .:? "uses"
    shell <- o .:? "shell"
    stepId <- o .:? "id"
    timeoutMinutes <- o .:? "timeout-minutes"
    with <- o .:? "with"
    workingDirectory <- o .:? "working-directory"
    pure Step {..}

instance ToJSON Step where
  toJSON Step {..} =
    Aeson.object $
      catMaybes
        [ if continueOnError then Just ("continue-on-error" .= True) else Nothing,
          ("env" .=) <$> monoidToMaybe env,
          ("id" .=) <$> stepId,
          ("if" .=) <$> runIf,
          ("name" .=) <$> name,
          ("run" .=) <$> run,
          ("shell" .=) <$> shell,
          ("timeout-minutes" .=) <$> timeoutMinutes,
          ("uses" .=) <$> uses,
          ("with" .=) <$> with,
          ("working-directory" .=) <$> workingDirectory
        ]
    where
      monoidToMaybe :: (Eq a, Monoid a) => a -> Maybe a
      monoidToMaybe a = if a == mempty then Nothing else Just a

-- | Generate a random 'Step' for property-based testing.
--
-- This generator creates steps with randomized properties suitable for testing
-- JSON serialization roundtrips and other property-based tests.
gen :: (MonadGen m) => m Step
gen = do
  continueOnError <- Gen.bool
  env <- genTextMap
  name <- Gen.maybe genText
  run <- Gen.maybe genText
  runIf <- Gen.maybe genText
  shell <- Gen.maybe Shell.gen
  stepId <- Gen.maybe StepId.gen
  timeoutMinutes <- Gen.maybe $ Gen.int (Range.linear 1 120)
  uses <- Gen.maybe genText
  with <- Gen.maybe StepWith.gen
  workingDirectory <- Gen.maybe genText
  pure Step {..}
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
    genTextMap = Gen.map (Range.linear 3 20) $ liftA2 (,) genText genText

-- | Create a new empty 'Step' with default values.
--
-- This provides a minimal step that can be extended with specific commands,
-- actions, or other configuration.
--
-- Example:
--
-- @
-- checkoutStep = new
--   { name = Just "Checkout repository"
--   , uses = Just "actions/checkout\@v4"
--   }
--
-- commandStep = new
--   { name = Just "Build project"
--   , run = Just "make build"
--   }
-- @
new :: Step
new =
  Step
    { continueOnError = False,
      env = mempty,
      name = Nothing,
      run = Nothing,
      runIf = Nothing,
      shell = Nothing,
      stepId = Nothing,
      timeoutMinutes = Nothing,
      uses = Nothing,
      with = Nothing,
      workingDirectory = Nothing
    }
