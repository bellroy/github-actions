{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Language.Github.Actions.Job
-- Description : GitHub Actions job definition and serialization
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'Job' type for representing individual jobs within GitHub Actions workflows.
-- Jobs are collections of steps that execute on the same runner.
--
-- A job defines the environment, dependencies, and steps that should be executed as part of a workflow.
-- Jobs can run in parallel or be configured to depend on other jobs.
--
-- For more information about GitHub Actions job syntax, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobs>
module Language.Github.Actions.Job
  ( Job (..),
    gen,
    new,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), (.!=), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Language.Github.Actions.Concurrency (Concurrency)
import qualified Language.Github.Actions.Concurrency as Concurrency
import Language.Github.Actions.Defaults (Defaults)
import qualified Language.Github.Actions.Defaults as Defaults
import Language.Github.Actions.Job.Container (JobContainer)
import qualified Language.Github.Actions.Job.Container as JobContainer
import Language.Github.Actions.Job.Environment (JobEnvironment)
import qualified Language.Github.Actions.Job.Environment as JobEnvironment
import Language.Github.Actions.Job.Needs (JobNeeds)
import qualified Language.Github.Actions.Job.Needs as JobNeeds
import Language.Github.Actions.Job.Strategy (JobStrategy)
import qualified Language.Github.Actions.Job.Strategy as JobStrategy
import Language.Github.Actions.Permissions (Permissions)
import qualified Language.Github.Actions.Permissions as Permissions
import Language.Github.Actions.RunIf (RunIf)
import qualified Language.Github.Actions.RunIf as RunIf
import Language.Github.Actions.Service (Service)
import qualified Language.Github.Actions.Service as Service
import Language.Github.Actions.Service.Id (ServiceId)
import qualified Language.Github.Actions.Service.Id as ServiceId
import Language.Github.Actions.Step (Step)
import qualified Language.Github.Actions.Step as Step

-- | A job within a GitHub Actions workflow.
--
-- A job is a set of steps that execute on the same runner. Jobs can run in parallel
-- or sequentially depending on their dependencies. Each job runs in its own virtual
-- environment specified by the runner.
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Job
-- import qualified Language.Github.Actions.Step as Step
--
-- myJob :: Job
-- myJob = new
--  { jobName = Just "Build and Test"
--  , runsOn = Just "ubuntu-latest"
--  , steps = Just $ Step.new :| []
--  }
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobs>
data Job = Job
  { -- | Concurrency settings for this job
    concurrency :: Maybe Concurrency,
    -- | Container to run the job in
    container :: Maybe JobContainer,
    -- | Whether to continue on step failure
    continueOnError :: Maybe Bool,
    -- | Default settings for steps in this job
    defaults :: Maybe Defaults,
    -- | Environment variables for this job
    env :: Map Text Text,
    -- | Deployment environment settings
    environment :: Maybe JobEnvironment,
    -- | Display name for the job
    jobName :: Maybe Text,
    -- | Jobs this job depends on
    needs :: Maybe JobNeeds,
    -- | Outputs from this job
    outputs :: Map Text Text,
    -- | Permissions for this job
    permissions :: Maybe Permissions,
    -- | Condition for running this job
    runIf :: Maybe RunIf,
    -- | Runner type (e.g., "ubuntu-latest")
    runsOn :: Maybe Text,
    -- | Secrets available to this job
    secrets :: Map Text Text,
    -- | Services to run alongside this job
    services :: Map ServiceId Service,
    -- | Steps to execute in this job
    steps :: Maybe (NonEmpty Step),
    -- | Matrix strategy for this job
    strategy :: Maybe JobStrategy,
    -- | Timeout for the job in minutes
    timeoutMinutes :: Maybe Int,
    -- | Reusable workflow to call
    uses :: Maybe Text,
    -- | Inputs for reusable workflows
    with :: Map Text Text
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON Job where
  parseJSON = Aeson.withObject "Job" $ \o -> do
    concurrency <- o .:? "concurrency"
    container <- o .:? "container"
    continueOnError <- o .:? "continue-on-error"
    defaults <- o .:? "defaults"
    env <- o .:? "env" .!= mempty
    environment <- o .:? "environment"
    jobName <- o .:? "name"
    needs <- o .:? "needs"
    outputs <- o .:? "outputs" .!= mempty
    permissions <- o .:? "permissions"
    runIf <- o .:? "if"
    runsOn <- o .:? "runs-on"
    secrets <- o .:? "secrets" .!= mempty
    services <- o .:? "services" .!= mempty
    steps <- o .:? "steps"
    strategy <- o .:? "strategy"
    timeoutMinutes <- o .:? "timeout-minutes"
    uses <- o .:? "uses"
    with <- o .:? "with" .!= mempty
    pure Job {..}

instance ToJSON Job where
  toJSON Job {..} =
    Aeson.object $
      catMaybes
        [ ("concurrency" .=) <$> concurrency,
          ("container" .=) <$> container,
          ("continue-on-error" .=) <$> continueOnError,
          ("defaults" .=) <$> defaults,
          ("env" .=) <$> monoidToMaybe env,
          ("environment" .=) <$> environment,
          ("if" .=) <$> runIf,
          ("name" .=) <$> jobName,
          ("needs" .=) <$> needs,
          ("outputs" .=) <$> monoidToMaybe outputs,
          ("permissions" .=) <$> permissions,
          ("runs-on" .=) <$> runsOn,
          ("secrets" .=) <$> monoidToMaybe secrets,
          ("services" .=) <$> monoidToMaybe services,
          ("steps" .=) <$> steps,
          ("strategy" .=) <$> strategy,
          ("timeout-minutes" .=) <$> timeoutMinutes,
          ("uses" .=) <$> uses,
          ("with" .=) <$> monoidToMaybe with
        ]
    where
      monoidToMaybe :: (Eq a, Monoid a) => a -> Maybe a
      monoidToMaybe a = if a == mempty then Nothing else Just a

gen :: (MonadGen m) => m Job
gen = do
  concurrency <- Gen.maybe Concurrency.gen
  container <- Gen.maybe JobContainer.gen
  continueOnError <- Gen.maybe Gen.bool
  defaults <- Gen.maybe Defaults.gen
  env <- genTextMap
  environment <- Gen.maybe JobEnvironment.gen
  jobName <- Gen.maybe genText
  needs <- Gen.maybe JobNeeds.gen
  outputs <- genTextMap
  permissions <- Gen.maybe Permissions.gen
  runIf <- Gen.maybe RunIf.gen
  runsOn <- Gen.maybe genText
  secrets <- genTextMap
  services <- Gen.map (Range.linear 1 5) $ liftA2 (,) ServiceId.gen Service.gen
  steps <- Gen.maybe (Gen.nonEmpty (Range.linear 1 20) Step.gen)
  strategy <- Gen.maybe JobStrategy.gen
  timeoutMinutes <- Gen.maybe $ Gen.int (Range.linear 1 120)
  uses <- Gen.maybe genText
  with <- genTextMap
  pure Job {..}
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
    genTextMap = Gen.map (Range.linear 1 5) $ liftA2 (,) genText genText

-- | Create a new empty 'Job' with default values.
--
-- This provides a minimal job that can be extended with specific steps,
-- runner configuration, and other settings.
--
-- Example:
--
-- @
-- buildJob = new
--   { jobName = Just "Build"
--   , runsOn = Just "ubuntu-latest"
--   , steps = Just $ checkoutStep :| [buildStep]
--   }
-- @
new :: Job
new =
  Job
    { concurrency = Nothing,
      container = Nothing,
      continueOnError = Nothing,
      defaults = Nothing,
      env = mempty,
      environment = Nothing,
      jobName = Nothing,
      needs = Nothing,
      outputs = mempty,
      permissions = Nothing,
      runIf = Nothing,
      runsOn = Nothing,
      secrets = mempty,
      services = mempty,
      steps = Nothing,
      strategy = Nothing,
      timeoutMinutes = Nothing,
      uses = Nothing,
      with = mempty
    }
