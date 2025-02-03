{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Job
  ( Job (..),
    gen,
    new,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), (.!=), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.Github.Actions.Concurrency (Concurrency)
import Language.Github.Actions.Concurrency qualified as Concurrency
import Language.Github.Actions.Defaults (Defaults)
import Language.Github.Actions.Defaults qualified as Defaults
import Language.Github.Actions.Job.Container (JobContainer)
import Language.Github.Actions.Job.Container qualified as JobContainer
import Language.Github.Actions.Job.Environment (JobEnvironment)
import Language.Github.Actions.Job.Environment qualified as JobEnvironment
import Language.Github.Actions.Job.Id (JobId)
import Language.Github.Actions.Job.Id qualified as JobId
import Language.Github.Actions.Job.Strategy (JobStrategy)
import Language.Github.Actions.Job.Strategy qualified as JobStrategy
import Language.Github.Actions.Permissions (Permissions)
import Language.Github.Actions.Permissions qualified as Permissions
import Language.Github.Actions.Service (Service)
import Language.Github.Actions.Service qualified as Service
import Language.Github.Actions.Service.Id (ServiceId)
import Language.Github.Actions.Service.Id qualified as ServiceId
import Language.Github.Actions.Step (Step)
import Language.Github.Actions.Step qualified as Step

data Job = Job
  { concurrency :: Maybe Concurrency,
    container :: Maybe JobContainer,
    continueOnError :: Maybe Bool,
    defaults :: Maybe Defaults,
    env :: Map Text Text,
    environment :: Maybe JobEnvironment,
    jobName :: Maybe Text,
    needs :: Maybe (NonEmpty JobId),
    outputs :: Map Text Text,
    permissions :: Maybe Permissions,
    runIf :: Maybe Text,
    runsOn :: Maybe Text,
    secrets :: Map Text Text,
    services :: Map ServiceId Service,
    steps :: Maybe (NonEmpty Step),
    strategy :: Maybe JobStrategy,
    timeoutMinutes :: Maybe Int,
    uses :: Maybe Text,
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
  needs <- Gen.maybe (Gen.nonEmpty (Range.linear 1 5) JobId.gen)
  outputs <- genTextMap
  permissions <- Gen.maybe Permissions.gen
  runIf <- Gen.maybe genText
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
