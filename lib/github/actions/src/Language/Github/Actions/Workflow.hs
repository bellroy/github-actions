{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Workflow
  ( Workflow (..),
    gen,
  )
where

import Control.Monad.Fail.Hoist (hoistFail')
import Data.Aeson (FromJSON, ToJSON (..), (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.Github.Actions.Concurrency (Concurrency)
import Language.Github.Actions.Concurrency qualified as Concurrency
import Language.Github.Actions.Defaults (Defaults)
import Language.Github.Actions.Defaults qualified as Defaults
import Language.Github.Actions.Job (Job)
import Language.Github.Actions.Job qualified as Job
import Language.Github.Actions.Job.Id (JobId)
import Language.Github.Actions.Job.Id qualified as JobId
import Language.Github.Actions.Permissions (Permissions)
import Language.Github.Actions.Permissions qualified as Permissions
import Language.Github.Actions.Types (ObjectKey, genObjectKey)
import Language.Github.Actions.Workflow.Trigger (WorkflowTrigger)
import Language.Github.Actions.Workflow.Trigger qualified as WorkflowTrigger
import Relude hiding (on)
import Text.NonEmpty (NonEmptyText)
import Text.NonEmpty qualified as NonEmptyText

data WorkflowJobStep = WorkflowJobStep
  { name :: Maybe NonEmptyText,
    run :: Maybe NonEmptyText
  }
  deriving stock (Eq, Generic, Ord, Show)

data Workflow = Workflow
  { concurrency :: Maybe Concurrency,
    defaults :: Maybe Defaults,
    env :: Map ObjectKey NonEmptyText,
    jobs :: NEMap JobId Job,
    on :: NESet WorkflowTrigger,
    permissions :: Maybe Permissions,
    runName :: Maybe NonEmptyText,
    workflowName :: Maybe NonEmptyText
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON Workflow where
  parseJSON = Aeson.withObject "Workflow" $ \o -> do
    concurrency <- o .:? "concurrency"
    defaults <- o .:? "defaults"
    env <- o .:? "env" .!= mempty
    jobs <- o .: "jobs"
    Aeson.Object onObject <- o .: "on"
    triggers <-
      traverse
        ( Aeson.parseJSON
            . Aeson.Object
            . uncurry AesonKeyMap.singleton
        )
        $ AesonKeyMap.toList onObject
    on <-
      maybe (fail "workflow triggers are empty") (pure . NESet.fromList) $
        nonEmpty triggers
    permissions <- o .:? "permissions"
    runName <- o .:? "run-name"
    workflowName <- o .:? "name"
    pure Workflow {..}

instance ToJSON Workflow where
  toJSON Workflow {..} =
    Aeson.object $
      catMaybes
        [ ("concurrency" .=) <$> concurrency,
          ("defaults" .=) <$> defaults,
          ("env" .=) <$> monoidToMaybe env,
          Just $ "jobs" .= jobs,
          ("name" .=) <$> workflowName,
          Just $ "on" .= Aeson.Object (mconcat (forceToJSONObject . Aeson.toJSON <$> toList on)),
          ("permissions" .=) <$> permissions,
          ("run-name" .=) <$> runName
        ]
    where
      forceToJSONObject :: Aeson.Value -> Aeson.Object
      forceToJSONObject (Aeson.Object o) = o
      forceToJSONObject _ = error "Expected Aeson.Object"

      monoidToMaybe :: (Eq a, Monoid a) => a -> Maybe a
      monoidToMaybe a = if a == mempty then Nothing else Just a

gen :: (MonadGen m, MonadFail m) => m Workflow
gen = do
  concurrency <- Gen.maybe Concurrency.gen
  defaults <- Gen.maybe Defaults.gen
  env <-
    Gen.map
      (Range.linear 0 5)
      ( liftA2
          (,)
          genObjectKey
          (NonEmptyText.gen Gen.alphaNum)
      )
  jobs <-
    Gen.map (Range.linear 1 5) (liftA2 (,) JobId.gen Job.gen)
      >>= hoistFail'
        . maybeToRight "empty job map specified"
        . NEMap.nonEmptyMap
  on <-
    Gen.list (Range.linear 1 5) WorkflowTrigger.gen
      >>= hoistFail'
        . maybeToRight "empty workflow trigger set specified"
        . NESet.nonEmptySet
        . fromList
        . snd
        . foldr onlyAddIfJsonKeyNotAlreadyPresent ([], [])
  permissions <- Gen.maybe Permissions.gen
  runName <- Gen.maybe $ NonEmptyText.gen Gen.alphaNum
  workflowName <- Gen.maybe $ NonEmptyText.gen Gen.alphaNum
  pure Workflow {..}
  where
    onlyAddIfJsonKeyNotAlreadyPresent :: WorkflowTrigger -> ([AesonKeyMap.Key], [WorkflowTrigger]) -> ([AesonKeyMap.Key], [WorkflowTrigger])
    onlyAddIfJsonKeyNotAlreadyPresent trigger (keys, triggers) =
      let key = case Aeson.toJSONKey of
            Aeson.ToJSONKeyText f _ -> f trigger
            _ -> error "Expected Aeson.ToJSONKeyText"
       in if key `elem` keys
            then (keys, triggers)
            else (key : keys, trigger : triggers)
