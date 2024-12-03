{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Workflow
  ( Workflow (..),
    gen,
    new,
  )
where

import Control.Applicative (liftA2, pure)
import Data.Aeson (FromJSON, ToJSON (..), (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.Eq (Eq, (==))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (elem, foldr)
import Data.Map (Map)
import Data.Maybe (Maybe (..), catMaybes)
import Data.Monoid (Monoid, mconcat, mempty)
import Data.Ord (Ord)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (traverse)
import Data.Tuple (snd, uncurry)
import GHC.Err (error)
import GHC.Generics (Generic)
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
import Language.Github.Actions.Workflow.Trigger (WorkflowTrigger)
import Language.Github.Actions.Workflow.Trigger qualified as WorkflowTrigger
import Text.Show (Show)

data WorkflowJobStep = WorkflowJobStep
  { name :: Maybe Text,
    run :: Maybe Text
  }
  deriving stock (Eq, Generic, Ord, Show)

data Workflow = Workflow
  { concurrency :: Maybe Concurrency,
    defaults :: Maybe Defaults,
    env :: Map Text Text,
    jobs :: Map JobId Job,
    on :: Set WorkflowTrigger,
    permissions :: Maybe Permissions,
    runName :: Maybe Text,
    workflowName :: Maybe Text
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
    let on = Set.fromList triggers
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
          Just $ "on" .= Aeson.Object (mconcat (forceToJSONObject . Aeson.toJSON <$> Set.toList on)),
          ("permissions" .=) <$> permissions,
          ("run-name" .=) <$> runName
        ]
    where
      forceToJSONObject :: Aeson.Value -> Aeson.Object
      forceToJSONObject (Aeson.Object o) = o
      forceToJSONObject _ = error "Expected Aeson.Object"

      monoidToMaybe :: (Eq a, Monoid a) => a -> Maybe a
      monoidToMaybe a = if a == mempty then Nothing else Just a

gen :: (MonadGen m) => m Workflow
gen = do
  concurrency <- Gen.maybe Concurrency.gen
  defaults <- Gen.maybe Defaults.gen
  env <- genTextMap
  jobs <-
    Gen.map (Range.linear 1 5) (liftA2 (,) JobId.gen Job.gen)
  on <-
    Set.fromList
      . snd
      . foldr onlyAddIfJsonKeyNotAlreadyPresent ([], [])
      <$> Gen.list (Range.linear 1 5) WorkflowTrigger.gen

  permissions <- Gen.maybe Permissions.gen
  runName <- Gen.maybe genText
  workflowName <- Gen.maybe genText
  pure Workflow {..}
  where
    onlyAddIfJsonKeyNotAlreadyPresent ::
      WorkflowTrigger ->
      ([AesonKeyMap.Key], [WorkflowTrigger]) ->
      ([AesonKeyMap.Key], [WorkflowTrigger])
    onlyAddIfJsonKeyNotAlreadyPresent trigger (keys, triggers) =
      let key = case Aeson.toJSONKey of
            Aeson.ToJSONKeyText f _ -> f trigger
            _ -> error "Expected Aeson.ToJSONKeyText"
       in if key `elem` keys
            then (keys, triggers)
            else (key : keys, trigger : triggers)

    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
    genTextMap = Gen.map (Range.linear 1 5) $ liftA2 (,) genText genText

new :: Workflow
new =
  Workflow
    { concurrency = Nothing,
      defaults = Nothing,
      env = mempty,
      jobs = mempty,
      on = mempty,
      permissions = Nothing,
      runName = Nothing,
      workflowName = Nothing
    }
