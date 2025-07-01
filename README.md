# Github Actions

[![Haskell-CI](https://github.com/bellroy/github-actions/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/bellroy/github-actions/actions/workflows/haskell-ci.yml)

This library provides types and instances for serializing and deserializing
GitHub Actions YAML, so that workflows can be built and maintained in Haskell.

As specified here:
https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions

## Usage Examples

### 1. Exporting a Workflow to YAML

You can create a workflow in Haskell and export it to YAML format:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Yaml as Yaml
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))
import Language.Github.Actions.Workflow
import qualified Language.Github.Actions.Job as Job
import qualified Language.Github.Actions.Job.Id as JobId
import qualified Language.Github.Actions.Step as Step
import qualified Language.Github.Actions.Workflow.Trigger as Trigger

-- Create a simple CI workflow
myWorkflow :: Workflow
myWorkflow = new
  { workflowName = Just "CI"
  , on = Set.singleton (Trigger.PushTrigger Trigger.pushTriggerDefaults)
  , jobs = Map.singleton (JobId.JobId "build") buildJob
  }

buildJob :: Job.Job
buildJob = Job.new
  { Job.jobName = Just "Build and Test"
  , Job.runsOn = Just "ubuntu-latest"
  , Job.steps = Just $ checkoutStep :| [buildStep, testStep]
  }

checkoutStep :: Step.Step
checkoutStep = Step.new
  { Step.name = Just "Checkout repository"
  , Step.uses = Just "actions/checkout@v4"
  }

buildStep :: Step.Step
buildStep = Step.new
  { Step.name = Just "Build project"
  , Step.run = Just "npm install && npm run build"
  }

testStep :: Step.Step
testStep = Step.new
  { Step.name = Just "Run tests"
  , Step.run = Just "npm test"
  }

-- Export to YAML
exportWorkflow :: IO ()
exportWorkflow = Yaml.encodeFile "workflow.yml" myWorkflow
```

### 2. Importing a YAML file into a Workflow representation

You can load an existing GitHub Actions YAML file into a Haskell `Workflow` type:

```haskell
{-# LANGUAGE TypeApplications #-}

import qualified Data.Yaml as Yaml
import Language.Github.Actions.Workflow (Workflow)

-- Import from YAML file
importWorkflow :: FilePath -> IO (Either String Workflow)
importWorkflow yamlFilePath = do
  result <- Yaml.decodeFileEither @Workflow yamlFilePath
  case result of
    Left parseException ->
      return $ Left $ Yaml.prettyPrintParseException parseException
    Right workflow ->
      return $ Right workflow

-- Example usage
main :: IO ()
main = do
  result <- importWorkflow ".github/workflows/ci.yml"
  case result of
    Left errorMsg -> putStrLn $ "Failed to parse workflow: " ++ errorMsg
    Right workflow -> do
      putStrLn "Successfully parsed workflow!"
      print workflow
```
