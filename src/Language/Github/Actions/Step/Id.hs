{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Language.Github.Actions.Step.Id
-- Description : Step identifiers for GitHub Actions workflows
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'StepId' type for uniquely identifying steps within
-- GitHub Actions jobs.
--
-- Step IDs are used to reference step outputs from later steps in the same job
-- or from other jobs. They must be unique within a job and follow specific
-- naming conventions.
--
-- For more information about GitHub Actions step IDs, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idstepsid>
module Language.Github.Actions.Step.Id
  ( StepId (..),
    gen,
    render,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON (..), ToJSONKey)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | A unique identifier for a step within a GitHub Actions job.
--
-- Step IDs are used to:
-- * Reference step outputs from later steps using @steps.<step_id>.outputs.<output_name>@
-- * Reference step outcomes using @steps.<step_id>.outcome@ or @steps.<step_id>.conclusion@
-- * Create dependencies between steps (though steps run sequentially by default)
--
-- Step IDs must be unique within a job and should follow these conventions:
-- * Start with a letter or underscore
-- * Contain only alphanumeric characters, hyphens, and underscores
-- * Be descriptive of the step's purpose
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Step.Id
--
-- -- Simple step IDs
-- checkoutStepId :: StepId
-- checkoutStepId = StepId "checkout"
--
-- buildStepId :: StepId
-- buildStepId = StepId "build"
--
-- -- More descriptive step IDs
-- uploadArtifactsStepId :: StepId
-- uploadArtifactsStepId = StepId "upload-build-artifacts"
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idstepsid>
newtype StepId = StepId Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

-- | Generate a random 'StepId' for property-based testing.
gen :: (MonadGen m) => m StepId
gen = StepId <$> Gen.text (Range.linear 1 5) Gen.alphaNum

-- | Extract the text representation of a 'StepId'.
render :: StepId -> Text
render (StepId t) = t
