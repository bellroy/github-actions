{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Language.Github.Actions.Job.Id
-- Description : Job identifiers for GitHub Actions workflows
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'JobId' type for uniquely identifying jobs within
-- GitHub Actions workflows.
--
-- Job IDs are used to reference jobs in dependency declarations (needs), outputs,
-- and other cross-job references. They must be unique within a workflow and follow
-- specific naming conventions.
--
-- For more information about GitHub Actions job IDs, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_id>
module Language.Github.Actions.Job.Id
  ( JobId (..),
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

-- | A unique identifier for a job within a GitHub Actions workflow.
--
-- Job IDs are used to:
-- * Reference jobs in dependency declarations (needs)
-- * Access job outputs from other jobs
-- * Identify jobs in workflow run logs and API responses
--
-- Job IDs must be unique within a workflow and should follow these conventions:
-- * Start with a letter or underscore
-- * Contain only alphanumeric characters, hyphens, and underscores
-- * Be descriptive of the job's purpose
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Job.Id
--
-- -- Simple job IDs
-- buildJobId :: JobId
-- buildJobId = JobId "build"
--
-- testJobId :: JobId
-- testJobId = JobId "test"
--
-- -- More descriptive job IDs
-- deployProdJobId :: JobId
-- deployProdJobId = JobId "deploy-production"
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_id>
newtype JobId = JobId Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

-- | Generate a random 'JobId' for property-based testing.
gen :: (MonadGen m) => m JobId
gen = JobId <$> Gen.text (Range.linear 1 5) Gen.alphaNum

-- | Extract the text representation of a 'JobId'.
render :: JobId -> Text
render (JobId t) = t
