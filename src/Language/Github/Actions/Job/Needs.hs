{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : Language.Github.Actions.Job.Needs
-- Description : Job dependency specification for GitHub Actions
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'JobNeeds' type for representing job dependencies
-- in GitHub Actions workflows. GitHub Actions allows both single job and
-- multiple job dependencies for the 'needs' field.
--
-- Examples of valid 'needs' specifications:
-- * @needs: build@ - Single job dependency
-- * @needs: [build, test]@ - Multiple job dependencies
--
-- For more information about GitHub Actions job dependencies, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idneeds>
module Language.Github.Actions.Job.Needs
  ( JobNeeds (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), Value (..))
import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Language.Github.Actions.Job.Id (JobId)
import qualified Language.Github.Actions.Job.Id as JobId

-- | Job dependency specification that preserves YAML representation.
--
-- GitHub Actions supports flexible job dependency specification:
--
-- * 'JobNeedsString' - Single job dependency as string like @needs: build@
-- * 'JobNeedsArray' - Multiple job dependencies as array like @needs: [build, test]@
--
-- Examples:
--
-- @
-- -- Single job dependency (string form)
-- stringDep :: JobNeeds
-- stringDep = JobNeedsString (JobId "build")
--
-- -- Multiple job dependencies (array form)
-- arrayDeps :: JobNeeds
-- arrayDeps = JobNeedsArray (JobId "build" :| [JobId "test", JobId "lint"])
-- @
--
-- The type preserves the original YAML format during round-trip serialization.
-- A string input will serialize back to a string, and an array input will
-- serialize back to an array, preventing information loss.
data JobNeeds
  = -- | Single job dependency as string (e.g., @needs: build@)
    JobNeedsString JobId
  | -- | Multiple job dependencies as array (e.g., @needs: [build, test]@)
    JobNeedsArray (NonEmpty JobId)
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON JobNeeds where
  parseJSON v@(Array _) = JobNeedsArray <$> Aeson.parseJSON v
  parseJSON v = JobNeedsString <$> Aeson.parseJSON v

instance ToJSON JobNeeds where
  toJSON (JobNeedsString jobId) = toJSON jobId
  toJSON (JobNeedsArray jobIds) = toJSON jobIds

-- | Generate random 'JobNeeds' values for property testing.
gen :: (MonadGen m) => m JobNeeds
gen =
  Gen.choice
    [ JobNeedsString <$> JobId.gen,
      JobNeedsArray <$> Gen.nonEmpty (Range.linear 1 5) JobId.gen
    ]
