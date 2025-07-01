{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Language.Github.Actions.Service.Id
-- Description : Service identifiers for GitHub Actions workflows
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'ServiceId' type for uniquely identifying service
-- containers within GitHub Actions jobs.
--
-- Service IDs are used to reference service containers from job steps via hostname.
-- They must be unique within a job and follow specific naming conventions.
--
-- For more information about GitHub Actions service containers, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idservices>
module Language.Github.Actions.Service.Id
  ( ServiceId (..),
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

-- | A unique identifier for a service container within a GitHub Actions job.
--
-- Service IDs are used to:
-- * Reference the service container from job steps using the service ID as hostname
-- * Configure service-specific settings like ports, environment variables, and health checks
-- * Identify services in workflow run logs and container networks
--
-- Service IDs must be unique within a job and should follow these conventions:
-- * Start with a letter or underscore
-- * Contain only alphanumeric characters, hyphens, and underscores
-- * Be descriptive of the service's purpose
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Service.Id
--
-- -- Database service IDs
-- postgresServiceId :: ServiceId
-- postgresServiceId = ServiceId "postgres"
--
-- redisServiceId :: ServiceId
-- redisServiceId = ServiceId "redis"
--
-- -- More descriptive service IDs
-- testDatabaseServiceId :: ServiceId
-- testDatabaseServiceId = ServiceId "test-database"
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idservices>
newtype ServiceId = ServiceId Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

gen :: (MonadGen m) => m ServiceId
gen = ServiceId <$> Gen.text (Range.linear 1 5) Gen.alphaNum

render :: ServiceId -> Text
render (ServiceId t) = t
