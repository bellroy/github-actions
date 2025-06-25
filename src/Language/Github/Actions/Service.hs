{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Language.Github.Actions.Service
-- Description : GitHub Actions service containers for jobs
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'Service' type for defining service containers that run
-- alongside job steps in GitHub Actions workflows.
--
-- Service containers are Docker containers that provide services like databases,
-- message queues, or other dependencies that your job steps might need during execution.
-- They run in parallel with your job and are accessible via hostname.
--
-- For more information about GitHub Actions service containers, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idservices>
module Language.Github.Actions.Service
  ( Service (..),
    gen,
    new,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | A service container definition for GitHub Actions jobs.
--
-- Service containers run Docker images that provide services your job steps can use.
-- Common examples include databases, caches, and message queues.
--
-- Service containers are automatically started before job steps run and stopped after
-- the job completes. They can be accessed by job steps using their service ID as hostname.
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Service
-- import qualified Data.Map as Map
--
-- -- PostgreSQL database service
-- postgresService :: Service
-- postgresService = new
--  { image = Just "postgres:13"
--  , env = Just $ Map.fromList
--      [ ("POSTGRES_PASSWORD", "postgres")
--      , ("POSTGRES_DB", "testdb")
--      ]
--  , ports = Just ["5432:5432"]
--  }
--
-- -- Redis cache service
-- redisService :: Service
-- redisService = new
--  { image = Just "redis:6-alpine"
--  , ports = Just ["6379:6379"]
--  }
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idservices>
data Service = Service
  { -- | Registry credentials for private images
    credentials :: Maybe (Map Text Text),
    -- | Environment variables for the service
    env :: Maybe (Map Text Text),
    -- | Docker image to use for the service
    image :: Maybe Text,
    -- | Additional Docker options
    options :: Maybe Text,
    -- | Ports to expose from the service
    ports :: Maybe [Text],
    -- | Volumes to mount in the service
    volumes :: Maybe [Text]
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON Service where
  parseJSON = Aeson.withObject "Service" $ \o -> do
    credentials <- o .:? "credentials"
    env <- o .:? "env"
    image <- o .:? "image"
    options <- o .:? "options"
    ports <- o .:? "ports"
    volumes <- o .:? "volumes"
    pure Service {..}

instance ToJSON Service where
  toJSON Service {..} =
    Aeson.object $
      catMaybes
        [ ("credentials" .=) <$> credentials,
          ("env" .=) <$> env,
          ("image" .=) <$> image,
          ("options" .=) <$> options,
          ("ports" .=) <$> ports,
          ("volumes" .=) <$> volumes
        ]

-- | Generate a random 'Service' for property-based testing.
--
-- This generator creates services with randomized properties suitable for testing
-- JSON serialization roundtrips and other property-based tests.
gen :: (MonadGen m) => m Service
gen = do
  credentials <- Gen.maybe genTextMap
  env <- Gen.maybe genTextMap
  image <- Gen.maybe genText
  options <- Gen.maybe genText
  ports <- Gen.maybe . Gen.list (Range.linear 1 3) $ Gen.text (Range.linear 1 5) Gen.digit
  volumes <- Gen.maybe . Gen.list (Range.linear 1 3) $ genText
  pure Service {..}
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
    genTextMap = Gen.map (Range.linear 1 5) $ liftA2 (,) genText genText

-- | Create a new empty 'Service' with default values.
--
-- This provides a minimal service definition that can be extended with specific
-- image, ports, environment variables, and other configuration.
--
-- Example:
--
-- @
-- databaseService = new
--   { image = Just "postgres:13"
--   , env = Just $ Map.singleton "POSTGRES_PASSWORD" "secret"
--   , ports = Just ["5432:5432"]
--   }
-- @
new :: Service
new =
  Service
    { credentials = Nothing,
      env = Nothing,
      image = Nothing,
      options = Nothing,
      ports = Nothing,
      volumes = Nothing
    }
