{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Language.Github.Actions.Job.Container
-- Description : Container configuration for GitHub Actions jobs
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'JobContainer' type for configuring Docker containers
-- that jobs run inside of in GitHub Actions workflows.
--
-- Job containers allow you to run job steps inside a Docker container with a specific
-- environment, dependencies, and configuration. This provides consistency across
-- different runner environments and enables the use of custom tooling.
--
-- For more information about GitHub Actions job containers, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idcontainer>
module Language.Github.Actions.Job.Container
  ( JobContainer (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON, (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Container configuration for running a job inside a Docker container.
--
-- Job containers provide an isolated, consistent environment for job execution.
-- This is useful for ensuring specific tool versions, operating system environments,
-- or complex dependency setups.
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Job.Container
-- import qualified Data.Map as Map
--
-- -- Node.js development container
-- nodeContainer :: JobContainer
-- nodeContainer = JobContainer
--  { image = Just "node:18"
--  , env = Just $ Map.fromList [("NODE_ENV", "test")]
--  , credentials = Nothing
--  , options = Nothing
--  , ports = Nothing
--  , volumes = Nothing
--  }
--
-- -- Database testing container with services
-- dbTestContainer :: JobContainer
-- dbTestContainer = JobContainer
--  { image = Just "ubuntu:22.04"
--  , env = Just $ Map.fromList [("DEBIAN_FRONTEND", "noninteractive")]
--  , credentials = Nothing
--  , options = Just "--network postgres"
--  , ports = Just ["8080:8080"]
--  , volumes = Just ["\${{ github.workspace }}:\/workspace"]
--  }
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idcontainer>
data JobContainer = JobContainer
  { -- | Registry credentials for private images
    credentials :: Maybe (Map Text Text),
    -- | Environment variables for the container
    env :: Maybe (Map Text Text),
    -- | Docker image to use for the container
    image :: Maybe Text,
    -- | Additional Docker run options
    options :: Maybe Text,
    -- | Ports to expose from the container
    ports :: Maybe [Text],
    -- | Volumes to mount in the container
    volumes :: Maybe [Text]
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON JobContainer where
  parseJSON = Aeson.withObject "JobContainer" $ \o -> do
    credentials <- o .:? "credentials"
    env <- o .:? "env"
    image <- o .:? "image"
    options <- o .:? "options"
    ports <- o .:? "ports"
    volumes <- o .:? "volumes"
    pure JobContainer {..}

instance ToJSON JobContainer where
  toJSON JobContainer {..} =
    Aeson.object $
      catMaybes
        [ ("credentials" .=) <$> credentials,
          ("env" .=) <$> env,
          ("image" .=) <$> image,
          ("options" .=) <$> options,
          ("ports" .=) <$> ports,
          ("volumes" .=) <$> volumes
        ]

gen :: (MonadGen m) => m JobContainer
gen = do
  credentials <- Gen.maybe genTextMap
  env <- Gen.maybe genTextMap
  image <- Gen.maybe genText
  options <- Gen.maybe genText
  ports <-
    Gen.maybe . Gen.list (Range.linear 0 10) $
      Gen.text (Range.linear 1 5) Gen.digit
  volumes <- Gen.maybe $ Gen.list (Range.linear 0 10) genText
  pure JobContainer {..}
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
    genTextMap = Gen.map (Range.linear 1 5) $ liftA2 (,) genText genText
