{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Language.Github.Actions.Job.Environment
-- Description : Deployment environment configuration for GitHub Actions jobs
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'JobEnvironment' type for configuring deployment
-- environments that jobs target in GitHub Actions workflows.
--
-- Job environments allow you to:
-- * Target specific deployment environments (production, staging, etc.)
-- * Control access through environment protection rules
-- * Use environment-specific secrets and variables
-- * Track deployment history and status
--
-- For more information about GitHub Actions environments, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idenvironment>
module Language.Github.Actions.Job.Environment
  ( JobEnvironment (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Environment configuration for deployment jobs.
--
-- Job environments can be specified in two ways:
--
-- * 'NamedJobEnvironment' - Reference a named environment by string
-- * 'CustomJobEnvironment' - Configure environment with custom URL and other properties
--
-- Named environments are the most common and reference environments configured
-- in your repository settings. Custom environments allow inline configuration
-- for specific deployment scenarios.
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Job.Environment
-- import qualified Data.Map as Map
--
-- -- Reference a named environment
-- prodEnvironment :: JobEnvironment
-- prodEnvironment = NamedJobEnvironment "production"
--
-- stagingEnvironment :: JobEnvironment
-- stagingEnvironment = NamedJobEnvironment "staging"
--
-- -- Custom environment with URL
-- customEnvironment :: JobEnvironment
-- customEnvironment = CustomJobEnvironment $ Map.fromList
--  [ ("name", "review-pr-123")
--  , ("url", "https:\/\/pr-123.preview.example.com")
--  ]
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idenvironment>
data JobEnvironment
  = -- | Reference a named environment
    NamedJobEnvironment Text
  | -- | Custom environment configuration
    CustomJobEnvironment (Map Text Text)
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON JobEnvironment where
  parseJSON v = case v of
    Aeson.String s ->
      pure $ NamedJobEnvironment s
    Aeson.Object o -> CustomJobEnvironment <$> Aeson.parseJSON (Aeson.Object o)
    _ -> fail "JobEnvironment must be a string or an object"

instance ToJSON JobEnvironment where
  toJSON (NamedJobEnvironment s) = Aeson.String s
  toJSON (CustomJobEnvironment m) = Aeson.toJSON m

-- | Generate a random 'JobEnvironment' for property-based testing.
--
-- This generator creates job environments with randomized properties suitable for testing
-- JSON serialization roundtrips and other property-based tests.
gen :: (MonadGen m) => m JobEnvironment
gen =
  Gen.choice
    [ NamedJobEnvironment <$> genText,
      CustomJobEnvironment <$> genTextMap
    ]
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
    genTextMap = Gen.map (Range.linear 1 5) $ liftA2 (,) genText genText
