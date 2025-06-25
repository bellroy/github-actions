{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Language.Github.Actions.Step.With
-- Description : Input parameters for GitHub Actions steps
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'StepWith' type for specifying input parameters
-- to GitHub Actions steps that use pre-built actions.
--
-- The 'with' keyword allows you to pass inputs to actions, which can be:
-- * Environment variables for the action
-- * Docker container arguments (for Docker actions)
-- * Configuration parameters specific to the action
--
-- For more information about GitHub Actions step inputs, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idstepswith>
module Language.Github.Actions.Step.With
  ( StepWith (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKeyMap
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Docker container arguments for Docker actions.
--
-- Specifies the entry point and optional arguments for Docker actions.
data StepWithDockerArgsAttrs = StepWithDockerArgsAttrs
  { -- | Docker container entry point
    entryPoint :: Text,
    -- | Optional arguments to pass to the entry point
    args :: Maybe Text
  }
  deriving stock (Eq, Generic, Ord, Show)

-- | Input parameters for GitHub Actions steps.
--
-- Step inputs can be specified in two main ways:
--
-- * 'StepWithDockerArgs' - For Docker actions that need entry point and arguments
-- * 'StepWithEnv' - For actions that accept environment variables or general inputs
--
-- Most actions use the environment variable format, where inputs are passed
-- as key-value pairs.
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Step.With
-- import qualified Data.Map as Map
--
-- -- Environment inputs for actions/checkout
-- checkoutInputs :: StepWith
-- checkoutInputs = StepWithEnv $ Map.fromList
--  [ ("repository", "owner/repo")
--  , ("ref", "main")
--  , ("token", "\${{ secrets.GITHUB_TOKEN }}")
--  ]
--
-- -- Docker action inputs
-- dockerInputs :: StepWith
-- dockerInputs = StepWithDockerArgs $ StepWithDockerArgsAttrs
--  { entryPoint = "/entrypoint.sh"
--  , args = Just "arg1 arg2"
--  }
--
-- -- Action inputs for actions/upload-artifact
-- uploadInputs :: StepWith
-- uploadInputs = StepWithEnv $ Map.fromList
--  [ ("name", "build-artifacts")
--  , ("path", "dist/")
--  , ("retention-days", "30")
--  ]
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idstepswith>
data StepWith
  = -- | Docker action arguments
    StepWithDockerArgs StepWithDockerArgsAttrs
  | -- | Environment variables/general inputs
    StepWithEnv (Map Text Text)
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON StepWith where
  parseJSON = Aeson.withObject "StepWith" $ \o ->
    let objectKeySet = Set.fromList (AesonKeyMap.keys o)
        dockerKeySet = Set.fromList ["entryPoint", "args"]
     in if objectKeySet `Set.isSubsetOf` dockerKeySet
          then do
            entryPoint <- o .: "entryPoint"
            args <- o .:? "args"
            pure . StepWithDockerArgs $ StepWithDockerArgsAttrs {..}
          else StepWithEnv <$> Aeson.parseJSON (Aeson.Object o)

instance ToJSON StepWith where
  toJSON = \case
    StepWithDockerArgs StepWithDockerArgsAttrs {..} ->
      Aeson.object $
        catMaybes
          [ Just $ "entryPoint" .= entryPoint,
            ("args" .=) <$> args
          ]
    StepWithEnv env ->
      toJSON env

-- | Generate a random 'StepWith' for property-based testing.
--
-- This generator creates step inputs with randomized properties suitable for testing
-- JSON serialization roundtrips and other property-based tests.
gen :: (MonadGen m) => m StepWith
gen =
  Gen.choice
    [ StepWithDockerArgs <$> do
        entryPoint <- genText
        args <- Gen.maybe genText
        pure StepWithDockerArgsAttrs {..},
      StepWithEnv <$> genTextMap
    ]
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
    genTextMap = Gen.map (Range.linear 1 5) $ liftA2 (,) genText genText
