{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Language.Github.Actions.Concurrency
-- Description : GitHub Actions concurrency settings
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'Concurrency' type for controlling concurrent execution
-- of GitHub Actions workflows and jobs.
--
-- Concurrency settings allow you to control how many workflow runs or job executions
-- can happen simultaneously.
--
-- For more information about GitHub Actions concurrency, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#concurrency>
module Language.Github.Actions.Concurrency
  ( Concurrency (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Concurrency settings for workflows and jobs.
--
-- Concurrency allows you to control whether multiple workflow runs or job executions
-- can happen simultaneously. This is useful for preventing conflicts when deploying
-- or when you want to ensure only one workflow processes a particular resource at a time.
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Concurrency
--
-- -- Only allow one deployment per branch
-- deploymentConcurrency :: Concurrency
-- deploymentConcurrency = Concurrency
--  { group = Just "${{ github.ref }}"
--  , cancelInProgress = Just "true"
--  }
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#concurrency>
data Concurrency = Concurrency
  { -- | Concurrency group identifier
    group :: Maybe Text,
    -- | Whether to cancel in-progress runs (can be "true", "false", or an expression)
    cancelInProgress :: Maybe Text
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON Concurrency where
  parseJSON = Aeson.withObject "Concurrency" $ \o -> do
    group <- o .:? "group"
    mVal <- o .:? "cancel-in-progress" :: Parser (Maybe Aeson.Value)
    cancelInProgress <- case mVal of
      Nothing -> pure Nothing
      Just (Aeson.Bool True) -> pure (Just "true")
      Just (Aeson.Bool False) -> pure (Just "false")
      Just (Aeson.String s) -> pure (Just s)
      Just _ -> fail "cancel-in-progress must be a boolean or string"
    pure Concurrency {..}

instance ToJSON Concurrency where
  toJSON Concurrency {..} =
    Aeson.object
      [ "group" .= group,
        "cancel-in-progress" .= fmap cancelInProgressToJSON cancelInProgress
      ]
    where
      cancelInProgressToJSON "true" = Aeson.Bool True
      cancelInProgressToJSON "false" = Aeson.Bool False
      cancelInProgressToJSON t = Aeson.String t

gen :: (MonadGen m) => m Concurrency
gen = do
  group <- Gen.maybe (Gen.text (Range.linear 1 5) Gen.alphaNum)
  cancelInProgress <- Gen.maybe (Gen.text (Range.linear 1 5) Gen.alphaNum)
  pure Concurrency {..}
