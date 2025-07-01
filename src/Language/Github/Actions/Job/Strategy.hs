{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Language.Github.Actions.Job.Strategy
-- Description : Matrix strategies for GitHub Actions jobs
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'JobStrategy' type for configuring matrix strategies
-- that allow jobs to run multiple times with different configurations.
--
-- Matrix strategies are useful for testing across multiple:
-- * Operating systems (Ubuntu, Windows, macOS)
-- * Language versions (Node 16, 18, 20)
-- * Database versions (PostgreSQL 12, 13, 14)
-- * Or any other configurable parameters
--
-- For more information about GitHub Actions matrix strategies, see:
-- <https://docs.github.com/en/actions/writing-workflows/choosing-when-your-workflow-runs/running-variations-of-jobs-in-a-workflow>
module Language.Github.Actions.Job.Strategy
  ( JobStrategy (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON, (.!=), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Matrix strategy configuration for running job variations.
--
-- Matrix strategies define how to run a job multiple times with different
-- configurations. The matrix creates a cross-product of all variable combinations,
-- with options to include additional combinations or exclude specific ones.
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Job.Strategy
-- import qualified Data.Map as Map
-- import qualified Data.Aeson as Aeson
--
-- -- Simple OS and Node version matrix
-- nodeMatrix :: JobStrategy
-- nodeMatrix = JobStrategy
--  { exclude = Nothing
--  , failFast = Just False
--  , include = Nothing
--  , maxParallel = Just 3
--  , otherVariables = Just $ Map.fromList
--      [ ("os", Aeson.toJSON ["ubuntu-latest", "windows-latest", "macos-latest"])
--      , ("node-version", Aeson.toJSON ["16", "18", "20"])
--      ]
--  }
--
-- -- Matrix with exclusions and includes
-- complexMatrix :: JobStrategy
-- complexMatrix = JobStrategy
--  { exclude = Just ["{ \"os\": \"windows-latest\", \"node-version\": \"16\" }"]
--  , failFast = Just True
--  , include = Just ["{ \"os\": \"ubuntu-latest\", \"node-version\": \"21\", \"experimental\": true }"]
--  , maxParallel = Nothing
--  , otherVariables = Just $ Map.fromList
--      [ ("os", Aeson.toJSON ["ubuntu-latest", "windows-latest"])
--      , ("node-version", Aeson.toJSON ["18", "20"])
--      ]
--  }
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/choosing-when-your-workflow-runs/running-variations-of-jobs-in-a-workflow>
data JobStrategy = JobStrategy
  { -- | Matrix combinations to exclude
    exclude :: Maybe [Text],
    -- | Whether to cancel all jobs when one fails
    failFast :: Maybe Bool,
    -- | Additional matrix combinations to include
    include :: Maybe [Text],
    -- | Maximum number of parallel jobs
    maxParallel :: Maybe Int,
    -- | Matrix variables (os, version, etc.)
    otherVariables :: Maybe (Map Text Aeson.Value)
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON JobStrategy where
  parseJSON = Aeson.withObject "JobStrategy" $ \o -> do
    matrix <- o .:? "matrix" .!= mempty
    exclude <- matrix .:? "exclude"
    failFast <- o .:? "fail-fast"
    include <- matrix .:? "include"
    maxParallel <- o .:? "max-parallel"
    rawMatrix :: Map Text Aeson.Value <- o .:? "matrix" .!= mempty
    let excludeKey :: Text = "exclude"
        includeKey :: Text = "include"
        filteredRawMatrix =
          Map.filterWithKey
            (\k _ -> k /= excludeKey && k /= includeKey)
            rawMatrix
        otherVariables =
          if null filteredRawMatrix
            then Nothing
            else Just filteredRawMatrix
    pure JobStrategy {..}

instance ToJSON JobStrategy where
  toJSON JobStrategy {..} =
    Aeson.object $
      catMaybes
        [ ("fail-fast" .=) <$> failFast,
          ("matrix" .=) <$> maybeMatrixObject,
          ("max-parallel" .=) <$> maxParallel
        ]
    where
      maybeMatrixObject :: Maybe Aeson.Value
      maybeMatrixObject =
        let pairs =
              maybe [] otherVariableMapToAesonPair otherVariables
                ++ catMaybes
                  [ ("exclude" .=) <$> exclude,
                    ("include" .=) <$> include
                  ]
         in if null pairs
              then Nothing
              else Just $ Aeson.object pairs

      otherVariableMapToAesonPair :: Map Text Aeson.Value -> [Aeson.Pair]
      otherVariableMapToAesonPair =
        Map.foldMapWithKey
          ( \k v ->
              [fromString (Text.unpack k) .= v]
          )

gen :: (MonadGen m) => m JobStrategy
gen = do
  exclude <- Gen.maybe $ Gen.list (Range.linear 1 3) genText
  failFast <- Gen.maybe Gen.bool
  include <- Gen.maybe $ Gen.list (Range.linear 1 3) genText
  maxParallel <- Gen.maybe $ Gen.int (Range.linear 1 10)
  otherVariables <- Gen.maybe $ (fmap . fmap) Aeson.String genTextMap
  pure JobStrategy {..}
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
    genTextMap = Gen.map (Range.linear 1 5) $ liftA2 (,) genText genText
