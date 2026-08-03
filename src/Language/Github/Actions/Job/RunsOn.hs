{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : Language.Github.Actions.Job.RunsOn
-- Description : Runner specification for GitHub Actions jobs
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'RunsOn' type for representing the runner
-- configuration of a GitHub Actions job. GitHub Actions allows both strings
-- and lists of strings for the 'runs-on' field.
--
-- Examples of valid 'runs-on' specifications:
-- * @runs-on: ubuntu-latest@ - A single runner specified as a string
-- * @runs-on: [self-hosted, linux]@ - A runner specified as a list of labels
-- * @runs-on: [self-hosted, linux, x64]@ - A runner specified as a list of labels
--
-- For more information about GitHub Actions runner selection, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idruns-on>
module Language.Github.Actions.Job.RunsOn
  ( RunsOn (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), Value (..))
import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Runner specification that preserves YAML representation.
--
-- GitHub Actions supports flexible runner specification:
--
-- * 'RunsOnString' - A single runner label as a string like @runs-on: ubuntu-latest@
-- * 'RunsOnArray' - A list of runner labels like @runs-on: [self-hosted, linux]@
--
-- Examples:
--
-- @
-- -- Single runner label (string form)
-- stringRunner :: RunsOn
-- stringRunner = RunsOnString "ubuntu-latest"
--
-- -- Multiple runner labels (array form)
-- arrayRunner :: RunsOn
-- arrayRunner = RunsOnArray ("self-hosted" :| ["linux", "x64"])
-- @
--
-- The type preserves the original YAML format during round-trip serialization.
-- A string input will serialize back to a string, and an array input will
-- serialize back to an array, preventing information loss.
data RunsOn
  = RunsOnString Text
  | RunsOnArray (NonEmpty Text)
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON RunsOn where
  parseJSON v@(Array _) = RunsOnArray <$> Aeson.parseJSON v
  parseJSON v = RunsOnString <$> Aeson.parseJSON v

instance ToJSON RunsOn where
  toJSON (RunsOnString label) = toJSON label
  toJSON (RunsOnArray labels) = toJSON labels

gen :: (MonadGen m) => m RunsOn
gen =
  Gen.choice
    [ RunsOnString <$> genText,
      RunsOnArray <$> Gen.nonEmpty (Range.linear 1 5) genText
    ]
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
