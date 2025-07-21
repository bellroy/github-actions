{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Language.Github.Actions.RunIf
-- Description : Flexible conditional expressions for GitHub Actions
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'RunIf' type for representing conditional expressions
-- in GitHub Actions workflows. GitHub Actions allows both boolean and string
-- expressions in 'if' conditions for jobs and steps.
--
-- Examples of valid 'if' conditions:
-- * @if: false@ - Simple boolean
-- * @if: "github.ref == 'refs/heads/main'"@ - GitHub expression
-- * @if: "\${{ success() && matrix.os == 'ubuntu-latest' }}"@ - Complex expression
--
-- For more information about GitHub Actions conditional expressions, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idif>
module Language.Github.Actions.RunIf
  ( RunIf (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), Value (..))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | A conditional expression that can be either a boolean or string.
--
-- GitHub Actions supports flexible 'if' conditions:
--
-- * 'RunIfBool' - Simple boolean values like @true@ or @false@
-- * 'RunIfString' - GitHub expressions like @"github.ref == 'refs/heads/main'"@
--
-- Examples:
--
-- @
-- -- Simple boolean condition
-- simpleFalse :: RunIf
-- simpleFalse = RunIfBool False
--
-- -- GitHub expression condition
-- branchCheck :: RunIf
-- branchCheck = RunIfString "github.ref == 'refs/heads/main'"
--
-- -- Complex expression with functions
-- complexCheck :: RunIf
-- complexCheck = RunIfString "\${{ success() && matrix.os == 'ubuntu-latest' }}"
-- @
--
-- The type preserves the original format during round-trip serialization,
-- so a boolean input remains a boolean in the output YAML.
data RunIf
  = -- | Boolean condition (e.g., @false@, @true@)
    RunIfBool Bool
  | -- | String expression (e.g., @"github.ref == 'refs/heads/main'"@)
    RunIfString Text
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON RunIf where
  parseJSON (Bool b) = pure $ RunIfBool b
  parseJSON (String s) = pure $ RunIfString s
  parseJSON v = fail $ "Expected Bool or String for RunIf, got: " ++ show v

instance ToJSON RunIf where
  toJSON (RunIfBool b) = Bool b
  toJSON (RunIfString s) = String s

-- | Generate random 'RunIf' values for property testing.
gen :: (MonadGen m) => m RunIf
gen =
  Gen.choice
    [ RunIfBool <$> Gen.bool,
      RunIfString <$> Gen.text (Range.linear 5 50) Gen.alphaNum
    ]
