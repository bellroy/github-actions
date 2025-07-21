{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Language.Github.Actions.UnstructuredMap
-- Description : Flexible value types for GitHub Actions YAML parsing
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'UnstructuredMap' type for representing a map of
-- values that can be strings, numbers, or booleans in GitHub Actions YAML files.
--
-- GitHub Actions allows flexible typing in many contexts:
-- * @retention-days: 1@ (number)
-- * @retention-days: "1"@ (string)
-- * @should-retain: false@ (boolean)
--
-- This type preserves the original YAML type during round-trip parsing,
-- ensuring that numeric values remain numeric and strings remain strings.
module Language.Github.Actions.UnstructuredMap
  ( UnstructuredValue (..),
    UnstructuredMap (..),
    renderUnstructuredValue,
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), Value (..))
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | A map that can have values of string, number, or boolean.
--
-- This type is designed to handle the flexible typing that GitHub Actions
-- allows in YAML files.
--
-- The type preserves the original format during round-trip serialization,
-- so numeric inputs remain numeric in the output YAML.
data UnstructuredValue
  = UnstructuredValueString Text
  | UnstructuredValueNumber Double
  | UnstructuredValueBool Bool
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON UnstructuredValue where
  parseJSON (String s) = pure $ UnstructuredValueString s
  parseJSON (Number n) = pure $ UnstructuredValueNumber (realToFrac n)
  parseJSON (Bool b) = pure $ UnstructuredValueBool b
  parseJSON v = fail $ "Expected String, Number, or Bool for UnstructuredValue, got: " ++ show v

instance ToJSON UnstructuredValue where
  toJSON (UnstructuredValueString s) = String s
  toJSON (UnstructuredValueNumber n) = Number (fromRational (toRational n))
  toJSON (UnstructuredValueBool b) = Bool b

renderUnstructuredValue :: UnstructuredValue -> Text
renderUnstructuredValue (UnstructuredValueString s) = s
renderUnstructuredValue (UnstructuredValueNumber n) =
  -- Format numbers nicely, avoiding unnecessary decimal places for integers
  if n == fromInteger (round n)
    then Text.pack (show (round n :: Integer))
    else Text.pack (show n)
renderUnstructuredValue (UnstructuredValueBool b) = if b then "true" else "false"

newtype UnstructuredMap = UnstructuredMap (Map Text UnstructuredValue)
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

genUnstructuredValue :: (MonadGen m) => m UnstructuredValue
genUnstructuredValue =
  Gen.choice
    [ UnstructuredValueString <$> Gen.text (Range.linear 1 20) Gen.alphaNum,
      UnstructuredValueNumber <$> Gen.realFloat (Range.linearFrac 0 1000),
      UnstructuredValueBool <$> Gen.bool
    ]

gen :: (MonadGen m) => m UnstructuredMap
gen = UnstructuredMap <$> Gen.map (Range.linear 0 10) genKeyValue
  where
    genKeyValue = do
      key <- Gen.text (Range.linear 1 20) Gen.alphaNum
      value <- genUnstructuredValue
      pure (key, value)
