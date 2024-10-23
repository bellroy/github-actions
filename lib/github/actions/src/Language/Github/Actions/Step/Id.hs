{-# LANGUAGE DerivingStrategies #-}

module Language.Github.Actions.Step.Id
  ( StepId (..),
    gen,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON (..), ToJSONKey)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Relude
import Text.NonEmpty (NonEmptyText)
import Text.NonEmpty qualified as NonEmptyText

newtype StepId = StepId NonEmptyText
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

gen :: (MonadGen m) => m StepId
gen = StepId <$> NonEmptyText.gen Gen.alphaNum
