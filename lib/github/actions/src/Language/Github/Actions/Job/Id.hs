{-# LANGUAGE DerivingStrategies #-}

module Language.Github.Actions.Job.Id
  ( JobId (..),
    gen,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON (..), ToJSONKey)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Relude
import Text.NonEmpty (NonEmptyText)
import Text.NonEmpty qualified as NonEmptyText

newtype JobId = JobId NonEmptyText
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

gen :: (MonadGen m) => m JobId
gen = JobId <$> NonEmptyText.gen Gen.alphaNum
