{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Concurrency
  ( Concurrency (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Relude hiding (group)
import Text.NonEmpty (NonEmptyText)
import Text.NonEmpty qualified as NonEmptyText

data Concurrency = Concurrency
  { group :: Maybe NonEmptyText,
    cancelInProgress :: Maybe Bool
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON Concurrency where
  parseJSON = Aeson.withObject "Concurrency" $ \o -> do
    group <- o .:? "group"
    cancelInProgress <- o .:? "cancel-in-progress"
    pure Concurrency {..}

instance ToJSON Concurrency where
  toJSON Concurrency {..} =
    Aeson.object
      [ "group" .= group,
        "cancel-in-progress" .= cancelInProgress
      ]

gen :: (MonadGen m) => m Concurrency
gen = do
  group <- Gen.maybe (NonEmptyText.gen Gen.alphaNum)
  cancelInProgress <- Gen.maybe Gen.bool
  pure Concurrency {..}
