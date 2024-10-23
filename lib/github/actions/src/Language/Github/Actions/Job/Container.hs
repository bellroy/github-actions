{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Job.Container
  ( JobContainer (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON, (.:?), (.=))
import Data.Aeson qualified as Aeson
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.Github.Actions.Types (ObjectKey, genObjectKeyMap)
import Relude
import Text.NonEmpty (NonEmptyText)
import Text.NonEmpty qualified as NonEmptyText

data JobContainer = JobContainer
  { credentials :: Maybe (Map ObjectKey NonEmptyText),
    env :: Maybe (Map ObjectKey NonEmptyText),
    image :: Maybe NonEmptyText,
    options :: Maybe NonEmptyText,
    ports :: Maybe [NonEmptyText],
    volumes :: Maybe [NonEmptyText]
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON JobContainer where
  parseJSON = Aeson.withObject "JobContainer" $ \o -> do
    credentials <- o .:? "credentials"
    env <- o .:? "env"
    image <- o .:? "image"
    options <- o .:? "options"
    ports <- o .:? "ports"
    volumes <- o .:? "volumes"
    pure JobContainer {..}

instance ToJSON JobContainer where
  toJSON JobContainer {..} =
    Aeson.object $
      catMaybes
        [ ("credentials" .=) <$> credentials,
          ("env" .=) <$> env,
          ("image" .=) <$> image,
          ("options" .=) <$> options,
          ("ports" .=) <$> ports,
          ("volumes" .=) <$> volumes
        ]

gen :: (MonadGen m, MonadFail m) => m JobContainer
gen = do
  credentials <- Gen.maybe $ genObjectKeyMap (NonEmptyText.gen Gen.alphaNum)
  env <- Gen.maybe $ genObjectKeyMap (NonEmptyText.gen Gen.alphaNum)
  image <- Gen.maybe $ NonEmptyText.gen Gen.alphaNum
  options <- Gen.maybe $ NonEmptyText.gen Gen.alphaNum
  ports <- Gen.maybe . Gen.list (Range.linear 0 10) $ NonEmptyText.gen Gen.digit
  volumes <-
    Gen.maybe . Gen.list (Range.linear 0 10) $
      NonEmptyText.gen Gen.alphaNum
  pure JobContainer {..}
