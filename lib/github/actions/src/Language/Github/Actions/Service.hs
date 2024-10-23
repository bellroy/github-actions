{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Service
  ( Service (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.Github.Actions.Types (ObjectKey, genObjectKeyMap)
import Relude
import Text.NonEmpty (NonEmptyText)
import Text.NonEmpty qualified as NonEmptyText

data Service = Service
  { credentials :: Maybe (Map ObjectKey NonEmptyText),
    env :: Maybe (Map ObjectKey NonEmptyText),
    image :: Maybe NonEmptyText,
    options :: Maybe NonEmptyText,
    ports :: Maybe [NonEmptyText],
    volumes :: Maybe [NonEmptyText]
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON Service where
  parseJSON = Aeson.withObject "Service" $ \o -> do
    credentials <- o .:? "credentials"
    env <- o .:? "env"
    image <- o .:? "image"
    options <- o .:? "options"
    ports <- o .:? "ports"
    volumes <- o .:? "volumes"
    pure Service {..}

instance ToJSON Service where
  toJSON Service {..} =
    Aeson.object $
      catMaybes
        [ ("credentials" .=) <$> credentials,
          ("env" .=) <$> env,
          ("image" .=) <$> image,
          ("options" .=) <$> options,
          ("ports" .=) <$> ports,
          ("volumes" .=) <$> volumes
        ]

gen :: (MonadGen m, MonadFail m) => m Service
gen = do
  credentials <- Gen.maybe $ genObjectKeyMap (NonEmptyText.gen Gen.alphaNum)
  env <- Gen.maybe $ genObjectKeyMap (NonEmptyText.gen Gen.alphaNum)
  image <- Gen.maybe $ NonEmptyText.gen Gen.alphaNum
  options <- Gen.maybe $ NonEmptyText.gen Gen.alphaNum
  ports <- Gen.maybe . Gen.list (Range.linear 1 3) $ NonEmptyText.gen Gen.digit
  volumes <- Gen.maybe . Gen.list (Range.linear 1 3) $ NonEmptyText.gen Gen.alphaNum
  pure Service {..}
