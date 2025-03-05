{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Service
  ( Service (..),
    gen,
    new,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

data Service = Service
  { credentials :: Maybe (Map Text Text),
    env :: Maybe (Map Text Text),
    image :: Maybe Text,
    options :: Maybe Text,
    ports :: Maybe [Text],
    volumes :: Maybe [Text]
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

gen :: (MonadGen m) => m Service
gen = do
  credentials <- Gen.maybe genTextMap
  env <- Gen.maybe genTextMap
  image <- Gen.maybe genText
  options <- Gen.maybe genText
  ports <- Gen.maybe . Gen.list (Range.linear 1 3) $ Gen.text (Range.linear 1 5) Gen.digit
  volumes <- Gen.maybe . Gen.list (Range.linear 1 3) $ genText
  pure Service {..}
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
    genTextMap = Gen.map (Range.linear 1 5) $ liftA2 (,) genText genText

new :: Service
new =
  Service
    { credentials = Nothing,
      env = Nothing,
      image = Nothing,
      options = Nothing,
      ports = Nothing,
      volumes = Nothing
    }
