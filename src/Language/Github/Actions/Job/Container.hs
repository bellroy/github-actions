{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Job.Container
  ( JobContainer (..),
    gen,
  )
where

import Control.Applicative (liftA2, pure)
import Data.Aeson (FromJSON, ToJSON, (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Map (Map)
import Data.Maybe (Maybe (..), catMaybes)
import Data.Ord (Ord)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Text.Show (Show)

data JobContainer = JobContainer
  { credentials :: Maybe (Map Text Text),
    env :: Maybe (Map Text Text),
    image :: Maybe Text,
    options :: Maybe Text,
    ports :: Maybe [Text],
    volumes :: Maybe [Text]
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

gen :: (MonadGen m) => m JobContainer
gen = do
  credentials <- Gen.maybe genTextMap
  env <- Gen.maybe genTextMap
  image <- Gen.maybe genText
  options <- Gen.maybe genText
  ports <-
    Gen.maybe . Gen.list (Range.linear 0 10) $
      Gen.text (Range.linear 1 5) Gen.digit
  volumes <- Gen.maybe $ Gen.list (Range.linear 0 10) genText
  pure JobContainer {..}
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
    genTextMap = Gen.map (Range.linear 1 5) $ liftA2 (,) genText genText
