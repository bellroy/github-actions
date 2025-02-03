{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Job.Strategy
  ( JobStrategy (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON, (.!=), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

data JobStrategy = JobStrategy
  { exclude :: Maybe [Text],
    failFast :: Maybe Bool,
    include :: Maybe [Text],
    maxParallel :: Maybe Int,
    otherVariables :: Maybe (Map Text Aeson.Value)
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON JobStrategy where
  parseJSON = Aeson.withObject "JobStrategy" $ \o -> do
    matrix <- o .:? "matrix" .!= mempty
    exclude <- matrix .:? "exclude"
    failFast <- o .:? "fail-fast"
    include <- matrix .:? "include"
    maxParallel <- o .:? "max-parallel"
    rawMatrix :: Map Text Aeson.Value <- o .:? "matrix" .!= mempty
    let excludeKey :: Text = "exclude"
        includeKey :: Text = "include"
        filteredRawMatrix =
          Map.filterWithKey
            (\k _ -> k /= excludeKey && k /= includeKey)
            rawMatrix
        otherVariables =
          if null filteredRawMatrix
            then Nothing
            else Just filteredRawMatrix
    pure JobStrategy {..}

instance ToJSON JobStrategy where
  toJSON JobStrategy {..} =
    Aeson.object $
      catMaybes
        [ ("fail-fast" .=) <$> failFast,
          ("matrix" .=) <$> maybeMatrixObject,
          ("max-parallel" .=) <$> maxParallel
        ]
    where
      maybeMatrixObject :: Maybe Aeson.Value
      maybeMatrixObject =
        let pairs =
              maybe [] otherVariableMapToAesonPair otherVariables
                ++ catMaybes
                  [ ("exclude" .=) <$> exclude,
                    ("include" .=) <$> include
                  ]
         in if null pairs
              then Nothing
              else Just $ Aeson.object pairs

      otherVariableMapToAesonPair :: Map Text Aeson.Value -> [Aeson.Pair]
      otherVariableMapToAesonPair =
        Map.foldMapWithKey
          ( \k v ->
              [fromString (Text.unpack k) .= v]
          )

gen :: (MonadGen m) => m JobStrategy
gen = do
  exclude <- Gen.maybe $ Gen.list (Range.linear 1 3) genText
  failFast <- Gen.maybe Gen.bool
  include <- Gen.maybe $ Gen.list (Range.linear 1 3) genText
  maxParallel <- Gen.maybe $ Gen.int (Range.linear 1 10)
  otherVariables <- Gen.maybe $ (fmap . fmap) Aeson.String genTextMap
  pure JobStrategy {..}
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
    genTextMap = Gen.map (Range.linear 1 5) $ liftA2 (,) genText genText
