{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Github.Actions.Job.Strategy
  ( JobStrategy (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON, (.!=), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map qualified as Map
import Data.Text qualified as Text
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.Github.Actions.Types (ObjectKey, genObjectKey)
import Refined (refineTH, unrefine)
import Relude
import Text.NonEmpty (NonEmptyText)
import Text.NonEmpty qualified as NonEmptyText

data JobStrategy = JobStrategy
  { exclude :: Maybe [NonEmptyText],
    failFast :: Maybe Bool,
    include :: Maybe [NonEmptyText],
    maxParallel :: Maybe Int,
    otherVariables :: Maybe (Map ObjectKey Aeson.Value)
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON JobStrategy where
  parseJSON = Aeson.withObject "JobStrategy" $ \o -> do
    matrix <- o .:? "matrix" .!= mempty
    exclude <- matrix .:? "exclude"
    failFast <- o .:? "fail-fast"
    include <- matrix .:? "include"
    maxParallel <- o .:? "max-parallel"
    rawMatrix :: Map ObjectKey Aeson.Value <- o .:? "matrix" .!= mempty
    let excludeKey :: ObjectKey = $$(refineTH "exclude")
        includeKey :: ObjectKey = $$(refineTH "include")
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

      otherVariableMapToAesonPair :: Map ObjectKey Aeson.Value -> [Aeson.Pair]
      otherVariableMapToAesonPair =
        Map.foldMapWithKey
          ( \k v ->
              [(fromString . Text.unpack $ unrefine k) .= v]
          )

gen :: (MonadGen m, MonadFail m) => m JobStrategy
gen = do
  exclude <- Gen.maybe . Gen.list (Range.linear 1 3) $ NonEmptyText.gen Gen.alphaNum
  failFast <- Gen.maybe Gen.bool
  include <- Gen.maybe . Gen.list (Range.linear 1 3) $ NonEmptyText.gen Gen.alphaNum
  maxParallel <- Gen.maybe $ Gen.int (Range.linear 1 10)
  otherVariables <-
    Gen.maybe $
      Gen.map (Range.linear 1 3) $
        liftA2
          (,)
          genObjectKey
          (Aeson.String <$> Gen.text (Range.linear 1 10) Gen.alphaNum)
  pure JobStrategy {..}
