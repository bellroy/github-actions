{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Step.With
  ( StepWith (..),
    gen,
  )
where

import Control.Monad.Fail.Hoist (hoistFail')
import Data.Aeson (FromJSON, ToJSON (..), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Set qualified as Set
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Language.Github.Actions.Types (ObjectKey, genObjectKeyMap)
import Relude hiding (id)
import Text.NonEmpty (NonEmptyText)
import Text.NonEmpty qualified as NonEmptyText

data StepWithDockerArgsAttrs = StepWithDockerArgsAttrs
  { entryPoint :: NonEmptyText,
    args :: Maybe NonEmptyText
  }
  deriving stock (Eq, Generic, Ord, Show)

data StepWith
  = StepWithDockerArgs StepWithDockerArgsAttrs
  | StepWithEnv (NEMap ObjectKey NonEmptyText)
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON StepWith where
  parseJSON = Aeson.withObject "StepWith" $ \o ->
    let objectKeySet = fromList (AesonKeyMap.keys o)
        dockerKeySet = fromList ["entryPoint", "args"]
     in if objectKeySet `Set.isSubsetOf` dockerKeySet
          then do
            entryPoint <- o .: "entryPoint"
            args <- o .:? "args"
            pure . StepWithDockerArgs $ StepWithDockerArgsAttrs {..}
          else StepWithEnv <$> Aeson.parseJSON (Aeson.Object o)

instance ToJSON StepWith where
  toJSON = \case
    StepWithDockerArgs StepWithDockerArgsAttrs {..} ->
      Aeson.object $
        catMaybes
          [ Just $ "entryPoint" .= entryPoint,
            ("args" .=) <$> args
          ]
    StepWithEnv env ->
      toJSON env

gen :: (MonadGen m, MonadFail m) => m StepWith
gen =
  Gen.choice
    [ StepWithDockerArgs <$> do
        entryPoint <- NonEmptyText.gen Gen.alphaNum
        args <- Gen.maybe $ NonEmptyText.gen Gen.alphaNum
        pure StepWithDockerArgsAttrs {..},
      fmap StepWithEnv $
        genObjectKeyMap (NonEmptyText.gen Gen.alphaNum)
          >>= hoistFail'
            . maybeToRight "Empty map of step->with args"
            . NEMap.nonEmptyMap
    ]
