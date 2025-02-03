{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Step.With
  ( StepWith (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

data StepWithDockerArgsAttrs = StepWithDockerArgsAttrs
  { entryPoint :: Text,
    args :: Maybe Text
  }
  deriving stock (Eq, Generic, Ord, Show)

data StepWith
  = StepWithDockerArgs StepWithDockerArgsAttrs
  | StepWithEnv (Map Text Text)
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON StepWith where
  parseJSON = Aeson.withObject "StepWith" $ \o ->
    let objectKeySet = Set.fromList (AesonKeyMap.keys o)
        dockerKeySet = Set.fromList ["entryPoint", "args"]
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

gen :: (MonadGen m) => m StepWith
gen =
  Gen.choice
    [ StepWithDockerArgs <$> do
        entryPoint <- genText
        args <- Gen.maybe genText
        pure StepWithDockerArgsAttrs {..},
      StepWithEnv <$> genTextMap
    ]
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
    genTextMap = Gen.map (Range.linear 1 5) $ liftA2 (,) genText genText
