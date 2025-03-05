{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Github.Actions.Job.Environment
  ( JobEnvironment (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

data JobEnvironment
  = NamedJobEnvironment Text
  | CustomJobEnvironment (Map Text Text)
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON JobEnvironment where
  parseJSON v = case v of
    Aeson.String s ->
      pure $ NamedJobEnvironment s
    Aeson.Object o -> CustomJobEnvironment <$> Aeson.parseJSON (Aeson.Object o)
    _ -> fail "JobEnvironment must be a string or an object"

instance ToJSON JobEnvironment where
  toJSON (NamedJobEnvironment s) = Aeson.String s
  toJSON (CustomJobEnvironment m) = Aeson.toJSON m

gen :: (MonadGen m) => m JobEnvironment
gen =
  Gen.choice
    [ NamedJobEnvironment <$> genText,
      CustomJobEnvironment <$> genTextMap
    ]
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
    genTextMap = Gen.map (Range.linear 1 5) $ liftA2 (,) genText genText
