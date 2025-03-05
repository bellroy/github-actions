{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Github.Actions.Step.Id
  ( StepId (..),
    gen,
    render,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON (..), ToJSONKey)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

newtype StepId = StepId Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

gen :: (MonadGen m) => m StepId
gen = StepId <$> Gen.text (Range.linear 1 5) Gen.alphaNum

render :: StepId -> Text
render (StepId t) = t
