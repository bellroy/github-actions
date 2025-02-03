{-# LANGUAGE DerivingStrategies #-}

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
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

newtype StepId = StepId Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

gen :: (MonadGen m) => m StepId
gen = StepId <$> Gen.text (Range.linear 1 5) Gen.alphaNum

render :: StepId -> Text
render (StepId t) = t
