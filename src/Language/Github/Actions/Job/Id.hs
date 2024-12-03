{-# LANGUAGE DerivingStrategies #-}

module Language.Github.Actions.Job.Id
  ( JobId (..),
    gen,
    render,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON (..), ToJSONKey)
import Data.Eq (Eq)
import Data.Functor ((<$>))
import Data.Ord (Ord)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Text.Show (Show)

newtype JobId = JobId Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

gen :: (MonadGen m) => m JobId
gen = JobId <$> Gen.text (Range.linear 1 5) Gen.alphaNum

render :: JobId -> Text
render (JobId t) = t
