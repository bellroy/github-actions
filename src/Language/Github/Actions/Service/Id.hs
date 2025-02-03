{-# LANGUAGE DerivingStrategies #-}

module Language.Github.Actions.Service.Id
  ( ServiceId (..),
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

newtype ServiceId = ServiceId Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

gen :: (MonadGen m) => m ServiceId
gen = ServiceId <$> Gen.text (Range.linear 1 5) Gen.alphaNum

render :: ServiceId -> Text
render (ServiceId t) = t
