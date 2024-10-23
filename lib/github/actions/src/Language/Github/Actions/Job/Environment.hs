{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Github.Actions.Job.Environment
  ( JobEnvironment (..),
    gen,
  )
where

import Control.Monad.Fail.Hoist (hoistFail')
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.Github.Actions.Types (ObjectKey, genObjectKeyMap)
import Refined (unrefine)
import Relude
import Text.NonEmpty (NonEmptyText)
import Text.NonEmpty qualified as NonEmptyText

data JobEnvironment
  = NamedJobEnvironment NonEmptyText
  | CustomJobEnvironment (Map ObjectKey Text)
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON JobEnvironment where
  parseJSON v = case v of
    Aeson.String s ->
      hoistFail' $
        NamedJobEnvironment
          <$> maybeToRight
            "Empty named job environment"
            (NonEmptyText.maybeFromText s)
    Aeson.Object o -> CustomJobEnvironment <$> Aeson.parseJSON (Aeson.Object o)
    _ -> fail "JobEnvironment must be a string or an object"

instance ToJSON JobEnvironment where
  toJSON (NamedJobEnvironment s) = Aeson.String $ unrefine s
  toJSON (CustomJobEnvironment m) = Aeson.toJSON m

gen :: (MonadGen m, MonadFail m) => m JobEnvironment
gen =
  Gen.choice
    [ NamedJobEnvironment <$> NonEmptyText.gen Gen.alphaNum,
      fmap CustomJobEnvironment
        . genObjectKeyMap
        $ Gen.text (Range.linear 3 20) Gen.alphaNum
    ]
