{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Github.Actions.Types
  ( EnvVariable,
    ObjectKey,
    genObjectKey,
    genObjectKeyMap,
  )
where

import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Refined (Refined)
import Refined qualified
import Relude
import Text.MatchesRegex (MatchesRegex)

type EnvVariable =
  Refined
    ( Refined.And
        Refined.NonEmpty
        (MatchesRegex "^[A-Z0-9]{1}$|^[A-Z0-9]+[A-Z0-9_-]*[A-Z0-9]+$")
    )
    Text

type ObjectKey =
  Refined
    ( Refined.And
        Refined.NonEmpty
        (MatchesRegex "^[a-zA-Z0-9]{1}$|^[a-zA-Z0-9]+[a-zA-Z0-9_-]*[a-zA-Z0-9]+$")
    )
    Text

genObjectKey :: (MonadGen m, MonadFail m) => m ObjectKey
genObjectKey = Refined.refineFail =<< Gen.text (Range.linear 1 10) Gen.alphaNum

genObjectKeyMap :: (MonadGen m, MonadFail m) => m a -> m (Map ObjectKey a)
genObjectKeyMap =
  Gen.map (Range.linear 1 5) . liftA2 (,) genObjectKey
