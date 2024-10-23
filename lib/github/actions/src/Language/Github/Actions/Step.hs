{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Step
  ( Step (..),
    gen,
  )
where

import Data.Aeson (FromJSON, ToJSON (..), (.!=), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.Github.Actions.Shell (Shell)
import Language.Github.Actions.Shell qualified as Shell
import Language.Github.Actions.Step.Id (StepId)
import Language.Github.Actions.Step.Id qualified as StepId
import Language.Github.Actions.Step.With (StepWith)
import Language.Github.Actions.Step.With qualified as StepWith
import Language.Github.Actions.Types (ObjectKey, genObjectKeyMap)
import Relude hiding (id)
import Text.NonEmpty (NonEmptyText)
import Text.NonEmpty qualified as NonEmptyText

data Step = Step
  { continueOnError :: Bool,
    env :: Map ObjectKey Text,
    id :: Maybe StepId,
    name :: Maybe NonEmptyText,
    run :: Maybe NonEmptyText,
    runIf :: Maybe NonEmptyText,
    shell :: Maybe Shell,
    timeoutMinutes :: Maybe Int,
    uses :: Maybe NonEmptyText,
    with :: Maybe StepWith,
    workingDirectory :: Maybe NonEmptyText
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON Step where
  parseJSON = Aeson.withObject "Step" $ \o -> do
    continueOnError <- o .:? "continue-on-error" .!= False
    env <- o .:? "env" .!= mempty
    id <- o .:? "id"
    name <- o .:? "name"
    run <- o .:? "run"
    runIf <- o .:? "if"
    uses <- o .:? "uses"
    shell <- o .:? "shell"
    timeoutMinutes <- o .:? "timeout-minutes"
    with <- o .:? "with"
    workingDirectory <- o .:? "working-directory"
    pure Step {..}

instance ToJSON Step where
  toJSON Step {..} =
    Aeson.object $
      catMaybes
        [ if continueOnError then Just ("continue-on-error" .= True) else Nothing,
          ("env" .=) <$> monoidToMaybe env,
          ("id" .=) <$> id,
          ("if" .=) <$> runIf,
          ("name" .=) <$> name,
          ("run" .=) <$> run,
          ("shell" .=) <$> shell,
          ("timeout-minutes" .=) <$> timeoutMinutes,
          ("uses" .=) <$> uses,
          ("with" .=) <$> with,
          ("working-directory" .=) <$> workingDirectory
        ]
    where
      monoidToMaybe :: (Eq a, Monoid a) => a -> Maybe a
      monoidToMaybe a = if a == mempty then Nothing else Just a

gen :: (MonadGen m, MonadFail m) => m Step
gen = do
  continueOnError <- Gen.bool
  env <- genObjectKeyMap (Gen.text (Range.linear 3 20) Gen.alphaNum)
  id <- Gen.maybe StepId.gen
  name <- Gen.maybe $ NonEmptyText.gen Gen.alphaNum
  run <- Gen.maybe $ NonEmptyText.gen Gen.alphaNum
  runIf <- Gen.maybe $ NonEmptyText.gen Gen.alphaNum
  shell <- Gen.maybe Shell.gen
  timeoutMinutes <- Gen.maybe $ Gen.int (Range.linear 1 120)
  uses <- Gen.maybe $ NonEmptyText.gen Gen.alphaNum
  with <- Gen.maybe StepWith.gen
  workingDirectory <- Gen.maybe $ NonEmptyText.gen Gen.alphaNum
  pure Step {..}
