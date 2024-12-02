{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Github.Actions.Step
  ( Step (..),
    gen,
    new,
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
import Relude hiding (id)

data Step = Step
  { continueOnError :: Bool,
    env :: Map Text Text,
    id :: Maybe StepId,
    name :: Maybe Text,
    run :: Maybe Text,
    runIf :: Maybe Text,
    shell :: Maybe Shell,
    timeoutMinutes :: Maybe Int,
    uses :: Maybe Text,
    with :: Maybe StepWith,
    workingDirectory :: Maybe Text
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

gen :: (MonadGen m) => m Step
gen = do
  continueOnError <- Gen.bool
  env <- genTextMap
  id <- Gen.maybe StepId.gen
  name <- Gen.maybe genText
  run <- Gen.maybe genText
  runIf <- Gen.maybe genText
  shell <- Gen.maybe Shell.gen
  timeoutMinutes <- Gen.maybe $ Gen.int (Range.linear 1 120)
  uses <- Gen.maybe genText
  with <- Gen.maybe StepWith.gen
  workingDirectory <- Gen.maybe genText
  pure Step {..}
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
    genTextMap = Gen.map (Range.linear 3 20) $ liftA2 (,) genText genText

new :: Step
new =
  Step
    { continueOnError = False,
      env = mempty,
      id = Nothing,
      name = Nothing,
      run = Nothing,
      runIf = Nothing,
      shell = Nothing,
      timeoutMinutes = Nothing,
      uses = Nothing,
      with = Nothing,
      workingDirectory = Nothing
    }
