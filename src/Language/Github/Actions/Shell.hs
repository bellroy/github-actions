{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Github.Actions.Shell
  ( Shell (..),
    gen,
  )
where

import Control.Monad.Fail.Hoist (hoistFail')
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.String.Interpolate (i)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Refined (refineEither, unrefine)
import Relude hiding (group)
import Text.NonEmpty (NonEmptyText)
import Text.NonEmpty qualified as NonEmptyText

data Shell
  = Bash (Maybe NonEmptyText)
  | LinuxMacOSOnlySh (Maybe NonEmptyText)
  | Python (Maybe NonEmptyText)
  | WindowsOnlyCmd (Maybe NonEmptyText)
  | WindowsOnlyPowershell (Maybe NonEmptyText)
  | WindowsOnlyPwsh (Maybe NonEmptyText)
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON Shell where
  parseJSON =
    Aeson.withText "Shell" $
      hoistFail' . parseShell

instance ToJSON Shell where
  toJSON = Aeson.String . renderShell

renderShell :: Shell -> Text
renderShell = \case
  Bash args -> [i|bash#{maybe "" ((" " <>) . unrefine) args}|]
  LinuxMacOSOnlySh args -> [i|sh#{maybe "" ((" " <>) . unrefine) args}|]
  Python args -> [i|python#{maybe "" ((" " <>) . unrefine) args}|]
  WindowsOnlyCmd args -> [i|cmd#{maybe "" ((" " <>) . unrefine) args}|]
  WindowsOnlyPowershell args -> [i|powershell#{maybe "" ((" " <>) . unrefine) args}|]
  WindowsOnlyPwsh args -> [i|pwsh#{maybe "" ((" " <>) . unrefine) args}|]

parseShell :: Text -> Either String Shell
parseShell t =
  case tokens of
    "bash" : args -> Bash <$> eitherArgs args
    "cmd" : args -> WindowsOnlyCmd <$> eitherArgs args
    "powershell" : args -> WindowsOnlyPowershell <$> eitherArgs args
    "pwsh" : args -> WindowsOnlyPwsh <$> eitherArgs args
    "python" : args -> Python <$> eitherArgs args
    "sh" : args -> LinuxMacOSOnlySh <$> eitherArgs args
    _ -> Left [i|Unknown shell: #{t}|]
  where
    tokens = words t
    eitherArgs = \case
      [] -> Right Nothing
      args ->
        Just
          <$> first
            (const "Empty arguments given for shell")
            (refineEither (unwords args))

gen :: (MonadGen m) => m Shell
gen =
  Gen.choice
    [ Bash <$> Gen.maybe (NonEmptyText.gen Gen.alphaNum),
      LinuxMacOSOnlySh <$> Gen.maybe (NonEmptyText.gen Gen.alphaNum),
      Python <$> Gen.maybe (NonEmptyText.gen Gen.alphaNum),
      WindowsOnlyCmd <$> Gen.maybe (NonEmptyText.gen Gen.alphaNum),
      WindowsOnlyPowershell <$> Gen.maybe (NonEmptyText.gen Gen.alphaNum),
      WindowsOnlyPwsh <$> Gen.maybe (NonEmptyText.gen Gen.alphaNum)
    ]
