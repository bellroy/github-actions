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
import Data.Either (Either (..))
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..), maybe)
import Data.Monoid ((<>))
import Data.Ord (Ord)
import Data.String (String)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Text.Show (Show)

data Shell
  = Bash (Maybe Text)
  | LinuxMacOSOnlySh (Maybe Text)
  | Python (Maybe Text)
  | WindowsOnlyCmd (Maybe Text)
  | WindowsOnlyPowershell (Maybe Text)
  | WindowsOnlyPwsh (Maybe Text)
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON Shell where
  parseJSON =
    Aeson.withText "Shell" $
      hoistFail' . parseShell

instance ToJSON Shell where
  toJSON = Aeson.String . renderShell

renderShell :: Shell -> Text
renderShell = \case
  Bash args -> [i|bash#{maybe "" ((" " <>)) args}|]
  LinuxMacOSOnlySh args -> [i|sh#{maybe "" ((" " <>)) args}|]
  Python args -> [i|python#{maybe "" ((" " <>)) args}|]
  WindowsOnlyCmd args -> [i|cmd#{maybe "" ((" " <>)) args}|]
  WindowsOnlyPowershell args -> [i|powershell#{maybe "" ((" " <>)) args}|]
  WindowsOnlyPwsh args -> [i|pwsh#{maybe "" ((" " <>)) args}|]

parseShell :: Text -> Either String Shell
parseShell t =
  case tokens of
    "bash" : args -> Right . Bash $ maybeArgs args
    "cmd" : args -> Right . WindowsOnlyCmd $ maybeArgs args
    "powershell" : args -> Right . WindowsOnlyPowershell $ maybeArgs args
    "pwsh" : args -> Right . WindowsOnlyPwsh $ maybeArgs args
    "python" : args -> Right . Python $ maybeArgs args
    "sh" : args -> Right . LinuxMacOSOnlySh $ maybeArgs args
    _ -> Left [i|Unknown shell: #{t}|]
  where
    tokens = Text.words t
    maybeArgs = \case
      [] -> Nothing
      args -> Just $ Text.unwords args

gen :: (MonadGen m) => m Shell
gen =
  Gen.choice
    [ Bash <$> Gen.maybe genText,
      LinuxMacOSOnlySh <$> Gen.maybe genText,
      Python <$> Gen.maybe genText,
      WindowsOnlyCmd <$> Gen.maybe genText,
      WindowsOnlyPowershell <$> Gen.maybe genText,
      WindowsOnlyPwsh <$> Gen.maybe genText
    ]
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum
