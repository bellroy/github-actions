{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Language.Github.Actions.Shell
-- Description : GitHub Actions shell configuration for steps
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides the 'Shell' type for specifying which shell to use when
-- running commands in GitHub Actions steps.
--
-- Different shells provide different capabilities and are available on different
-- operating systems. The shell can be specified at the workflow, job, or step level.
--
-- For more information about GitHub Actions shell configuration, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idstepsshell>
module Language.Github.Actions.Shell
  ( Shell (..),
    gen,
  )
where

import Control.Monad.Fail.Hoist (hoistFail')
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Shell types available for running commands in GitHub Actions steps.
--
-- Each shell has different capabilities and platform availability:
--
-- * 'Bash' - Available on all platforms, most commonly used
-- * 'LinuxMacOSOnlySh' - POSIX sh, available on Linux and macOS only
-- * 'Python' - Executes commands as Python code
-- * 'WindowsOnlyCmd' - Windows Command Prompt, Windows only
-- * 'WindowsOnlyPowershell' - Windows PowerShell 5.1, Windows only
-- * 'WindowsOnlyPwsh' - PowerShell Core, available on all platforms
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Shell
--
-- -- Use bash with default options
-- bashShell :: Shell
-- bashShell = Bash Nothing
--
-- -- Use bash with specific options
-- bashWithOptions :: Shell
-- bashWithOptions = Bash (Just "--noprofile --norc")
--
-- -- Use PowerShell Core
-- pwshShell :: Shell
-- pwshShell = WindowsOnlyPwsh Nothing
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idstepsshell>
data Shell
  = -- | Bash shell with optional arguments
    Bash (Maybe Text)
  | -- | POSIX sh shell (Linux/macOS only) with optional arguments
    LinuxMacOSOnlySh (Maybe Text)
  | -- | Python interpreter with optional arguments
    Python (Maybe Text)
  | -- | Windows cmd.exe with optional arguments
    WindowsOnlyCmd (Maybe Text)
  | -- | Windows PowerShell 5.1 with optional arguments
    WindowsOnlyPowershell (Maybe Text)
  | -- | PowerShell Core with optional arguments
    WindowsOnlyPwsh (Maybe Text)
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
