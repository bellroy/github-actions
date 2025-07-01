{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

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
  ( Platform (..),
    Shell (..),
    arguments,
    gen,
    supportedPlatforms,
  )
where

import Control.Monad.Fail.Hoist (hoistFail')
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty (..))
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
-- * 'Sh' - POSIX sh, available on Linux and macOS only
-- * 'Python' - Executes commands as Python code
-- * 'Cmd' - Windows Command Prompt, Windows only
-- * 'Powershell' - Windows PowerShell 5.1, Windows only
-- * 'Pwsh' - PowerShell Core, available on all platforms
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
-- pwshShell = Pwsh Nothing
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#jobsjob_idstepsshell>
data Shell
  = -- | Bash shell with optional arguments
    Bash (Maybe Text)
  | -- | POSIX sh shell (Linux/macOS only) with optional arguments
    Sh (Maybe Text)
  | -- | Python interpreter with optional arguments
    Python (Maybe Text)
  | -- | Windows cmd.exe with optional arguments
    Cmd (Maybe Text)
  | -- | Windows PowerShell 5.1 with optional arguments
    Powershell (Maybe Text)
  | -- | PowerShell Core with optional arguments
    Pwsh (Maybe Text)
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
  Sh args -> [i|sh#{maybe "" ((" " <>)) args}|]
  Python args -> [i|python#{maybe "" ((" " <>)) args}|]
  Cmd args -> [i|cmd#{maybe "" ((" " <>)) args}|]
  Powershell args -> [i|powershell#{maybe "" ((" " <>)) args}|]
  Pwsh args -> [i|pwsh#{maybe "" ((" " <>)) args}|]

parseShell :: Text -> Either String Shell
parseShell t =
  case tokens of
    "bash" : args -> Right . Bash $ maybeArgs args
    "cmd" : args -> Right . Cmd $ maybeArgs args
    "powershell" : args -> Right . Powershell $ maybeArgs args
    "pwsh" : args -> Right . Pwsh $ maybeArgs args
    "python" : args -> Right . Python $ maybeArgs args
    "sh" : args -> Right . Sh $ maybeArgs args
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
      Sh <$> Gen.maybe genText,
      Python <$> Gen.maybe genText,
      Cmd <$> Gen.maybe genText,
      Powershell <$> Gen.maybe genText,
      Pwsh <$> Gen.maybe genText
    ]
  where
    genText = Gen.text (Range.linear 1 5) Gen.alphaNum

data Platform = Windows | MacOS | Linux

supportedPlatforms :: Shell -> NonEmpty Platform
supportedPlatforms = \case
  Bash _ -> Linux :| [MacOS, Windows]
  Sh _ -> MacOS :| []
  Python _ -> Linux :| [MacOS, Windows]
  Cmd _ -> Windows :| []
  Powershell _ -> Windows :| []
  Pwsh _ -> Windows :| []

-- | A lens into the arguments for a shell, compatible with the "lens" package
--
-- @
-- arguments :: Lens' Shell (Maybe Text)
-- @
arguments :: forall f. (Functor f) => (Maybe Text -> f (Maybe Text)) -> Shell -> f Shell
arguments f = \case
  Bash args -> Bash <$> f args
  Sh args -> Sh <$> f args
  Python args -> Python <$> f args
  Cmd args -> Cmd <$> f args
  Powershell args -> Powershell <$> f args
  Pwsh args -> Pwsh <$> f args
