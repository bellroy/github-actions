{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Language.Github.Actions.Permissions
-- Description : GitHub Actions permissions and access control
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides types for managing GitHub Actions permissions and access control.
-- Permissions control what GitHub APIs and resources workflows and jobs can access.
--
-- You can set permissions at the workflow level (affecting all jobs) or at individual
-- job levels. This follows the principle of least privilege by allowing you to grant
-- only the specific permissions needed.
--
-- For more information about GitHub Actions permissions, see:
-- <https://docs.github.com/en/actions/writing-workflows/workflow-syntax-for-github-actions#permissions>
module Language.Github.Actions.Permissions
  ( Permissions (..),
    PermissionType (..),
    Permission (..),
    gen,
  )
where

import Control.Monad.Fail.Hoist (hoistFail')
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Map (Map)
import Data.String.Interpolate (i)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Language.Github.Actions.Internal (inverseMap)

-- | Types of permissions that can be granted to GitHub Actions workflows.
--
-- Each permission type corresponds to a specific area of GitHub functionality
-- that workflows might need to access.
--
-- For more details about each permission type, see: <https://docs.github.com/en/actions/security-guides/automatic-token-authentication#permissions-for-the-github_token>
data PermissionType
  = -- | Manage GitHub Actions (e.g., cancel workflow runs)
    Actions
  | -- | Create and verify attestations
    Attestations
  | -- | Create and update check runs and suites
    Checks
  | -- | Read and write repository contents
    Contents
  | -- | Create and manage deployments
    Deployments
  | -- | Request OIDC JWT ID tokens
    IdToken
  | -- | Create and manage issues
    Issues
  | -- | Create and manage discussions
    Discussions
  | -- | Publish and manage packages
    Packages
  | -- | Deploy to GitHub Pages
    Pages
  | -- | Create and manage pull requests
    PullRequests
  | -- | Manage repository projects
    RepositoryProjects
  | -- | Read and write security events
    SecurityEvents
  | -- | Create commit status checks
    Statuses
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON PermissionType where
  parseJSON =
    Aeson.withText "PermissionType" $
      hoistFail' . parsePermissionType

instance FromJSONKey PermissionType where
  fromJSONKey = Aeson.FromJSONKeyTextParser (Aeson.parseJSON . Aeson.String)

instance ToJSON PermissionType where
  toJSON = Aeson.String . renderPermissionType

instance ToJSONKey PermissionType where
  toJSONKey = Aeson.toJSONKeyText renderPermissionType

renderPermissionType :: PermissionType -> Text
renderPermissionType = \case
  Actions -> "actions"
  Attestations -> "attestations"
  Checks -> "checks"
  Contents -> "contents"
  Deployments -> "deployments"
  IdToken -> "id-token"
  Issues -> "issues"
  Discussions -> "discussions"
  Packages -> "packages"
  Pages -> "pages"
  PullRequests -> "pull-requests"
  RepositoryProjects -> "repository-projects"
  SecurityEvents -> "security-events"
  Statuses -> "statuses"

parsePermissionType :: Text -> Either String PermissionType
parsePermissionType t =
  maybe (Left [i|Unknown PermissionType: #{t}|]) Right $
    inverseMap renderPermissionType t

-- | Permission levels that can be granted for each permission type.
data Permission
  = -- | No access granted
    None
  | -- | Read-only access
    Read
  | -- | Read and write access
    Write
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON Permission where
  parseJSON =
    Aeson.withText "Permission" $
      hoistFail' . parsePermission

instance ToJSON Permission where
  toJSON = Aeson.String . renderPermission

renderPermission :: Permission -> Text
renderPermission = \case
  None -> "none"
  Read -> "read"
  Write -> "write"

parsePermission :: Text -> Either String Permission
parsePermission t =
  maybe (Left [i|Unknown Permission: #{t}|]) Right $
    inverseMap renderPermission t

-- | Overall permissions configuration for a workflow or job.
--
-- Permissions can be set globally (affecting all permission types) or
-- individually for specific permission types.
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Permissions
--
-- -- Grant read access to everything
-- readOnlyPerms :: Permissions
-- readOnlyPerms = ReadAll
--
-- -- Grant specific permissions only
-- customPerms :: Permissions
-- customPerms = Custom $ Map.fromList
--  [ (Contents, Read)
--  , (PullRequests, Write)
--  ]
-- @
data Permissions
  = -- | No permissions granted (empty object)
    NoPermissions
  | -- | Read access to all permission types
    ReadAll
  | -- | Write access to all permission types
    WriteAll
  | -- | Custom permission mapping
    Custom (Map PermissionType Permission)
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON Permissions where
  parseJSON v = case v of
    Aeson.String "{}" -> pure NoPermissions
    Aeson.String "read-all" -> pure ReadAll
    Aeson.String "write-all" -> pure WriteAll
    Aeson.String s -> fail [i|Invalid Permissions: #{s}|]
    Aeson.Object o -> Custom <$> Aeson.parseJSON (Aeson.Object o)
    _ -> fail "Permissions must be a string or an object"

instance ToJSON Permissions where
  toJSON = \case
    NoPermissions -> Aeson.String "{}"
    ReadAll -> Aeson.String "read-all"
    WriteAll -> Aeson.String "write-all"
    Custom m -> Aeson.toJSON m

gen :: (MonadGen m) => m Permissions
gen =
  Gen.choice
    [ pure NoPermissions,
      pure ReadAll,
      pure WriteAll,
      fmap Custom . Gen.map (Range.linear 1 5) $
        liftA2 (,) Gen.enumBounded Gen.enumBounded
    ]
