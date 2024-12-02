{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Github.Actions.Permissions
  ( Permissions (..),
    gen,
  )
where

import Control.Monad.Fail.Hoist (hoistFail')
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.String.Interpolate (i)
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Relude

data PermissionType
  = Actions
  | Attestations
  | Checks
  | Contents
  | Deployments
  | IdToken
  | Issues
  | Discussions
  | Packages
  | Pages
  | PullRequests
  | RepositoryProjects
  | SecurityEvents
  | Statuses
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
  maybe (fail [i|Unknown PermissionType: #{t}|]) pure $
    inverseMap renderPermissionType t

data Permission
  = None
  | Read
  | Write
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
  maybe (fail [i|Unknown Permission: #{t}|]) pure $
    inverseMap renderPermission t

data Permissions
  = NoPermissions
  | ReadAll
  | WriteAll
  | Custom (Map PermissionType Permission)
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
