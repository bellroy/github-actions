{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Language.Github.Actions.Workflow.Trigger
-- Description : GitHub Actions workflow trigger events and configurations
-- Copyright   : (c) 2025 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
--
-- This module provides comprehensive support for GitHub Actions workflow triggers,
-- which determine when workflows should run.
--
-- Workflow triggers respond to various GitHub events such as pushes, pull requests,
-- issue updates, scheduled times, and external repository dispatch events. Each trigger
-- type can be configured with specific activity types and filtering criteria.
--
-- The main types include:
--
-- * 'WorkflowTrigger' - The primary trigger type supporting all GitHub event types
-- * Activity type enums for each event (e.g., 'PullRequestActivityType', 'IssuesActivityType')
-- * Attribute types for triggers with complex configuration (e.g., 'PushTriggerAttributes')
--
-- Example usage:
--
-- @
-- import Language.Github.Actions.Workflow.Trigger
--
-- -- Trigger on push to main branch
-- pushTrigger :: WorkflowTrigger
-- pushTrigger = PushTrigger $ PushTriggerAttributes
--   { pushBranches = Just ("main" :| [])
--   , pushBranchesIgnore = Nothing
--   , pushPaths = Nothing
--   , pushPathsIgnore = Nothing
--   , pushTags = Nothing
--   }
--
-- -- Trigger on pull request opened or synchronized
-- prTrigger :: WorkflowTrigger
-- prTrigger = PullRequestTrigger $ PullRequestTriggerAttributes
--   { pullRequestActivityTypes = Just (PullRequestOpened :| [PullRequestSynchronize])
--   , pullRequestBranches = Nothing
--   , pullRequestBranchesIgnore = Nothing
--   , pullRequestPaths = Nothing
--   , pullRequestPathsIgnore = Nothing
--   }
-- @
--
-- For more information about GitHub Actions events and triggers, see:
-- <https://docs.github.com/en/actions/writing-workflows/choosing-when-your-workflow-runs/events-that-trigger-workflows>
module Language.Github.Actions.Workflow.Trigger
  ( BranchProtectionRuleActivityType (..),
    CheckRunActivityType (..),
    DiscussionActivityType (..),
    DiscussionCommentActivityType (..),
    IssueCommentActivityType (..),
    IssuesActivityType (..),
    LabelActivityType (..),
    MilestoneActivityType (..),
    PullRequestActivityType (..),
    PullRequestReviewActivityType (..),
    PullRequestReviewCommentActivityType (..),
    PullRequestTargetActivityType (..),
    PullRequestTargetTriggerAttributes (..),
    PullRequestTriggerAttributes (..),
    PushTriggerAttributes (..),
    RegistryPackageActivityType (..),
    ReleaseActivityType (..),
    WorkflowCallAttributes (..),
    WorkflowDispatchAttributes (..),
    WorkflowRunActivityType (..),
    WorkflowRunTriggerAttributes (..),
    WorkflowTrigger (..),
    gen,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Fail.Hoist (hoistFail')
import Data.Aeson (FromJSON, ToJSON, ToJSONKey, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Types as Aeson
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Traversable (for)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Language.Github.Actions.Internal (inverseMap)

data BranchProtectionRuleActivityType
  = BranchProtectionRuleCreated
  | BranchProtectionRuleDeleted
  | BranchProtectionRuleEdited
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON BranchProtectionRuleActivityType where
  parseJSON =
    Aeson.withText "BranchProtectionRuleActivityType" $
      hoistFail' . parseBranchProtectionRuleActivityType

instance ToJSON BranchProtectionRuleActivityType where
  toJSON = Aeson.String . renderBranchProtectionRuleActivityType

renderBranchProtectionRuleActivityType :: BranchProtectionRuleActivityType -> Text
renderBranchProtectionRuleActivityType = \case
  BranchProtectionRuleCreated -> "created"
  BranchProtectionRuleDeleted -> "deleted"
  BranchProtectionRuleEdited -> "edited"

parseBranchProtectionRuleActivityType :: Text -> Either String BranchProtectionRuleActivityType
parseBranchProtectionRuleActivityType t =
  maybe (Left [i|Unknown BranchProtectionRuleActivityType: #{t}|]) Right $
    inverseMap renderBranchProtectionRuleActivityType t

data CheckRunActivityType
  = CheckRunCompleted
  | CheckRunCreated
  | CheckRunRequestedAction
  | CheckRunRerequested
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON CheckRunActivityType where
  parseJSON =
    Aeson.withText "CheckRunActivityType" $
      hoistFail' . parseCheckRunActivityType

instance ToJSON CheckRunActivityType where
  toJSON = Aeson.String . renderCheckRunActivityType

renderCheckRunActivityType :: CheckRunActivityType -> Text
renderCheckRunActivityType = \case
  CheckRunCompleted -> "completed"
  CheckRunCreated -> "created"
  CheckRunRequestedAction -> "requested_action"
  CheckRunRerequested -> "rerequested"

parseCheckRunActivityType :: Text -> Either String CheckRunActivityType
parseCheckRunActivityType t =
  maybe (Left [i|Unknown CheckRunActivityType: #{t}|]) Right $
    inverseMap renderCheckRunActivityType t

data DiscussionActivityType
  = DiscussionAnswered
  | DiscussionCategoryChanged
  | DiscussionCreated
  | DiscussionDeleted
  | DiscussionEdited
  | DiscussionLabeled
  | DiscussionLocked
  | DiscussionPinned
  | DiscussionTransferred
  | DiscussionUnanswered
  | DiscussionUnlabeled
  | DiscussionUnlocked
  | DiscussionUnpinned
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON DiscussionActivityType where
  parseJSON =
    Aeson.withText "DiscussionActivityType" $
      hoistFail' . parseDiscussionActivityType

instance ToJSON DiscussionActivityType where
  toJSON = Aeson.String . renderDiscussionActivityType

renderDiscussionActivityType :: DiscussionActivityType -> Text
renderDiscussionActivityType = \case
  DiscussionAnswered -> "answered"
  DiscussionCategoryChanged -> "category_changed"
  DiscussionCreated -> "created"
  DiscussionDeleted -> "deleted"
  DiscussionEdited -> "edited"
  DiscussionLabeled -> "labeled"
  DiscussionLocked -> "locked"
  DiscussionPinned -> "pinned"
  DiscussionTransferred -> "transferred"
  DiscussionUnanswered -> "unanswered"
  DiscussionUnlabeled -> "unlabeled"
  DiscussionUnlocked -> "unlocked"
  DiscussionUnpinned -> "unpinned"

parseDiscussionActivityType :: Text -> Either String DiscussionActivityType
parseDiscussionActivityType t =
  maybe (Left [i|Unknown DiscussionActivityType: #{t}|]) Right $
    inverseMap renderDiscussionActivityType t

data DiscussionCommentActivityType
  = DiscussionCommentCreated
  | DiscussionCommentDeleted
  | DiscussionCommentEdited
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON DiscussionCommentActivityType where
  parseJSON =
    Aeson.withText "DiscussionCommentActivityType" $
      hoistFail' . parseDiscussionCommentActivityType

instance ToJSON DiscussionCommentActivityType where
  toJSON = Aeson.String . renderDiscussionCommentActivityType

renderDiscussionCommentActivityType :: DiscussionCommentActivityType -> Text
renderDiscussionCommentActivityType = \case
  DiscussionCommentCreated -> "created"
  DiscussionCommentDeleted -> "deleted"
  DiscussionCommentEdited -> "edited"

parseDiscussionCommentActivityType :: Text -> Either String DiscussionCommentActivityType
parseDiscussionCommentActivityType t =
  maybe (Left [i|Unknown DiscussionCommentActivityType: #{t}|]) Right $
    inverseMap renderDiscussionCommentActivityType t

data IssueCommentActivityType
  = IssueCommentCreated
  | IssueCommentDeleted
  | IssueCommentEdited
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON IssueCommentActivityType where
  parseJSON =
    Aeson.withText "IssueCommentActivityType" $
      hoistFail' . parseIssueCommentActivityType

instance ToJSON IssueCommentActivityType where
  toJSON = Aeson.String . renderIssueCommentActivityType

renderIssueCommentActivityType :: IssueCommentActivityType -> Text
renderIssueCommentActivityType = \case
  IssueCommentCreated -> "created"
  IssueCommentDeleted -> "deleted"
  IssueCommentEdited -> "edited"

parseIssueCommentActivityType :: Text -> Either String IssueCommentActivityType
parseIssueCommentActivityType t =
  maybe (Left [i|Unknown IssueCommentActivityType: #{t}|]) Right $
    inverseMap renderIssueCommentActivityType t

data IssuesActivityType
  = IssuesAssigned
  | IssuesClosed
  | IssuesDeleted
  | IssuesDemilestoned
  | IssuesEdited
  | IssuesLabeled
  | IssuesLocked
  | IssuesMilestoned
  | IssuesOpened
  | IssuesPinned
  | IssuesReopened
  | IssuesTransferred
  | IssuesUnassigned
  | IssuesUnlabeled
  | IssuesUnlocked
  | IssuesUnpinned
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON IssuesActivityType where
  parseJSON =
    Aeson.withText "IssuesActivityType" $
      hoistFail' . parseIssuesActivityType

instance ToJSON IssuesActivityType where
  toJSON = Aeson.String . renderIssuesActivityType

renderIssuesActivityType :: IssuesActivityType -> Text
renderIssuesActivityType = \case
  IssuesAssigned -> "assigned"
  IssuesClosed -> "closed"
  IssuesDeleted -> "deleted"
  IssuesDemilestoned -> "demilestoned"
  IssuesEdited -> "edited"
  IssuesLabeled -> "labeled"
  IssuesLocked -> "locked"
  IssuesMilestoned -> "milestoned"
  IssuesOpened -> "opened"
  IssuesPinned -> "pinned"
  IssuesReopened -> "reopened"
  IssuesTransferred -> "transferred"
  IssuesUnassigned -> "unassigned"
  IssuesUnlabeled -> "unlabeled"
  IssuesUnlocked -> "unlocked"
  IssuesUnpinned -> "unpinned"

parseIssuesActivityType :: Text -> Either String IssuesActivityType
parseIssuesActivityType t =
  maybe (Left [i|Unknown IssuesActivityType: #{t}|]) Right $
    inverseMap renderIssuesActivityType t

data LabelActivityType
  = LabelCreated
  | LabelDeleted
  | LabelEdited
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON LabelActivityType where
  parseJSON =
    Aeson.withText "LabelActivityType" $
      hoistFail' . parseLabelActivityType

instance ToJSON LabelActivityType where
  toJSON = Aeson.String . renderLabelActivityType

renderLabelActivityType :: LabelActivityType -> Text
renderLabelActivityType = \case
  LabelCreated -> "created"
  LabelDeleted -> "deleted"
  LabelEdited -> "edited"

parseLabelActivityType :: Text -> Either String LabelActivityType
parseLabelActivityType t =
  maybe (Left [i|Unknown LabelActivityType: #{t}|]) Right $
    inverseMap renderLabelActivityType t

data MilestoneActivityType
  = MilestoneCreated
  | MilestoneClosed
  | MilestoneDeleted
  | MilestoneEdited
  | MilestoneOpened
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON MilestoneActivityType where
  parseJSON =
    Aeson.withText "MilestoneActivityType" $
      hoistFail' . parseMilestoneActivityType

instance ToJSON MilestoneActivityType where
  toJSON = Aeson.String . renderMilestoneActivityType

renderMilestoneActivityType :: MilestoneActivityType -> Text
renderMilestoneActivityType = \case
  MilestoneCreated -> "created"
  MilestoneClosed -> "closed"
  MilestoneDeleted -> "deleted"
  MilestoneEdited -> "edited"
  MilestoneOpened -> "opened"

parseMilestoneActivityType :: Text -> Either String MilestoneActivityType
parseMilestoneActivityType t =
  maybe (Left [i|Unknown MilestoneActivityType: #{t}|]) Right $
    inverseMap renderMilestoneActivityType t

-- | Activity types for pull request trigger events.
--
-- These specify which pull request activities should trigger the workflow.
-- Common combinations include opened/synchronized for CI workflows, or
-- closed for deployment workflows.
--
-- Example usage:
--
-- @
-- -- Trigger on pull request creation and updates
-- ciTrigger :: PullRequestActivityType
-- ciTrigger = PullRequestTrigger $ PullRequestTriggerAttributes
--  { pullRequestActivityTypes = Just (PullRequestOpened :| [PullRequestSynchronize])
--  , pullRequestBranches = Nothing
--  , pullRequestBranchesIgnore = Nothing
--  , pullRequestPaths = Nothing
--  , pullRequestPathsIgnore = Nothing
--  }
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/choosing-when-your-workflow-runs/events-that-trigger-workflows#pull_request>
data PullRequestActivityType
  = -- | Pull request was assigned to someone
    PullRequestAssigned
  | -- | Auto-merge was disabled
    PullRequestAutoMergeDisabled
  | -- | Auto-merge was enabled
    PullRequestAutoMergeEnabled
  | -- | Pull request was closed
    PullRequestClosed
  | -- | Pull request was converted to draft
    PullRequestConvertedToDraft
  | -- | Milestone was removed from pull request
    PullRequestDemilestoned
  | -- | Pull request was removed from merge queue
    PullRequestDequeued
  | -- | Pull request title or body was edited
    PullRequestEdited
  | -- | Pull request was added to merge queue
    PullRequestEnqueued
  | -- | Label was added to pull request
    PullRequestLabeled
  | -- | Pull request conversation was locked
    PullRequestLocked
  | -- | Milestone was added to pull request
    PullRequestMilestoned
  | -- | Pull request was opened
    PullRequestOpened
  | -- | Draft pull request was marked ready for review
    PullRequestReadyForReview
  | -- | Pull request was reopened
    PullRequestReopened
  | -- | Review request was removed
    PullRequestReviewRequestRemoved
  | -- | Review was requested
    PullRequestReviewRequested
  | -- | Pull request's head branch was updated
    PullRequestSynchronize
  | -- | Pull request was unassigned
    PullRequestUnassigned
  | -- | Label was removed from pull request
    PullRequestUnlabeled
  | -- | Pull request conversation was unlocked
    PullRequestUnlocked
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON PullRequestActivityType where
  parseJSON =
    Aeson.withText "PullRequestActivityType" $
      hoistFail' . parsePullRequestActivityType

instance ToJSON PullRequestActivityType where
  toJSON = Aeson.String . renderPullRequestActivityType

renderPullRequestActivityType :: PullRequestActivityType -> Text
renderPullRequestActivityType = \case
  PullRequestAssigned -> "assigned"
  PullRequestAutoMergeDisabled -> "auto_merge_disabled"
  PullRequestAutoMergeEnabled -> "auto_merge_enabled"
  PullRequestClosed -> "closed"
  PullRequestConvertedToDraft -> "converted_to_draft"
  PullRequestDemilestoned -> "demilestoned"
  PullRequestDequeued -> "dequeued"
  PullRequestEdited -> "edited"
  PullRequestEnqueued -> "enqueued"
  PullRequestLabeled -> "labeled"
  PullRequestLocked -> "locked"
  PullRequestMilestoned -> "milestoned"
  PullRequestOpened -> "opened"
  PullRequestReadyForReview -> "ready_for_review"
  PullRequestReopened -> "reopened"
  PullRequestReviewRequestRemoved -> "review_request_removed"
  PullRequestReviewRequested -> "review_requested"
  PullRequestSynchronize -> "synchronize"
  PullRequestUnassigned -> "unassigned"
  PullRequestUnlabeled -> "unlabeled"
  PullRequestUnlocked -> "unlocked"

parsePullRequestActivityType :: Text -> Either String PullRequestActivityType
parsePullRequestActivityType t =
  maybe (Left [i|Unknown PullRequestActivityType: #{t}|]) Right $
    inverseMap renderPullRequestActivityType t

data PullRequestTriggerAttributes = PullRequestTriggerAttributes
  { pullRequestActivityTypes :: Maybe (NonEmpty PullRequestActivityType),
    pullRequestBranches :: Maybe (NonEmpty Text),
    pullRequestBranchesIgnore :: Maybe (NonEmpty Text),
    pullRequestPaths :: Maybe (NonEmpty Text),
    pullRequestPathsIgnore :: Maybe (NonEmpty Text)
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON PullRequestTriggerAttributes where
  parseJSON = Aeson.withObject "PullRequestTriggerAttributes" $ \o -> do
    pullRequestActivityTypes <- o .:? "types"
    pullRequestBranches <- o .:? "branches"
    pullRequestBranchesIgnore <- o .:? "branches-ignore"
    pullRequestPaths <- o .:? "paths"
    pullRequestPathsIgnore <- o .:? "paths-ignore"
    pure PullRequestTriggerAttributes {..}

instance ToJSON PullRequestTriggerAttributes where
  toJSON PullRequestTriggerAttributes {..} =
    Aeson.object $
      catMaybes
        [ ("branches" .=) <$> pullRequestBranches,
          ("branches-ignore" .=) <$> pullRequestBranchesIgnore,
          ("paths" .=) <$> pullRequestPaths,
          ("paths-ignore" .=) <$> pullRequestPathsIgnore,
          ("types" .=) <$> pullRequestActivityTypes
        ]

data PullRequestReviewActivityType
  = PullRequestReviewDismissed
  | PullRequestReviewEdited
  | PullRequestReviewSubmitted
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON PullRequestReviewActivityType where
  parseJSON =
    Aeson.withText "PullRequestReviewActivityType" $
      hoistFail' . parsePullRequestReviewActivityType

instance ToJSON PullRequestReviewActivityType where
  toJSON = Aeson.String . renderPullRequestReviewActivityType

renderPullRequestReviewActivityType :: PullRequestReviewActivityType -> Text
renderPullRequestReviewActivityType = \case
  PullRequestReviewDismissed -> "dismissed"
  PullRequestReviewEdited -> "edited"
  PullRequestReviewSubmitted -> "submitted"

parsePullRequestReviewActivityType :: Text -> Either String PullRequestReviewActivityType
parsePullRequestReviewActivityType t =
  maybe (Left [i|Unknown PullRequestReviewActivityType: #{t}|]) Right $
    inverseMap renderPullRequestReviewActivityType t

data PullRequestReviewCommentActivityType
  = PullRequestReviewCommentCreated
  | PullRequestReviewCommentDeleted
  | PullRequestReviewCommentEdited
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON PullRequestReviewCommentActivityType where
  parseJSON =
    Aeson.withText "PullRequestReviewCommentActivityType" $
      hoistFail' . parsePullRequestReviewCommentActivityType

instance ToJSON PullRequestReviewCommentActivityType where
  toJSON = Aeson.String . renderPullRequestReviewCommentActivityType

renderPullRequestReviewCommentActivityType :: PullRequestReviewCommentActivityType -> Text
renderPullRequestReviewCommentActivityType = \case
  PullRequestReviewCommentCreated -> "created"
  PullRequestReviewCommentDeleted -> "deleted"
  PullRequestReviewCommentEdited -> "edited"

parsePullRequestReviewCommentActivityType :: Text -> Either String PullRequestReviewCommentActivityType
parsePullRequestReviewCommentActivityType t =
  maybe (Left [i|Unknown PullRequestReviewCommentActivityType: #{t}|]) Right $
    inverseMap renderPullRequestReviewCommentActivityType t

data PullRequestTargetActivityType
  = PullRequestTargetAssigned
  | PullRequestTargetAutoMergeDisabled
  | PullRequestTargetAutoMergeEnabled
  | PullRequestTargetClosed
  | PullRequestTargetConvertedToDraft
  | PullRequestTargetEdited
  | PullRequestTargetLabeled
  | PullRequestTargetLocked
  | PullRequestTargetOpened
  | PullRequestTargetReadyForReview
  | PullRequestTargetReopened
  | PullRequestTargetReviewRequestRemoved
  | PullRequestTargetReviewRequested
  | PullRequestTargetSynchronize
  | PullRequestTargetUnassigned
  | PullRequestTargetUnlabeled
  | PullRequestTargetUnlocked
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON PullRequestTargetActivityType where
  parseJSON =
    Aeson.withText "PullRequestTargetActivityType" $
      hoistFail' . parsePullRequestTargetActivityType

instance ToJSON PullRequestTargetActivityType where
  toJSON = Aeson.String . renderPullRequestTargetActivityType

renderPullRequestTargetActivityType :: PullRequestTargetActivityType -> Text
renderPullRequestTargetActivityType = \case
  PullRequestTargetAssigned -> "assigned"
  PullRequestTargetAutoMergeDisabled -> "auto_merge_disabled"
  PullRequestTargetAutoMergeEnabled -> "auto_merge_enabled"
  PullRequestTargetClosed -> "closed"
  PullRequestTargetConvertedToDraft -> "converted_to_draft"
  PullRequestTargetEdited -> "edited"
  PullRequestTargetLabeled -> "labeled"
  PullRequestTargetLocked -> "locked"
  PullRequestTargetOpened -> "opened"
  PullRequestTargetReadyForReview -> "ready_for_review"
  PullRequestTargetReopened -> "reopened"
  PullRequestTargetReviewRequestRemoved -> "review_request_removed"
  PullRequestTargetReviewRequested -> "review_requested"
  PullRequestTargetSynchronize -> "synchronize"
  PullRequestTargetUnassigned -> "unassigned"
  PullRequestTargetUnlabeled -> "unlabeled"
  PullRequestTargetUnlocked -> "unlocked"

parsePullRequestTargetActivityType :: Text -> Either String PullRequestTargetActivityType
parsePullRequestTargetActivityType t =
  maybe (Left [i|Unknown PullRequestTargetActivityType: #{t}|]) Right $
    inverseMap renderPullRequestTargetActivityType t

data PullRequestTargetTriggerAttributes = PullRequestTargetTriggerAttributes
  { pullRequestTargetActivityTypes :: Maybe (NonEmpty PullRequestTargetActivityType),
    pullRequestTargetBranches :: Maybe (NonEmpty Text),
    pullRequestTargetBranchesIgnore :: Maybe (NonEmpty Text),
    pullRequestTargetPaths :: Maybe (NonEmpty Text),
    pullRequestTargetPathsIgnore :: Maybe (NonEmpty Text)
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON PullRequestTargetTriggerAttributes where
  parseJSON = Aeson.withObject "PullRequestTargetTriggerAttributes" $ \o -> do
    pullRequestTargetActivityTypes <- o .:? "types"
    pullRequestTargetBranches <- o .:? "branches"
    pullRequestTargetBranchesIgnore <- o .:? "branches-ignore"
    pullRequestTargetPaths <- o .:? "paths"
    pullRequestTargetPathsIgnore <- o .:? "paths-ignore"
    pure PullRequestTargetTriggerAttributes {..}

instance ToJSON PullRequestTargetTriggerAttributes where
  toJSON PullRequestTargetTriggerAttributes {..} =
    Aeson.object $
      catMaybes
        [ ("branches" .=) <$> pullRequestTargetBranches,
          ("branches-ignore" .=) <$> pullRequestTargetBranchesIgnore,
          ("paths" .=) <$> pullRequestTargetPaths,
          ("paths-ignore" .=) <$> pullRequestTargetPathsIgnore,
          ("types" .=) <$> pullRequestTargetActivityTypes
        ]

-- | Configuration attributes for push trigger events.
--
-- Push triggers can be filtered by branches, paths, and tags to control exactly
-- when the workflow should run. This allows for fine-grained control over which
-- repository changes should initiate workflow execution.
--
-- Example usage:
--
-- @
-- -- Trigger on pushes to main or develop branches
-- pushMainDevelop :: PushTriggerAttributes
-- pushMainDevelop = PushTriggerAttributes
--  { pushBranches = Just ("main" :| ["develop"])
--  , pushBranchesIgnore = Nothing
--  , pushPaths = Nothing
--  , pushPathsIgnore = Nothing
--  , pushTags = Nothing
--  }
--
-- -- Trigger on pushes to docs directory, ignoring gh-pages
-- pushDocsOnly :: PushTriggerAttributes
-- pushDocsOnly = PushTriggerAttributes
--  { pushBranches = Nothing
--  , pushBranchesIgnore = Just ("gh-pages" :| [])
--  , pushPaths = Just ("docs\/**" :| [])
--  , pushPathsIgnore = Nothing
--  , pushTags = Nothing
--  }
-- @
--
-- For more details, see: <https://docs.github.com/en/actions/writing-workflows/choosing-when-your-workflow-runs/events-that-trigger-workflows#push>
data PushTriggerAttributes = PushTriggerAttributes
  { -- | Branches to include (default: all)
    pushBranches :: Maybe (NonEmpty Text),
    -- | Branches to exclude
    pushBranchesIgnore :: Maybe (NonEmpty Text),
    -- | File paths to include (default: all)
    pushPaths :: Maybe (NonEmpty Text),
    -- | File paths to exclude
    pushPathsIgnore :: Maybe (NonEmpty Text),
    -- | Tags to include
    pushTags :: Maybe (NonEmpty Text)
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON PushTriggerAttributes where
  parseJSON = Aeson.withObject "PushTriggerAttributes" $ \o -> do
    pushBranches <- o .:? "branches"
    pushBranchesIgnore <- o .:? "branches-ignore"
    pushPaths <- o .:? "paths"
    pushPathsIgnore <- o .:? "paths-ignore"
    pushTags <- o .:? "tags"
    pure PushTriggerAttributes {..}

instance ToJSON PushTriggerAttributes where
  toJSON PushTriggerAttributes {..} =
    Aeson.object $
      catMaybes
        [ ("branches" .=) <$> pushBranches,
          ("branches-ignore" .=) <$> pushBranchesIgnore,
          ("paths" .=) <$> pushPaths,
          ("paths-ignore" .=) <$> pushPathsIgnore,
          ("tags" .=) <$> pushTags
        ]

data RegistryPackageActivityType
  = RegistryPackagePublished
  | RegistryPackageUpdated
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON RegistryPackageActivityType where
  parseJSON =
    Aeson.withText "RegistryPackageActivityType" $
      hoistFail' . parseRegistryPackageActivityType

instance ToJSON RegistryPackageActivityType where
  toJSON = Aeson.String . renderRegistryPackageActivityType

renderRegistryPackageActivityType :: RegistryPackageActivityType -> Text
renderRegistryPackageActivityType = \case
  RegistryPackagePublished -> "published"
  RegistryPackageUpdated -> "updated"

parseRegistryPackageActivityType :: Text -> Either String RegistryPackageActivityType
parseRegistryPackageActivityType t =
  maybe (Left [i|Unknown RegistryPackageActivityType: #{t}|]) Right $
    inverseMap renderRegistryPackageActivityType t

data ReleaseActivityType
  = ReleaseCreated
  | ReleaseDeleted
  | ReleaseEdited
  | ReleasePrereleased
  | ReleasePublished
  | ReleaseReleased
  | ReleaseUnpublished
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON ReleaseActivityType where
  parseJSON =
    Aeson.withText "ReleaseActivityType" $ hoistFail' . parseReleaseActivityType

instance ToJSON ReleaseActivityType where
  toJSON = Aeson.String . renderReleaseActivityType

renderReleaseActivityType :: ReleaseActivityType -> Text
renderReleaseActivityType = \case
  ReleaseCreated -> "created"
  ReleaseDeleted -> "deleted"
  ReleaseEdited -> "edited"
  ReleasePrereleased -> "prereleased"
  ReleasePublished -> "published"
  ReleaseReleased -> "released"
  ReleaseUnpublished -> "unpublished"

parseReleaseActivityType :: Text -> Either String ReleaseActivityType
parseReleaseActivityType t =
  maybe (Left [i|Unknown ReleaseActivityType: #{t}|]) Right $
    inverseMap renderReleaseActivityType t

data WorkflowCallInputType
  = WorkflowCallInputTypeBoolean
  | WorkflowCallInputTypeNumber
  | WorkflowCallInputTypeString
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON WorkflowCallInputType where
  parseJSON =
    Aeson.withText "WorkflowCallInputType" $ hoistFail' . parseWorkflowCallInputType

instance ToJSON WorkflowCallInputType where
  toJSON = Aeson.String . renderWorkflowCallInputType

renderWorkflowCallInputType :: WorkflowCallInputType -> Text
renderWorkflowCallInputType = \case
  WorkflowCallInputTypeBoolean -> "boolean"
  WorkflowCallInputTypeNumber -> "number"
  WorkflowCallInputTypeString -> "string"

parseWorkflowCallInputType :: Text -> Either String WorkflowCallInputType
parseWorkflowCallInputType t =
  maybe (Left [i|Unknown WorkflowCallInputType: #{t}|]) Right $
    inverseMap renderWorkflowCallInputType t

data WorkflowCallInput = WorkflowCallInput
  { workflowCallInputDescription :: Maybe Text,
    workflowCallInputDefault :: Maybe Aeson.Value,
    workflowCallInputRequired :: Maybe Bool,
    workflowCallInputType :: WorkflowCallInputType
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON WorkflowCallInput where
  parseJSON = Aeson.withObject "WorkflowCallInput" $ \o -> do
    workflowCallInputDescription <- o .:? "description"
    workflowCallInputDefault <- o .:? "default"
    workflowCallInputRequired <- o .:? "required"
    workflowCallInputType <- o .: "type"
    pure WorkflowCallInput {..}

instance ToJSON WorkflowCallInput where
  toJSON WorkflowCallInput {..} =
    Aeson.object $
      catMaybes
        [ ("description" .=) <$> workflowCallInputDescription,
          ("default" .=) <$> workflowCallInputDefault,
          ("required" .=) <$> workflowCallInputRequired,
          Just $ "type" .= workflowCallInputType
        ]

data WorkflowCallOutput = WorkflowCallOutput
  { workflowCallOutputDescription :: Maybe Text,
    workflowCallOutputValue :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON WorkflowCallOutput where
  parseJSON = Aeson.withObject "WorkflowCallOutput" $ \o -> do
    workflowCallOutputDescription <- o .:? "description"
    workflowCallOutputValue <- o .: "value"
    pure WorkflowCallOutput {..}

instance ToJSON WorkflowCallOutput where
  toJSON WorkflowCallOutput {..} =
    Aeson.object $
      catMaybes
        [ ("description" .=) <$> workflowCallOutputDescription,
          Just $ "value" .= workflowCallOutputValue
        ]

data WorkflowCallSecret = WorkflowCallSecret
  { workflowCallSecretDescription :: Maybe Text,
    workflowCallSecretRequired :: Maybe Bool
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON WorkflowCallSecret where
  parseJSON = Aeson.withObject "WorkflowCallSecret" $ \o -> do
    workflowCallSecretDescription <- o .:? "description"
    workflowCallSecretRequired <- o .:? "required"
    pure WorkflowCallSecret {..}

instance ToJSON WorkflowCallSecret where
  toJSON WorkflowCallSecret {..} =
    Aeson.object $
      catMaybes
        [ ("description" .=) <$> workflowCallSecretDescription,
          ("required" .=) <$> workflowCallSecretRequired
        ]

data WorkflowCallAttributes = WorkflowCallAttributes
  { workflowCallInputs :: Maybe (Map Text WorkflowCallInput),
    workflowCallOutputs :: Maybe (Map Text WorkflowCallOutput),
    workflowCallSecrets :: Maybe (Map Text WorkflowCallSecret)
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON WorkflowCallAttributes where
  parseJSON = Aeson.withObject "WorkflowCallAttributes" $ \o -> do
    workflowCallInputs <- o .:? "inputs"
    workflowCallOutputs <- o .:? "outputs"
    workflowCallSecrets <- o .:? "secrets"
    pure WorkflowCallAttributes {..}

instance ToJSON WorkflowCallAttributes where
  toJSON WorkflowCallAttributes {..} =
    Aeson.object $
      catMaybes
        [ ("inputs" .=) <$> workflowCallInputs,
          ("outputs" .=) <$> workflowCallOutputs,
          ("secrets" .=) <$> workflowCallSecrets
        ]

data WorkflowDispatchInputType
  = WorkflowDispatchInputTypeBoolean
  | WorkflowDispatchInputTypeChoice (NonEmpty Text)
  | WorkflowDispatchInputTypeEnvironment
  | WorkflowDispatchInputTypeNumber
  | WorkflowDispatchInputTypeString
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON WorkflowDispatchInputType where
  parseJSON = Aeson.withObject "WorkflowDispatchInputType" $ \o -> do
    t :: Text <- o .: "type"
    maybeOptions <- o .:? "options"
    case t of
      "boolean" -> pure WorkflowDispatchInputTypeBoolean
      "choice" -> case maybeOptions of
        Just (op :| ops) -> pure $ WorkflowDispatchInputTypeChoice $ op :| ops
        Nothing -> fail "Expected a non-empty list of options"
      "environment" -> pure WorkflowDispatchInputTypeEnvironment
      "number" -> pure WorkflowDispatchInputTypeNumber
      "string" -> pure WorkflowDispatchInputTypeString
      _ -> fail [i|Unknown WorkflowDispatchInputType: #{t}|]

data WorkflowDispatchInput = WorkflowDispatchInput
  { workflowDispatchInputDescription :: Maybe Text,
    workflowDispatchInputDefault :: Maybe Aeson.Value,
    workflowDispatchInputRequired :: Maybe Bool,
    workflowDispatchInputType :: Maybe WorkflowDispatchInputType
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON WorkflowDispatchInput where
  parseJSON = Aeson.withObject "WorkflowDispatchInput" $ \o -> do
    workflowDispatchInputDescription <- o .:? "description"
    workflowDispatchInputDefault <- o .:? "default"
    workflowDispatchInputRequired <- o .:? "required"
    workflowDispatchInputTypeText :: Maybe Text <- o .:? "type"
    workflowDispatchInputType <-
      maybe
        (pure Nothing)
        (const . Aeson.parseJSON $ Aeson.Object o)
        workflowDispatchInputTypeText
    pure WorkflowDispatchInput {..}

instance ToJSON WorkflowDispatchInput where
  toJSON WorkflowDispatchInput {..} =
    Aeson.object $
      catMaybes
        [ ("description" .=) <$> workflowDispatchInputDescription,
          ("default" .=) <$> workflowDispatchInputDefault,
          ("required" .=) <$> workflowDispatchInputRequired,
          ("type" .=)
            . ( \case
                  WorkflowDispatchInputTypeBoolean ->
                    "boolean" :: Text
                  WorkflowDispatchInputTypeChoice _ ->
                    "choice"
                  WorkflowDispatchInputTypeEnvironment ->
                    "environment"
                  WorkflowDispatchInputTypeNumber ->
                    "number"
                  WorkflowDispatchInputTypeString ->
                    "string"
              )
            <$> workflowDispatchInputType,
          workflowDispatchInputType
            >>= ( \case
                    WorkflowDispatchInputTypeChoice ops ->
                      Just $ "options" .= ops
                    _ ->
                      Nothing
                )
        ]

newtype WorkflowDispatchAttributes = WorkflowDispatchAttributes
  { workflowDispatchInputs :: Maybe (Map Text WorkflowDispatchInput)
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON WorkflowDispatchAttributes where
  parseJSON = Aeson.withObject "WorkflowDispatchAttributes" $ \o -> do
    workflowDispatchInputs <- o .:? "inputs"
    pure WorkflowDispatchAttributes {..}

instance ToJSON WorkflowDispatchAttributes where
  toJSON WorkflowDispatchAttributes {..} =
    Aeson.object $
      catMaybes
        [ ("inputs" .=) <$> workflowDispatchInputs
        ]

data WorkflowRunActivityType
  = WorkflowRunCompleted
  | WorkflowRunInProgress
  | WorkflowRunRequested
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON WorkflowRunActivityType where
  parseJSON =
    Aeson.withText "WorkflowRunActivityType" $ hoistFail' . parseWorkflowRunActivityType

instance ToJSON WorkflowRunActivityType where
  toJSON = Aeson.String . renderWorkflowRunActivityType

renderWorkflowRunActivityType :: WorkflowRunActivityType -> Text
renderWorkflowRunActivityType = \case
  WorkflowRunCompleted -> "completed"
  WorkflowRunInProgress -> "in_progress"
  WorkflowRunRequested -> "requested"

parseWorkflowRunActivityType :: Text -> Either String WorkflowRunActivityType
parseWorkflowRunActivityType t =
  maybe (Left [i|Unknown WorkflowRunActivityType: #{t}|]) Right $
    inverseMap renderWorkflowRunActivityType t

data WorkflowRunTriggerAttributes = WorkflowRunTriggerAttributes
  { workflowRunActivityTypes :: NonEmpty WorkflowRunActivityType,
    workflowRunWorkflows :: Maybe (NonEmpty Text),
    workflowRunBranches :: Maybe (NonEmpty Text),
    workflowRunBranchesIgnore :: Maybe (NonEmpty Text)
  }
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON WorkflowRunTriggerAttributes where
  parseJSON = Aeson.withObject "WorkflowRunTriggerAttributes" $ \o -> do
    workflowRunActivityTypes <- o .: "types"
    workflowRunWorkflows <- o .:? "workflows"
    workflowRunBranches <- o .:? "branches"
    workflowRunBranchesIgnore <- o .:? "branches-ignore"
    pure WorkflowRunTriggerAttributes {..}

instance ToJSON WorkflowRunTriggerAttributes where
  toJSON WorkflowRunTriggerAttributes {..} =
    Aeson.object $
      catMaybes
        [ Just $ "types" .= workflowRunActivityTypes,
          ("workflows" .=) <$> workflowRunWorkflows,
          ("branches" .=) <$> workflowRunBranches,
          ("branches-ignore" .=) <$> workflowRunBranchesIgnore
        ]

-- | Comprehensive enumeration of all GitHub Actions workflow trigger events.
--
-- Each trigger corresponds to a specific GitHub event that can initiate a workflow run.
-- Many triggers can be configured with activity types to specify exactly which sub-events
-- should cause the workflow to run.
--
-- Common trigger examples:
--
-- * 'PushTrigger' - Runs on pushes to repository branches or tags
-- * 'PullRequestTrigger' - Runs on pull request events (open, sync, close, etc.)
-- * 'ScheduleTrigger' - Runs on a schedule using cron syntax
-- * 'WorkflowDispatchTrigger' - Allows manual workflow execution
-- * 'IssuesTrigger' - Runs on issue events (open, close, label, etc.)
--
-- For triggers with complex configuration, attribute types provide filtering options:
--
-- @
-- -- Push trigger with branch filtering
-- pushToMain :: WorkflowTrigger
-- pushToMain = PushTrigger $ PushTriggerAttributes
--  { pushBranches = Just ("main" :| [])
--  , pushBranchesIgnore = Nothing
--  , pushPaths = Nothing
--  , pushPathsIgnore = Nothing
--  , pushTags = Nothing
--  }
--
-- -- Issue trigger for specific activity types
-- issueEvents :: WorkflowTrigger
-- issueEvents = IssuesTrigger (IssuesOpened :| [IssuesClosed, IssuesLabeled])
-- @
--
-- For complete event documentation, see: <https://docs.github.com/en/actions/writing-workflows/choosing-when-your-workflow-runs/events-that-trigger-workflows>
data WorkflowTrigger
  = -- | Branch protection rule events
    BranchProtectionRuleTrigger (NonEmpty BranchProtectionRuleActivityType)
  | -- | Check run events
    CheckRunTrigger (NonEmpty CheckRunActivityType)
  | -- | Check suite completion
    CheckSuiteCompletedTrigger
  | -- | Branch or tag creation
    CreateTrigger
  | -- | Branch or tag deletion
    DeleteTrigger
  | -- | Deployment events
    DeploymentTrigger
  | -- | Deployment status changes
    DeploymentStatusTrigger
  | -- | Discussion events
    DiscussionTrigger (NonEmpty DiscussionActivityType)
  | -- | Discussion comment events
    DiscussionCommentTrigger (NonEmpty DiscussionCommentActivityType)
  | -- | Repository fork events
    ForkTrigger
  | -- | Wiki page events
    GollumTrigger
  | -- | Issue comment events
    IssueCommentTrigger (NonEmpty IssueCommentActivityType)
  | -- | Issue events
    IssuesTrigger (NonEmpty IssuesActivityType)
  | -- | Label events
    LabelTrigger (NonEmpty LabelActivityType)
  | -- | Merge group check requests
    MergeGroupChecksRequestedTrigger
  | -- | Milestone events
    MilestoneTrigger (NonEmpty MilestoneActivityType)
  | -- | GitHub Pages build events
    PageBuildTrigger
  | -- | Repository publicity changes
    PublicTrigger
  | -- | Pull request events with filtering
    PullRequestTrigger PullRequestTriggerAttributes
  | -- | Pull request review events
    PullRequestReviewTrigger (NonEmpty PullRequestReviewActivityType)
  | -- | PR review comment events
    PullRequestReviewCommentTrigger (NonEmpty PullRequestReviewCommentActivityType)
  | -- | Pull request target events with filtering
    PullRequestTargetTrigger PullRequestTargetTriggerAttributes
  | -- | Push events with filtering
    PushTrigger PushTriggerAttributes
  | -- | Package registry events
    RegistryPackageTrigger (NonEmpty RegistryPackageActivityType)
  | -- | Release events
    ReleaseTrigger (NonEmpty ReleaseActivityType)
  | -- | External repository dispatch events
    RepositoryDispatchTrigger (NonEmpty Text)
  | -- | Scheduled events (cron expressions)
    ScheduleTrigger (NonEmpty Text)
  | -- | Commit status events
    StatusTrigger
  | -- | Repository watch events
    WatchStartedTrigger
  | -- | Reusable workflow calls
    WorkflowCallTrigger WorkflowCallAttributes
  | -- | Manual workflow dispatch
    WorkflowDispatchTrigger WorkflowDispatchAttributes
  | -- | Workflow run events with filtering
    WorkflowRunTrigger WorkflowRunTriggerAttributes
  deriving stock (Eq, Generic, Ord, Show)

instance FromJSON WorkflowTrigger where
  parseJSON = Aeson.withObject "WorkflowTrigger" $ \o -> do
    maybeBranchProtectionRuleActivityTypes <-
      maybeActivityTypesInNestedObject o "branch_protection_rule"
    maybeCheckRunActivityTypes <- maybeActivityTypesInNestedObject o "check_run"
    maybeCheckSuiteActivityTypes :: Maybe (NonEmpty Text) <-
      maybeActivityTypesInNestedObject o "check_suite"
    maybeCreate :: Maybe Aeson.Value <- o .:? "create"
    maybeDelete :: Maybe Aeson.Value <- o .:? "delete"
    maybeDeployment :: Maybe Aeson.Value <- o .:? "deployment"
    maybeDeploymentStatus :: Maybe Aeson.Value <- o .:? "deployment_status"
    maybeDiscussionActivityTypes <-
      maybeActivityTypesInNestedObject o "discussion"
    maybeDiscussionCommentActivityTypes <-
      maybeActivityTypesInNestedObject o "discussion_comment"
    maybeFork :: Maybe Aeson.Value <- o .:? "fork"
    maybeGollum :: Maybe Aeson.Value <- o .:? "gollum"
    maybeIssueCommentActivityTypes <-
      maybeActivityTypesInNestedObject o "issue_comment"
    maybeIssuesActivityTypes <- maybeActivityTypesInNestedObject o "issues"
    maybeLabelActivityTypes <- maybeActivityTypesInNestedObject o "label"
    maybeMergeGroupChecksRequestedActivityTypes :: Maybe (NonEmpty Text) <-
      maybeActivityTypesInNestedObject o "merge_group"
    maybeMilestoneActivityTypes <- maybeActivityTypesInNestedObject o "milestone"
    maybePageBuild :: Maybe Aeson.Value <- o .:? "page_build"
    maybePublic :: Maybe Aeson.Value <- o .:? "public"
    maybePullRequestTriggerAttributes <- o .:? "pull_request"
    maybePullRequestReviewActivityTypes <-
      maybeActivityTypesInNestedObject o "pull_request_review"
    maybePullRequestReviewCommentActivityTypes <-
      maybeActivityTypesInNestedObject o "pull_request_review_comment"
    maybePullRequestTargetTriggerAttributes <- o .:? "pull_request_target"
    maybePushTriggerAttributes <- o .:? "push"
    maybeRegistryPackageActivityTypes <- maybeActivityTypesInNestedObject o "registry_package"
    maybeReleaseActivityTypes <- maybeActivityTypesInNestedObject o "release"
    maybeRepositoryDispatchActivityTypes :: Maybe (NonEmpty Text) <-
      maybeActivityTypesInNestedObject o "repository_dispatch"
    maybeScheduleCrons <- maybeScheduleCronsParser o
    maybeStatus :: Maybe Aeson.Value <- o .:? "status"
    maybeWatchStartedActivityTypes :: Maybe (NonEmpty Text) <-
      maybeActivityTypesInNestedObject o "watch"
    maybeWorkflowCall <- o .:? "workflow_call"
    maybeWorkflowDispatch <- o .:? "workflow_dispatch"
    maybeWorkflowRun <- o .:? "workflow_run"
    hoistFail'
      . fromMaybe (Left [i|Invalid workflow trigger (`on` property):\n#{AesonKeyMap.keys o}|])
      $ (Right . BranchProtectionRuleTrigger <$> maybeBranchProtectionRuleActivityTypes)
        <|> (Right . CheckRunTrigger <$> maybeCheckRunActivityTypes)
        <|> ( Right CheckSuiteCompletedTrigger
                <$ guard
                  (maybeCheckSuiteActivityTypes == Just ("completed" :| []))
            )
        <|> (Right CreateTrigger <$ guard (isJust maybeCreate))
        <|> (Right DeleteTrigger <$ guard (isJust maybeDelete))
        <|> (Right DeploymentTrigger <$ guard (isJust maybeDeployment))
        <|> (Right DeploymentStatusTrigger <$ guard (isJust maybeDeploymentStatus))
        <|> (Right . DiscussionTrigger <$> maybeDiscussionActivityTypes)
        <|> (Right . DiscussionCommentTrigger <$> maybeDiscussionCommentActivityTypes)
        <|> (Right ForkTrigger <$ guard (isJust maybeFork))
        <|> (Right GollumTrigger <$ guard (isJust maybeGollum))
        <|> (Right . IssueCommentTrigger <$> maybeIssueCommentActivityTypes)
        <|> (Right . IssuesTrigger <$> maybeIssuesActivityTypes)
        <|> (Right . LabelTrigger <$> maybeLabelActivityTypes)
        <|> ( Right MergeGroupChecksRequestedTrigger
                <$ guard
                  (maybeMergeGroupChecksRequestedActivityTypes == Just ("checks_requested" :| []))
            )
        <|> (Right . MilestoneTrigger <$> maybeMilestoneActivityTypes)
        <|> (Right PageBuildTrigger <$ guard (isJust maybePageBuild))
        <|> (Right PublicTrigger <$ guard (isJust maybePublic))
        <|> (Right . PullRequestTrigger <$> maybePullRequestTriggerAttributes)
        <|> (Right . PullRequestReviewTrigger <$> maybePullRequestReviewActivityTypes)
        <|> (Right . PullRequestReviewCommentTrigger <$> maybePullRequestReviewCommentActivityTypes)
        <|> (Right . PullRequestTargetTrigger <$> maybePullRequestTargetTriggerAttributes)
        <|> (Right . PushTrigger <$> maybePushTriggerAttributes)
        <|> (Right . RegistryPackageTrigger <$> maybeRegistryPackageActivityTypes)
        <|> (Right . ReleaseTrigger <$> maybeReleaseActivityTypes)
        <|> (Right . RepositoryDispatchTrigger <$> maybeRepositoryDispatchActivityTypes)
        <|> (Right . ScheduleTrigger <$> maybeScheduleCrons)
        <|> (Right StatusTrigger <$ guard (isJust maybeStatus))
        <|> ( Right WatchStartedTrigger
                <$ guard
                  (maybeWatchStartedActivityTypes == Just ("started" :| []))
            )
        <|> (Right . WorkflowCallTrigger <$> maybeWorkflowCall)
        <|> (Right . WorkflowDispatchTrigger <$> maybeWorkflowDispatch)
        <|> (Right . WorkflowRunTrigger <$> maybeWorkflowRun)
    where
      maybeActivityTypesInNestedObject ::
        (FromJSON a) =>
        Aeson.Object ->
        Aeson.Key ->
        Aeson.Parser (Maybe (NonEmpty a))
      maybeActivityTypesInNestedObject o attributeName =
        o
          .:? attributeName
          >>= maybe
            (pure Nothing)
            (Aeson.withObject "ActivityTypes" (.: "types"))

      maybeScheduleCronsParser :: Aeson.Object -> Aeson.Parser (Maybe (NonEmpty Text))
      maybeScheduleCronsParser o =
        o
          .:? "schedule"
          >>= maybe
            (pure Nothing)
            ( Aeson.withArray "Crons" $ \a ->
                fmap nonEmpty . for (Vector.toList a) $
                  Aeson.withObject "Cron" (.: "cron")
            )

instance ToJSONKey WorkflowTrigger where
  toJSONKey =
    Aeson.toJSONKeyText $ \case
      BranchProtectionRuleTrigger _ -> "branch_protection_rule"
      CheckRunTrigger _ -> "check_run"
      CheckSuiteCompletedTrigger -> "check_suite"
      CreateTrigger -> "create"
      DeleteTrigger -> "delete"
      DeploymentTrigger -> "deployment"
      DeploymentStatusTrigger -> "deployment_status"
      DiscussionTrigger _ -> "discussion"
      DiscussionCommentTrigger _ -> "discussion_comment"
      ForkTrigger -> "fork"
      GollumTrigger -> "gollum"
      IssueCommentTrigger _ -> "issue_comment"
      IssuesTrigger _ -> "issues"
      LabelTrigger _ -> "label"
      MergeGroupChecksRequestedTrigger -> "merge_group"
      MilestoneTrigger _ -> "milestone"
      PageBuildTrigger -> "page_build"
      PublicTrigger -> "public"
      PullRequestTrigger _ -> "pull_request"
      PullRequestReviewTrigger _ -> "pull_request_review"
      PullRequestReviewCommentTrigger _ -> "pull_request_review_comment"
      PullRequestTargetTrigger _ -> "pull_request_target"
      PushTrigger _ -> "push"
      RegistryPackageTrigger _ -> "registry_package"
      ReleaseTrigger _ -> "release"
      RepositoryDispatchTrigger _ -> "repository_dispatch"
      ScheduleTrigger _ -> "schedule"
      StatusTrigger -> "status"
      WatchStartedTrigger -> "watch"
      WorkflowCallTrigger _ -> "workflow_call"
      WorkflowDispatchTrigger _ -> "workflow_dispatch"
      WorkflowRunTrigger _ -> "workflow_run"

instance ToJSON WorkflowTrigger where
  toJSON trigger =
    Aeson.toJSON . Map.singleton trigger $ case trigger of
      BranchProtectionRuleTrigger activityTypes ->
        Aeson.object ["types" .= activityTypes]
      CheckRunTrigger activityTypes ->
        Aeson.object ["types" .= activityTypes]
      CheckSuiteCompletedTrigger ->
        Aeson.object ["types" .= [Aeson.String "completed"]]
      CreateTrigger ->
        Aeson.object []
      DeleteTrigger ->
        Aeson.object []
      DeploymentTrigger ->
        Aeson.object []
      DeploymentStatusTrigger ->
        Aeson.object []
      DiscussionTrigger activityTypes ->
        Aeson.object ["types" .= activityTypes]
      DiscussionCommentTrigger activityTypes ->
        Aeson.object ["types" .= activityTypes]
      ForkTrigger ->
        Aeson.object []
      GollumTrigger ->
        Aeson.object []
      IssueCommentTrigger activityTypes ->
        Aeson.object ["types" .= activityTypes]
      IssuesTrigger activityTypes ->
        Aeson.object ["types" .= activityTypes]
      LabelTrigger activityTypes ->
        Aeson.object ["types" .= activityTypes]
      MergeGroupChecksRequestedTrigger ->
        Aeson.object ["types" .= [Aeson.String "checks_requested"]]
      MilestoneTrigger activityTypes ->
        Aeson.object ["types" .= activityTypes]
      PageBuildTrigger ->
        Aeson.object []
      PublicTrigger ->
        Aeson.object []
      PullRequestTrigger attrs ->
        Aeson.toJSON attrs
      PullRequestReviewTrigger activityTypes ->
        Aeson.object ["types" .= activityTypes]
      PullRequestReviewCommentTrigger activityTypes ->
        Aeson.object ["types" .= activityTypes]
      PullRequestTargetTrigger attrs ->
        Aeson.toJSON attrs
      PushTrigger attrs ->
        Aeson.toJSON attrs
      RegistryPackageTrigger activityTypes ->
        Aeson.object ["types" .= activityTypes]
      ReleaseTrigger activityTypes ->
        Aeson.object ["types" .= activityTypes]
      RepositoryDispatchTrigger activityTypes ->
        Aeson.object ["types" .= activityTypes]
      ScheduleTrigger crons ->
        Aeson.toJSON $ (\cron -> Aeson.object ["cron" .= cron]) <$> crons
      StatusTrigger ->
        Aeson.object []
      WatchStartedTrigger ->
        Aeson.object ["types" .= [Aeson.String "started"]]
      WorkflowCallTrigger attrs ->
        Aeson.toJSON attrs
      WorkflowDispatchTrigger attrs ->
        Aeson.toJSON attrs
      WorkflowRunTrigger attrs ->
        Aeson.toJSON attrs

-- | Generate a random 'WorkflowTrigger' for property-based testing.
--
-- This generator creates workflow triggers with randomized properties suitable for testing
-- JSON serialization roundtrips and other property-based tests. It covers all trigger types
-- and their associated activity types and attributes.
gen :: (MonadGen m) => m WorkflowTrigger
gen =
  Gen.choice
    [ BranchProtectionRuleTrigger <$> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded,
      CheckRunTrigger <$> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded,
      pure CheckSuiteCompletedTrigger,
      pure CreateTrigger,
      pure DeleteTrigger,
      pure DeploymentTrigger,
      pure DeploymentStatusTrigger,
      DiscussionTrigger <$> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded,
      DiscussionCommentTrigger <$> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded,
      pure ForkTrigger,
      pure GollumTrigger,
      IssueCommentTrigger <$> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded,
      IssuesTrigger <$> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded,
      LabelTrigger <$> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded,
      pure MergeGroupChecksRequestedTrigger,
      MilestoneTrigger <$> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded,
      pure PageBuildTrigger,
      pure PublicTrigger,
      PullRequestTrigger <$> genPullRequestTriggerAttributes,
      PullRequestReviewTrigger <$> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded,
      PullRequestReviewCommentTrigger <$> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded,
      PullRequestTargetTrigger <$> genPullRequestTargetAttributes,
      PushTrigger <$> genPushTriggerAttributes,
      RegistryPackageTrigger <$> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded,
      ReleaseTrigger <$> Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded,
      RepositoryDispatchTrigger <$> Gen.nonEmpty (Range.linear 1 3) genText,
      ScheduleTrigger <$> Gen.nonEmpty (Range.linear 1 5) genText,
      pure StatusTrigger,
      pure WatchStartedTrigger,
      WorkflowCallTrigger <$> genWorkflowCallAttributes,
      WorkflowDispatchTrigger <$> genWorkflowDispatchAttributes,
      WorkflowRunTrigger <$> genWorkflowRunTriggerAttributes
    ]

genText :: (MonadGen m) => m Text
genText = Gen.text (Range.linear 1 5) Gen.alphaNum

genMap :: (MonadGen m) => m a -> m (Map Text a)
genMap ga = Gen.map (Range.linear 1 5) $ liftA2 (,) genText ga

genPullRequestTargetAttributes :: (MonadGen m) => m PullRequestTargetTriggerAttributes
genPullRequestTargetAttributes = do
  pullRequestTargetActivityTypes <-
    Gen.maybe $
      Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded
  pullRequestTargetBranches <-
    Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  pullRequestTargetBranchesIgnore <-
    Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  pullRequestTargetPaths <-
    Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  pullRequestTargetPathsIgnore <-
    Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  pure PullRequestTargetTriggerAttributes {..}

genPullRequestTriggerAttributes :: (MonadGen m) => m PullRequestTriggerAttributes
genPullRequestTriggerAttributes = do
  pullRequestActivityTypes <-
    Gen.maybe $
      Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded
  pullRequestBranches <-
    Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  pullRequestBranchesIgnore <-
    Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  pullRequestPaths <-
    Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  pullRequestPathsIgnore <-
    Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  pure PullRequestTriggerAttributes {..}

genPushTriggerAttributes :: (MonadGen m) => m PushTriggerAttributes
genPushTriggerAttributes = do
  pushBranches <-
    Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  pushBranchesIgnore <-
    Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  pushPaths <-
    Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  pushPathsIgnore <-
    Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  pushTags <-
    Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  pure PushTriggerAttributes {..}

genWorkflowCallAttributes :: (MonadGen m) => m WorkflowCallAttributes
genWorkflowCallAttributes = do
  workflowCallInputs <- Gen.maybe $ genMap genWorkflowCallInput
  workflowCallOutputs <- Gen.maybe $ genMap genWorkflowCallOutput
  workflowCallSecrets <- Gen.maybe $ genMap genWorkflowCallSecret
  pure WorkflowCallAttributes {..}

genWorkflowCallInput :: (MonadGen m) => m WorkflowCallInput
genWorkflowCallInput = do
  workflowCallInputDescription <- Gen.maybe genText
  workflowCallInputDefault <-
    Gen.maybe $
      Aeson.String
        <$> Gen.text (Range.linear 3 20) Gen.alphaNum
  workflowCallInputRequired <- Gen.maybe Gen.bool
  workflowCallInputType <- Gen.enumBounded
  pure WorkflowCallInput {..}

genWorkflowCallOutput :: (MonadGen m) => m WorkflowCallOutput
genWorkflowCallOutput = do
  workflowCallOutputDescription <- Gen.maybe genText
  workflowCallOutputValue <- genText
  pure WorkflowCallOutput {..}

genWorkflowCallSecret :: (MonadGen m) => m WorkflowCallSecret
genWorkflowCallSecret = do
  workflowCallSecretDescription <- Gen.maybe genText
  workflowCallSecretRequired <- Gen.maybe Gen.bool
  pure WorkflowCallSecret {..}

genWorkflowDispatchAttributes :: (MonadGen m) => m WorkflowDispatchAttributes
genWorkflowDispatchAttributes = do
  workflowDispatchInputs <- Gen.maybe $ genMap genWorkflowDispatchInput
  pure WorkflowDispatchAttributes {..}

genWorkflowDispatchInput :: (MonadGen m) => m WorkflowDispatchInput
genWorkflowDispatchInput = do
  workflowDispatchInputDescription <- Gen.maybe genText
  workflowDispatchInputDefault <-
    Gen.maybe $
      Aeson.String
        <$> Gen.text (Range.linear 3 20) Gen.alphaNum
  workflowDispatchInputRequired <- Gen.maybe Gen.bool
  workflowDispatchInputType <- Gen.maybe genWorkflowDispatchInputType
  pure WorkflowDispatchInput {..}

genWorkflowDispatchInputType :: (MonadGen m) => m WorkflowDispatchInputType
genWorkflowDispatchInputType = do
  Gen.choice
    [ pure WorkflowDispatchInputTypeBoolean,
      WorkflowDispatchInputTypeChoice <$> Gen.nonEmpty (Range.linear 1 5) genText,
      pure WorkflowDispatchInputTypeEnvironment,
      pure WorkflowDispatchInputTypeNumber,
      pure WorkflowDispatchInputTypeString
    ]

genWorkflowRunTriggerAttributes :: (MonadGen m) => m WorkflowRunTriggerAttributes
genWorkflowRunTriggerAttributes = do
  workflowRunActivityTypes <- Gen.nonEmpty (Range.linear 1 3) Gen.enumBounded
  workflowRunWorkflows <- Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  workflowRunBranches <- Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  workflowRunBranchesIgnore <- Gen.maybe $ Gen.nonEmpty (Range.linear 1 5) genText
  pure WorkflowRunTriggerAttributes {..}
