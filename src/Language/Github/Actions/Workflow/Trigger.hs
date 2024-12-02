{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

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

import Control.Monad.Fail.Hoist (hoistFail')
import Data.Aeson (FromJSON, ToJSON, ToJSONKey, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Data.Traversable (for)
import Data.Vector qualified as Vector
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Relude

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
  maybe (fail [i|Unknown BranchProtectionRuleActivityType: #{t}|]) pure $
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
  maybe (fail [i|Unknown CheckRunActivityType: #{t}|]) pure $
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
  maybe (fail [i|Unknown DiscussionActivityType: #{t}|]) pure $
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
  maybe (fail [i|Unknown DiscussionCommentActivityType: #{t}|]) pure $
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
  maybe (fail [i|Unknown IssueCommentActivityType: #{t}|]) pure $
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
  maybe (fail [i|Unknown IssuesActivityType: #{t}|]) pure $
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
  maybe (fail [i|Unknown LabelActivityType: #{t}|]) pure $
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
  maybe (fail [i|Unknown MilestoneActivityType: #{t}|]) pure $
    inverseMap renderMilestoneActivityType t

data PullRequestActivityType
  = PullRequestAssigned
  | PullRequestAutoMergeDisabled
  | PullRequestAutoMergeEnabled
  | PullRequestClosed
  | PullRequestConvertedToDraft
  | PullRequestDemilestoned
  | PullRequestDequeued
  | PullRequestEdited
  | PullRequestEnqueued
  | PullRequestLabeled
  | PullRequestLocked
  | PullRequestMilestoned
  | PullRequestOpened
  | PullRequestReadyForReview
  | PullRequestReopened
  | PullRequestReviewRequestRemoved
  | PullRequestReviewRequested
  | PullRequestSynchronize
  | PullRequestUnassigned
  | PullRequestUnlabeled
  | PullRequestUnlocked
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
  maybe (fail [i|Unknown PullRequestActivityType: #{t}|]) pure $
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
  maybe (fail [i|Unknown PullRequestReviewActivityType: #{t}|]) pure $
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
  maybe (fail [i|Unknown PullRequestReviewCommentActivityType: #{t}|]) pure $
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
  maybe (fail [i|Unknown PullRequestTargetActivityType: #{t}|]) pure $
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

data PushTriggerAttributes = PushTriggerAttributes
  { pushBranches :: Maybe (NonEmpty Text),
    pushBranchesIgnore :: Maybe (NonEmpty Text),
    pushPaths :: Maybe (NonEmpty Text),
    pushPathsIgnore :: Maybe (NonEmpty Text),
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
  maybe (fail [i|Unknown RegistryPackageActivityType: #{t}|]) pure $
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
  maybe (fail [i|Unknown ReleaseActivityType: #{t}|]) pure $
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
  maybe (fail [i|Unknown WorkflowCallInputType: #{t}|]) pure $
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
  maybe (fail [i|Unknown WorkflowRunActivityType: #{t}|]) pure $
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

data WorkflowTrigger
  = BranchProtectionRuleTrigger (NonEmpty BranchProtectionRuleActivityType)
  | CheckRunTrigger (NonEmpty CheckRunActivityType)
  | CheckSuiteCompletedTrigger
  | CreateTrigger
  | DeleteTrigger
  | DeploymentTrigger
  | DeploymentStatusTrigger
  | DiscussionTrigger (NonEmpty DiscussionActivityType)
  | DiscussionCommentTrigger (NonEmpty DiscussionCommentActivityType)
  | ForkTrigger
  | GollumTrigger
  | IssueCommentTrigger (NonEmpty IssueCommentActivityType)
  | IssuesTrigger (NonEmpty IssuesActivityType)
  | LabelTrigger (NonEmpty LabelActivityType)
  | MergeGroupChecksRequestedTrigger
  | MilestoneTrigger (NonEmpty MilestoneActivityType)
  | PageBuildTrigger
  | PublicTrigger
  | PullRequestTrigger PullRequestTriggerAttributes
  | PullRequestReviewTrigger (NonEmpty PullRequestReviewActivityType)
  | PullRequestReviewCommentTrigger (NonEmpty PullRequestReviewCommentActivityType)
  | PullRequestTargetTrigger PullRequestTargetTriggerAttributes
  | PushTrigger PushTriggerAttributes
  | RegistryPackageTrigger (NonEmpty RegistryPackageActivityType)
  | ReleaseTrigger (NonEmpty ReleaseActivityType)
  | RepositoryDispatchTrigger (NonEmpty Text)
  | ScheduleTrigger (NonEmpty Text)
  | StatusTrigger
  | WatchStartedTrigger
  | WorkflowCallTrigger WorkflowCallAttributes
  | WorkflowDispatchTrigger WorkflowDispatchAttributes
  | WorkflowRunTrigger WorkflowRunTriggerAttributes
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
