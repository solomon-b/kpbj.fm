module Sync
  ( SyncPlan (..),
    isProtectedRole,
    computeSyncPlan,
  )
where

--------------------------------------------------------------------------------

import Data.List (sort)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GoogleGroups (GroupMember (..), GroupMemberRole (..))

--------------------------------------------------------------------------------

-- | The reconciliation between the host set (source of truth) and the
-- current Google Group membership. All lists are sorted and lowercased.
data SyncPlan = SyncPlan
  { -- | Hosts missing from the group; added as MEMBER.
    spToAdd :: [Text],
    -- | Non-host MEMBERs to delete from the group.
    spToRemove :: [Text],
    -- | Non-host MANAGER/OWNERs shielded from removal (reported only).
    spProtected :: [Text]
  }
  deriving stock (Show, Eq)

-- | A member with this role is never removed by the sync.
isProtectedRole :: GroupMemberRole -> Bool
isProtectedRole = \case
  RoleOwner -> True
  RoleManager -> True
  _ -> False

-- | Reconcile current group members against the canonical host emails.
computeSyncPlan ::
  -- | Current Google Group members
  [GroupMember] ->
  -- | Host emails from the database
  [Text] ->
  SyncPlan
computeSyncPlan groupMembers hostEmails =
  SyncPlan
    { spToAdd = sortedList toAdd,
      spToRemove = sortedList toRemove,
      spProtected = sortedList protectedNonHost
    }
  where
    groupEmailSet = Set.fromList (map (Text.toLower . gmEmail) groupMembers)
    hostEmailSet = Set.fromList (map Text.toLower hostEmails)
    protectedSet =
      Set.fromList
        [Text.toLower (gmEmail m) | m <- groupMembers, isProtectedRole (gmRole m)]
    nonHostGroup = Set.difference groupEmailSet hostEmailSet
    toRemove = Set.difference nonHostGroup protectedSet
    toAdd = Set.difference hostEmailSet groupEmailSet
    protectedNonHost = Set.intersection nonHostGroup protectedSet
    sortedList = sort . Set.toList
