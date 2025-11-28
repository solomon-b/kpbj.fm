{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Database.Tables.UserMetadata
  ( -- * UserRole
    UserRole (..),
    isAdmin,
    isStaffOrHigher,
    isHostOrHigher,

    -- * Suspension
    SuspensionStatus (..),
    isSuspended,

    -- * Model Types
    Id (..),
    Model (..),
    ModelInsert (..),
    ModelUpdate (..),
    UserWithMetadata (..),
    UserWithMetadataInsert (..),

    -- * Queries
    getUserMetadata,
    insertUserMetadata,
    insertUserWithMetadata,
    getAllUsersWithPagination,
    getUsersByRole,
    searchUsers,
    getUserWithMetadataById,
    updateUserRole,
    updateUserMetadata,
    countUsers,
    countUsersByRole,
    softDeleteUser,

    -- * Suspension Queries
    suspendUser,
    unsuspendUser,
    getSuspensionStatus,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..), display, displayBuilder)
import Data.Time (UTCTime)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.UserSortBy (UserSortBy (..))
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneColumn (..), OneRow, interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- UserRole Type

data UserRole = User | Host | Staff | Admin
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance Display UserRole where
  displayBuilder User = "User"
  displayBuilder Host = "Host"
  displayBuilder Staff = "Staff"
  displayBuilder Admin = "Admin"

instance DecodeValue UserRole where
  decodeValue = Decoders.enum $ \case
    "User" -> Just User
    "Host" -> Just Host
    "Staff" -> Just Staff
    "Admin" -> Just Admin
    _ -> Nothing

instance EncodeValue UserRole where
  encodeValue = Encoders.enum $ \case
    User -> "User"
    Host -> "Host"
    Staff -> "Staff"
    Admin -> "Admin"

instance Servant.FromHttpApiData UserRole where
  parseUrlPiece "User" = Right User
  parseUrlPiece "Host" = Right Host
  parseUrlPiece "Staff" = Right Staff
  parseUrlPiece "Admin" = Right Admin
  parseUrlPiece invalid = Left $ "Invalid UserRole: " <> invalid

instance Servant.ToHttpApiData UserRole where
  toUrlPiece = display

-- | Check if a user role has administrative privileges
isAdmin :: UserRole -> Bool
isAdmin Admin = True
isAdmin _ = False

-- | Check if a user role has staff-level privileges or higher
isStaffOrHigher :: UserRole -> Bool
isStaffOrHigher Staff = True
isStaffOrHigher Admin = True
isStaffOrHigher _ = False

-- | Check if a user role has host-level privileges or higher
isHostOrHigher :: UserRole -> Bool
isHostOrHigher Host = True
isHostOrHigher Staff = True
isHostOrHigher Admin = True
isHostOrHigher _ = False

--------------------------------------------------------------------------------
-- Suspension Status

-- | Represents a user's suspension status
--
-- Suspended users can still log in but will see a warning banner and cannot
-- perform host actions. Their show relationships are preserved.
data SuspensionStatus = NotSuspended | Suspended
  deriving stock (Generic, Show, Eq)

instance Display SuspensionStatus where
  displayBuilder NotSuspended = "NotSuspended"
  displayBuilder Suspended = "Suspended"

instance DecodeValue SuspensionStatus where
  decodeValue = Decoders.enum $ \case
    "Suspended" -> Just Suspended
    "NotSuspended" -> Just NotSuspended
    _ -> Nothing

instance EncodeValue SuspensionStatus where
  encodeValue = Encoders.enum $ \case
    Suspended -> "Suspended"
    NotSuspended -> "NotSuspended"

-- | Check if a suspension status indicates the user is suspended
isSuspended :: Model -> Bool
isSuspended user = user.mSuspensionStatus == Suspended

--------------------------------------------------------------------------------
-- Model

newtype Id = Id Int64
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      ToJSON,
      FromJSON,
      Display,
      DecodeValue,
      EncodeValue
    )

-- | Database Model for the @user_metadata@ table.
data Model = Model
  { mId :: Id,
    mUserId :: User.Id,
    mDisplayName :: DisplayName,
    mFullName :: FullName,
    mAvatarUrl :: Maybe Text,
    mUserRole :: UserRole,
    mSuspensionStatus :: SuspensionStatus
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

--------------------------------------------------------------------------------

-- | Get user metadata by user ID
--
-- Only returns metadata if the user is active (not deleted).
-- This is used by authentication to verify that logged-in users haven't been deleted.
getUserMetadata :: User.Id -> Hasql.Statement () (Maybe Model)
getUserMetadata userId =
  interp
    False
    [sql|
    SELECT um.id, um.user_id, um.display_name, um.full_name, um.avatar_url, um.user_role, u.suspension_status
    FROM user_metadata um
    INNER JOIN users u ON um.user_id = u.id
    WHERE um.user_id = #{userId}
      AND u.deleted_at IS NULL
  |]

data ModelInsert = ModelInsert
  { miUserId :: User.Id,
    miDisplayName :: DisplayName,
    miFullName :: FullName,
    miAvatarUrl :: Maybe Text,
    miUserRole :: UserRole
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via ModelInsert
  deriving (Display) via (RecordInstance ModelInsert)

insertUserMetadata :: ModelInsert -> Hasql.Statement () (OneRow Id)
insertUserMetadata ModelInsert {..} =
  interp
    False
    [sql|
    INSERT INTO user_metadata(user_id, display_name, full_name, avatar_url, user_role, created_at, updated_at)
    VALUES (#{miUserId}, #{miDisplayName}, #{miFullName}, #{miAvatarUrl}, #{miUserRole}, NOW(), NOW())
    RETURNING id
  |]

-- | Model for inserting both user and metadata in a single transaction
data UserWithMetadataInsert = UserWithMetadataInsert
  { uwmiEmail :: EmailAddress,
    uwmiPassword :: PasswordHash Argon2,
    uwmiDisplayName :: DisplayName,
    uwmiFullName :: FullName,
    uwmiAvatarUrl :: Maybe Text,
    uwmiUserRole :: UserRole
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via UserWithMetadataInsert
  deriving (Display) via (RecordInstance UserWithMetadataInsert)

-- | Insert user and metadata in a single atomic transaction
insertUserWithMetadata :: UserWithMetadataInsert -> Hasql.Statement () (OneRow User.Id)
insertUserWithMetadata UserWithMetadataInsert {..} =
  interp
    False
    [sql|
    WITH new_user AS (
      INSERT INTO users(email, password, created_at, updated_at)
      VALUES (#{uwmiEmail}, #{uwmiPassword}, NOW(), NOW())
      RETURNING id
    )
    , new_metadata AS (
      INSERT INTO user_metadata(user_id, display_name, full_name, avatar_url, user_role, created_at, updated_at)
      SELECT id, #{uwmiDisplayName}, #{uwmiFullName}, #{uwmiAvatarUrl}, #{uwmiUserRole}, NOW(), NOW()
      FROM new_user
      RETURNING user_id
    )
    SELECT id FROM new_user
  |]

--------------------------------------------------------------------------------
-- Admin Management Queries

-- | Combined user and metadata for admin queries
data UserWithMetadata = UserWithMetadata
  { uwmUserId :: User.Id,
    uwmEmail :: EmailAddress,
    uwmUserCreatedAt :: UTCTime,
    uwmUserUpdatedAt :: UTCTime,
    uwmSuspendedAt :: Maybe UTCTime,
    uwmSuspensionReason :: Maybe Text,
    uwmMetadataId :: Id,
    uwmUserMetadataUserId :: User.Id,
    uwmDisplayName :: DisplayName,
    uwmFullName :: FullName,
    uwmAvatarUrl :: Maybe Text,
    uwmUserRole :: UserRole
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance UserWithMetadata)

-- | Get all users with pagination, including full user info
--
-- Only returns active (non-deleted) users.
getAllUsersWithPagination :: Limit -> Offset -> Hasql.Statement () [UserWithMetadata]
getAllUsersWithPagination limit offset =
  interp
    True
    [sql|
    SELECT
      u.id, u.email, u.created_at, u.updated_at,
      u.suspended_at, u.suspension_reason,
      um.id, um.user_id, um.display_name, um.full_name,
      um.avatar_url, um.user_role
    FROM users u
    INNER JOIN user_metadata um ON u.id = um.user_id
    WHERE u.deleted_at IS NULL
    ORDER BY u.created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get users filtered by role(s) with pagination and sorting
--
-- Only returns active (non-deleted) users.
-- Pass an empty list for roles to include all roles.
getUsersByRole :: [UserRole] -> Limit -> Offset -> UserSortBy -> Hasql.Statement () [UserWithMetadata]
getUsersByRole roles limit offset sortBy =
  case sortBy of
    JoinDateNewest ->
      interp
        True
        [sql|
        SELECT
          u.id, u.email, u.created_at, u.updated_at,
          u.suspended_at, u.suspension_reason,
          um.id, um.user_id, um.display_name, um.full_name,
          um.avatar_url, um.user_role
        FROM users u
        INNER JOIN user_metadata um ON u.id = um.user_id
        WHERE (cardinality(#{roles}) = 0 OR um.user_role = ANY(#{roles}))
          AND u.deleted_at IS NULL
        ORDER BY u.created_at DESC
        LIMIT #{limit} OFFSET #{offset}
      |]
    JoinDateOldest ->
      interp
        True
        [sql|
        SELECT
          u.id, u.email, u.created_at, u.updated_at,
          u.suspended_at, u.suspension_reason,
          um.id, um.user_id, um.display_name, um.full_name,
          um.avatar_url, um.user_role
        FROM users u
        INNER JOIN user_metadata um ON u.id = um.user_id
        WHERE (cardinality(#{roles}) = 0 OR um.user_role = ANY(#{roles}))
          AND u.deleted_at IS NULL
        ORDER BY u.created_at ASC
        LIMIT #{limit} OFFSET #{offset}
      |]
    NameAZ ->
      interp
        True
        [sql|
        SELECT
          u.id, u.email, u.created_at, u.updated_at,
          u.suspended_at, u.suspension_reason,
          um.id, um.user_id, um.display_name, um.full_name,
          um.avatar_url, um.user_role
        FROM users u
        INNER JOIN user_metadata um ON u.id = um.user_id
        WHERE (cardinality(#{roles}) = 0 OR um.user_role = ANY(#{roles}))
          AND u.deleted_at IS NULL
        ORDER BY um.display_name ASC
        LIMIT #{limit} OFFSET #{offset}
      |]
    ShowCount ->
      interp
        True
        [sql|
        SELECT
          u.id, u.email, u.created_at, u.updated_at,
          u.suspended_at, u.suspension_reason,
          um.id, um.user_id, um.display_name, um.full_name,
          um.avatar_url, um.user_role
        FROM users u
        INNER JOIN user_metadata um ON u.id = um.user_id
        LEFT JOIN show_hosts sh ON u.id = sh.user_id
        WHERE (cardinality(#{roles}) = 0 OR um.user_role = ANY(#{roles}))
          AND u.deleted_at IS NULL
        GROUP BY u.id, u.email, u.created_at, u.updated_at,
                 u.suspended_at, u.suspension_reason,
                 um.id, um.user_id, um.display_name, um.full_name,
                 um.avatar_url, um.user_role
        ORDER BY COUNT(sh.show_id) DESC, u.created_at DESC
        LIMIT #{limit} OFFSET #{offset}
      |]
    StatusSuspended ->
      interp
        True
        [sql|
        SELECT
          u.id, u.email, u.created_at, u.updated_at,
          u.suspended_at, u.suspension_reason,
          um.id, um.user_id, um.display_name, um.full_name,
          um.avatar_url, um.user_role
        FROM users u
        INNER JOIN user_metadata um ON u.id = um.user_id
        WHERE (cardinality(#{roles}) = 0 OR um.user_role = ANY(#{roles}))
          AND u.deleted_at IS NULL
        ORDER BY u.suspended_at IS NULL ASC, u.suspended_at DESC, u.created_at DESC
        LIMIT #{limit} OFFSET #{offset}
      |]

-- | Search users by display name or email with optional role filter, pagination, and sorting
--
-- Only returns active (non-deleted) users.
-- Pass an empty list for roles to include all roles.
searchUsers :: Text -> [UserRole] -> Limit -> Offset -> UserSortBy -> Hasql.Statement () [UserWithMetadata]
searchUsers query roles limit offset sortBy =
  case sortBy of
    JoinDateNewest ->
      interp
        True
        [sql|
        SELECT
          u.id, u.email, u.created_at, u.updated_at,
          u.suspended_at, u.suspension_reason,
          um.id, um.user_id, um.display_name, um.full_name,
          um.avatar_url, um.user_role
        FROM users u
        INNER JOIN user_metadata um ON u.id = um.user_id
        WHERE
          (um.display_name ILIKE ('%' || #{query} || '%') OR
           CAST(u.email AS TEXT) ILIKE ('%' || #{query} || '%'))
          AND (cardinality(#{roles}) = 0 OR um.user_role = ANY(#{roles}))
          AND u.deleted_at IS NULL
        ORDER BY u.created_at DESC
        LIMIT #{limit} OFFSET #{offset}
      |]
    JoinDateOldest ->
      interp
        True
        [sql|
        SELECT
          u.id, u.email, u.created_at, u.updated_at,
          u.suspended_at, u.suspension_reason,
          um.id, um.user_id, um.display_name, um.full_name,
          um.avatar_url, um.user_role
        FROM users u
        INNER JOIN user_metadata um ON u.id = um.user_id
        WHERE
          (um.display_name ILIKE ('%' || #{query} || '%') OR
           CAST(u.email AS TEXT) ILIKE ('%' || #{query} || '%'))
          AND (cardinality(#{roles}) = 0 OR um.user_role = ANY(#{roles}))
          AND u.deleted_at IS NULL
        ORDER BY u.created_at ASC
        LIMIT #{limit} OFFSET #{offset}
      |]
    NameAZ ->
      interp
        True
        [sql|
        SELECT
          u.id, u.email, u.created_at, u.updated_at,
          u.suspended_at, u.suspension_reason,
          um.id, um.user_id, um.display_name, um.full_name,
          um.avatar_url, um.user_role
        FROM users u
        INNER JOIN user_metadata um ON u.id = um.user_id
        WHERE
          (um.display_name ILIKE ('%' || #{query} || '%') OR
           CAST(u.email AS TEXT) ILIKE ('%' || #{query} || '%'))
          AND (cardinality(#{roles}) = 0 OR um.user_role = ANY(#{roles}))
          AND u.deleted_at IS NULL
        ORDER BY um.display_name ASC
        LIMIT #{limit} OFFSET #{offset}
      |]
    ShowCount ->
      interp
        True
        [sql|
        SELECT
          u.id, u.email, u.created_at, u.updated_at,
          u.suspended_at, u.suspension_reason,
          um.id, um.user_id, um.display_name, um.full_name,
          um.avatar_url, um.user_role
        FROM users u
        INNER JOIN user_metadata um ON u.id = um.user_id
        LEFT JOIN show_hosts sh ON u.id = sh.user_id
        WHERE
          (um.display_name ILIKE ('%' || #{query} || '%') OR
           CAST(u.email AS TEXT) ILIKE ('%' || #{query} || '%'))
          AND (cardinality(#{roles}) = 0 OR um.user_role = ANY(#{roles}))
          AND u.deleted_at IS NULL
        GROUP BY u.id, u.email, u.created_at, u.updated_at,
                 u.suspended_at, u.suspension_reason,
                 um.id, um.user_id, um.display_name, um.full_name,
                 um.avatar_url, um.user_role
        ORDER BY COUNT(sh.show_id) DESC, u.created_at DESC
        LIMIT #{limit} OFFSET #{offset}
      |]
    StatusSuspended ->
      interp
        True
        [sql|
        SELECT
          u.id, u.email, u.created_at, u.updated_at,
          u.suspended_at, u.suspension_reason,
          um.id, um.user_id, um.display_name, um.full_name,
          um.avatar_url, um.user_role
        FROM users u
        INNER JOIN user_metadata um ON u.id = um.user_id
        WHERE
          (um.display_name ILIKE ('%' || #{query} || '%') OR
           CAST(u.email AS TEXT) ILIKE ('%' || #{query} || '%'))
          AND (cardinality(#{roles}) = 0 OR um.user_role = ANY(#{roles}))
          AND u.deleted_at IS NULL
        ORDER BY u.suspended_at IS NULL ASC, u.suspended_at DESC, u.created_at DESC
        LIMIT #{limit} OFFSET #{offset}
      |]

-- | Get a single user with metadata by user ID
--
-- Only returns the user if they are active (not deleted).
getUserWithMetadataById :: User.Id -> Hasql.Statement () (Maybe UserWithMetadata)
getUserWithMetadataById userId =
  interp
    False
    [sql|
    SELECT
      u.id, u.email, u.created_at, u.updated_at,
      u.suspended_at, u.suspension_reason,
      um.id, um.user_id, um.display_name, um.full_name,
      um.avatar_url, um.user_role
    FROM users u
    INNER JOIN user_metadata um ON u.id = um.user_id
    WHERE u.id = #{userId}
      AND u.deleted_at IS NULL
  |]

-- | Update user role
updateUserRole :: User.Id -> UserRole -> Hasql.Statement () (Maybe Id)
updateUserRole userId newRole =
  listToMaybe
    <$> interp
      True
      [sql|
      UPDATE user_metadata
      SET user_role = #{newRole}
      WHERE user_id = #{userId}
      RETURNING id
    |]

-- | Model for updating user metadata fields
data ModelUpdate = ModelUpdate
  { muDisplayName :: Maybe DisplayName,
    muFullName :: Maybe FullName,
    muAvatarUrl :: Maybe (Maybe Text)
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance ModelUpdate)

-- | Update user metadata fields (NULL values preserve existing data)
updateUserMetadata :: Id -> ModelUpdate -> Hasql.Statement () (Maybe Id)
updateUserMetadata metadataId ModelUpdate {..} =
  listToMaybe
    <$> interp
      True
      [sql|
      UPDATE user_metadata
      SET
        display_name = COALESCE(#{muDisplayName}, display_name),
        full_name = COALESCE(#{muFullName}, full_name),
        avatar_url = CASE
          WHEN #{hasAvatarUpdate} THEN #{avatarValue}
          ELSE avatar_url
        END
      WHERE id = #{metadataId}
      RETURNING id
    |]
  where
    hasAvatarUpdate = case muAvatarUrl of
      Nothing -> False
      Just _ -> True
    avatarValue = fromMaybe Nothing muAvatarUrl

-- | Count total active users (excluding soft-deleted users)
countUsers :: Hasql.Statement () Int64
countUsers =
  let query =
        interp
          False
          [sql| SELECT COUNT(*)::bigint FROM users WHERE deleted_at IS NULL |]
   in maybe 0 getOneColumn <$> query

-- | Count active users by role (excluding soft-deleted users)
countUsersByRole :: UserRole -> Hasql.Statement () Int64
countUsersByRole role =
  let query =
        interp
          False
          [sql|
        SELECT COUNT(*)::bigint
        FROM user_metadata um
        INNER JOIN users u ON um.user_id = u.id
        WHERE um.user_role = #{role}
          AND u.deleted_at IS NULL
      |]
   in maybe 0 getOneColumn <$> query

--------------------------------------------------------------------------------
-- User Deletion

-- | Soft delete a user by setting deleted_at timestamp and invalidating all sessions
--
-- Atomically performs:
-- 1. Sets deleted_at = NOW() on users table
-- 2. Deletes all active sessions for the user
--
-- Returns user ID on success, Nothing if user doesn't exist or already deleted.
--
-- CASCADE BEHAVIOR: User content is PRESERVED for audit purposes:
-- - Episodes, blog posts, events: Remain with author attribution
-- - Show host assignments: Preserved in show_hosts table
-- - Authentication: Immediately prevented (getUserMetadata returns Nothing)
-- - Sessions: All invalidated immediately
--
-- Deleted users cannot authenticate or perform any actions but their historical
-- contributions remain visible. Hard delete is intentionally not supported.
softDeleteUser :: User.Id -> Hasql.Statement () (Maybe User.Id)
softDeleteUser userId =
  fmap listToMaybe
    . interp
      True
    $ [sql|
    WITH deleted_user AS (
      UPDATE users
      SET deleted_at = NOW()
      WHERE id = #{userId} AND deleted_at IS NULL
      RETURNING id
    ),
    deleted_sessions AS (
      DELETE FROM server_sessions
      WHERE user_id IN (SELECT id FROM deleted_user)
    )
    SELECT id FROM deleted_user
  |]

--------------------------------------------------------------------------------
-- User Suspension

-- | Suspend a user by setting suspended_at timestamp and invalidating all sessions
--
-- Atomically performs:
-- 1. Sets suspended_at = NOW() on users table
-- 2. Sets suspension_status = 'Suspended'
-- 3. Stores suspension reason for audit
-- 4. Deletes all active sessions for the user (forces re-login)
--
-- Returns user ID on success, Nothing if user doesn't exist, already suspended, or deleted.
--
-- BEHAVIOR: Suspended users maintain their relationships:
-- - Show host assignments: Preserved in show_hosts table
-- - Episodes, blog posts: Remain attributed to user
-- - Authentication: Still allowed (user sees suspension banner)
-- - Host actions: Blocked at handler level (checked via SuspensionStatus)
-- - Sessions: Invalidated to force re-login and see suspension notice
--
-- Unlike deletion, suspension is reversible via unsuspendUser.
suspendUser :: User.Id -> Text -> Hasql.Statement () (Maybe User.Id)
suspendUser userId reason =
  fmap listToMaybe
    . interp
      True
    $ [sql|
    WITH suspended_user AS (
      UPDATE users
      SET suspended_at = NOW(),
          suspension_status = 'Suspended',
          suspension_reason = #{reason}
      WHERE id = #{userId}
        AND suspended_at IS NULL
        AND deleted_at IS NULL
      RETURNING id
    ),
    deleted_sessions AS (
      DELETE FROM server_sessions
      WHERE user_id IN (SELECT id FROM suspended_user)
    )
    SELECT id FROM suspended_user
  |]

-- | Unsuspend (reinstate) a user by clearing suspended_at timestamp
--
-- Returns user ID on success, Nothing if user doesn't exist, not suspended, or deleted.
unsuspendUser :: User.Id -> Hasql.Statement () (Maybe User.Id)
unsuspendUser userId =
  fmap listToMaybe
    . interp
      True
    $ [sql|
    UPDATE users
    SET suspended_at = NULL,
        suspension_status = 'NotSuspended',
        suspension_reason = NULL
    WHERE id = #{userId}
      AND suspended_at IS NOT NULL
      AND deleted_at IS NULL
    RETURNING id
  |]

-- | Get suspension status for a user
--
-- Returns the suspension status (NotSuspended or Suspended).
-- Returns Nothing if user doesn't exist or is deleted.
getSuspensionStatus :: User.Id -> Hasql.Statement () (Maybe SuspensionStatus)
getSuspensionStatus userId =
  fmap getOneColumn
    <$> interp
      False
      [sql|
    SELECT suspension_status
    FROM users
    WHERE id = #{userId}
      AND deleted_at IS NULL
  |]
