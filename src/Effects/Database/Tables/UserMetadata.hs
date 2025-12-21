{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Database table definition and queries for @user_metadata@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex joins.
module Effects.Database.Tables.UserMetadata
  ( -- * UserRole
    UserRole (..),
    isAdmin,
    isStaffOrHigher,
    isHostOrHigher,

    -- * Suspension
    SuspensionStatus (..),
    isSuspended,

    -- * Color Scheme
    ColorScheme (..),

    -- * Id Type
    Id (..),

    -- * Table Definition
    UserMetadata (..),
    userMetadataSchema,

    -- * Model (Result alias)
    Model (..),

    -- * Insert Type
    Insert (..),

    -- * Update Type
    Update (..),

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

    -- * Result Types
    UserWithMetadata (..),
    UserWithMetadataInsert (..),
  )
where

--------------------------------------------------------------------------------

import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Password.Argon2 (Argon2, PasswordHash)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display, RecordInstance (..), display, displayBuilder)
import Data.Time (UTCTime)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Domain.Types.UserSortBy (UserSortBy (..))
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow (..), DecodeValue (..), EncodeValue (..), OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import OrphanInstances.UTCTime ()
import Rel8 hiding (Enum, Insert, Update)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified
import Prelude

--------------------------------------------------------------------------------
-- UserRole Type

-- | User role for authorization.
data UserRole = User | Host | Staff | Admin
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance DBType UserRole where
  typeInformation =
    parseTypeInformation
      ( \case
          "User" -> Right User
          "Host" -> Right Host
          "Staff" -> Right Staff
          "Admin" -> Right Admin
          other -> Left $ "Invalid UserRole: " <> Text.unpack other
      )
      ( \case
          User -> "User"
          Host -> "Host"
          Staff -> "Staff"
          Admin -> "Admin"
      )
      typeInformation

instance DBEq UserRole

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

instance DBType SuspensionStatus where
  typeInformation =
    parseTypeInformation
      ( \case
          "NotSuspended" -> Right NotSuspended
          "Suspended" -> Right Suspended
          other -> Left $ "Invalid SuspensionStatus: " <> Text.unpack other
      )
      ( \case
          NotSuspended -> "NotSuspended"
          Suspended -> "Suspended"
      )
      typeInformation

instance DBEq SuspensionStatus

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

--------------------------------------------------------------------------------
-- Color Scheme

-- | User's preferred color scheme for the UI
data ColorScheme = Automatic | LightMode | DarkMode
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)
  deriving (DBType) via (Rel8.Enum ColorScheme)

-- | DBEnum instance for ColorScheme to map to PostgreSQL color_scheme enum type
instance Rel8.DBEnum ColorScheme where
  enumTypeName = Rel8.QualifiedName {Rel8.name = "color_scheme", Rel8.schema = Nothing}

instance DBEq ColorScheme

instance Display ColorScheme where
  displayBuilder Automatic = "Automatic"
  displayBuilder LightMode = "LightMode"
  displayBuilder DarkMode = "DarkMode"

instance DecodeValue ColorScheme where
  decodeValue = Decoders.enum $ \case
    "Automatic" -> Just Automatic
    "LightMode" -> Just LightMode
    "DarkMode" -> Just DarkMode
    _ -> Nothing

instance EncodeValue ColorScheme where
  encodeValue = Encoders.enum $ \case
    Automatic -> "Automatic"
    LightMode -> "LightMode"
    DarkMode -> "DarkMode"

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for user_metadata primary keys.
--
-- Provides type safety to prevent mixing up IDs from different tables.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq, DBOrd)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Table Definition

-- | The @user_metadata@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
--
-- Note: This table joins with the @users@ table to get suspension_status.
-- The rel8 schema only includes columns from user_metadata itself.
data UserMetadata f = UserMetadata
  { umId :: Column f Id,
    umUserId :: Column f User.Id,
    umDisplayName :: Column f DisplayName,
    umFullName :: Column f FullName,
    umAvatarUrl :: Column f (Maybe Text),
    umUserRole :: Column f UserRole,
    umColorScheme :: Column f ColorScheme,
    umCreatedAt :: Column f UTCTime,
    umUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (UserMetadata f)

deriving stock instance (f ~ Result) => Eq (UserMetadata f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (UserMetadata Result)

-- | Display instance for UserMetadata Result.
instance Display (UserMetadata Result) where
  displayBuilder um =
    "UserMetadata { id = "
      <> displayBuilder (umId um)
      <> ", userId = "
      <> displayBuilder (umUserId um)
      <> ", displayName = "
      <> displayBuilder (umDisplayName um)
      <> " }"

-- | Table schema connecting the Haskell type to the database table.
userMetadataSchema :: TableSchema (UserMetadata Name)
userMetadataSchema =
  TableSchema
    { name = "user_metadata",
      columns =
        UserMetadata
          { umId = "id",
            umUserId = "user_id",
            umDisplayName = "display_name",
            umFullName = "full_name",
            umAvatarUrl = "avatar_url",
            umUserRole = "user_role",
            umColorScheme = "color_scheme",
            umCreatedAt = "created_at",
            umUpdatedAt = "updated_at"
          }
    }

--------------------------------------------------------------------------------
-- Model Types

-- | Database Model for the @user_metadata@ table.
--
-- This type includes suspension_status which comes from the users table via JOIN.
-- For queries that need suspension status, use raw SQL joins.
data Model = Model
  { mId :: Id,
    mUserId :: User.Id,
    mDisplayName :: DisplayName,
    mFullName :: FullName,
    mAvatarUrl :: Maybe Text,
    mUserRole :: UserRole,
    mColorScheme :: ColorScheme,
    mSuspensionStatus :: SuspensionStatus
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

-- | Check if a model indicates the user is suspended
isSuspended :: Model -> Bool
isSuspended user = user.mSuspensionStatus == Suspended

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new user metadata.
data Insert = Insert
  { iUserId :: User.Id,
    iDisplayName :: DisplayName,
    iFullName :: FullName,
    iAvatarUrl :: Maybe Text,
    iUserRole :: UserRole,
    iColorScheme :: ColorScheme
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Update Type

-- | Model for updating user metadata fields
data Update = Update
  { uDisplayName :: Maybe DisplayName,
    uFullName :: Maybe FullName,
    uAvatarUrl :: Maybe (Maybe Text),
    uColorScheme :: Maybe ColorScheme
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Update)

--------------------------------------------------------------------------------
-- Result Types

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

-- | Model for inserting both user and metadata in a single transaction
data UserWithMetadataInsert = UserWithMetadataInsert
  { uwmiEmail :: EmailAddress,
    uwmiPassword :: PasswordHash Argon2,
    uwmiDisplayName :: DisplayName,
    uwmiFullName :: FullName,
    uwmiAvatarUrl :: Maybe Text,
    uwmiUserRole :: UserRole,
    uwmiColorScheme :: ColorScheme
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance UserWithMetadataInsert)

--------------------------------------------------------------------------------
-- Queries

-- | Get user metadata by user ID
--
-- Only returns metadata if the user is active (not deleted).
-- This is used by authentication to verify that logged-in users haven't been deleted.
--
-- Uses raw SQL because it joins with users table to check deleted_at and get suspension_status.
getUserMetadata :: User.Id -> Hasql.Statement () (Maybe Model)
getUserMetadata userId =
  interp
    False
    [sql|
    SELECT um.id, um.user_id, um.display_name, um.full_name, um.avatar_url, um.user_role, um.color_scheme, u.suspension_status
    FROM user_metadata um
    INNER JOIN users u ON um.user_id = u.id
    WHERE um.user_id = #{userId}
      AND u.deleted_at IS NULL
  |]

-- | Insert user metadata.
insertUserMetadata :: Insert -> Hasql.Statement () Id
insertUserMetadata Insert {..} =
  fmap head $
    run $
      insert
        Rel8.Insert
          { into = userMetadataSchema,
            rows =
              values
                [ UserMetadata
                    { umId = coerce (nextval "user_metadata_id_seq"),
                      umUserId = lit iUserId,
                      umDisplayName = lit iDisplayName,
                      umFullName = lit iFullName,
                      umAvatarUrl = lit iAvatarUrl,
                      umUserRole = lit iUserRole,
                      umColorScheme = lit iColorScheme,
                      umCreatedAt = now,
                      umUpdatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning umId
          }

-- | Insert user and metadata in a single atomic transaction
--
-- Uses raw SQL because it involves CTE with multiple tables.
insertUserWithMetadata :: UserWithMetadataInsert -> Hasql.Statement () User.Id
insertUserWithMetadata UserWithMetadataInsert {..} =
  head
    <$> interp
      True
      [sql|
    WITH new_user AS (
      INSERT INTO users(email, password, created_at, updated_at)
      VALUES (#{uwmiEmail}, #{uwmiPassword}, NOW(), NOW())
      RETURNING id
    )
    , new_metadata AS (
      INSERT INTO user_metadata(user_id, display_name, full_name, avatar_url, user_role, color_scheme, created_at, updated_at)
      SELECT id, #{uwmiDisplayName}, #{uwmiFullName}, #{uwmiAvatarUrl}, #{uwmiUserRole}, #{uwmiColorScheme}::color_scheme, NOW(), NOW()
      FROM new_user
      RETURNING user_id
    )
    SELECT id FROM new_user
  |]

--------------------------------------------------------------------------------
-- Admin Management Queries

-- | Get all users with pagination, including full user info
--
-- Only returns active (non-deleted) users.
-- Uses raw SQL because it joins user_metadata with users table.
getAllUsersWithPagination :: Limit -> Offset -> Hasql.Statement () [UserWithMetadata]
getAllUsersWithPagination (Limit lim) (Offset off) =
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
    LIMIT #{lim} OFFSET #{off}
  |]

-- | Get users filtered by role(s) with pagination and sorting
--
-- Only returns active (non-deleted) users.
-- Pass an empty list for roles to include all roles.
-- Uses raw SQL because of complex sorting and array filtering.
getUsersByRole :: [UserRole] -> Limit -> Offset -> UserSortBy -> Hasql.Statement () [UserWithMetadata]
getUsersByRole roles (Limit lim) (Offset off) sortBy =
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
        LIMIT #{lim} OFFSET #{off}
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
        LIMIT #{lim} OFFSET #{off}
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
        LIMIT #{lim} OFFSET #{off}
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
        LIMIT #{lim} OFFSET #{off}
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
        LIMIT #{lim} OFFSET #{off}
      |]

-- | Search users by display name or email with optional role filter, pagination, and sorting
--
-- Only returns active (non-deleted) users.
-- Pass an empty list for roles to include all roles.
-- Uses raw SQL because of ILIKE pattern matching and complex sorting.
searchUsers :: Text -> [UserRole] -> Limit -> Offset -> UserSortBy -> Hasql.Statement () [UserWithMetadata]
searchUsers query roles (Limit lim) (Offset off) sortBy =
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
        LIMIT #{lim} OFFSET #{off}
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
        LIMIT #{lim} OFFSET #{off}
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
        LIMIT #{lim} OFFSET #{off}
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
        LIMIT #{lim} OFFSET #{off}
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
        LIMIT #{lim} OFFSET #{off}
      |]

-- | Get a single user with metadata by user ID
--
-- Only returns the user if they are active (not deleted).
-- Uses raw SQL because it joins user_metadata with users table.
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
--
-- Uses rel8 update.
updateUserRole :: User.Id -> UserRole -> Hasql.Statement () (Maybe Id)
updateUserRole userId newRole =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = userMetadataSchema,
            from = pure (),
            set = \_ um -> um {umUserRole = lit newRole, umUpdatedAt = now},
            updateWhere = \_ um -> umUserId um ==. lit userId,
            returning = Returning umId
          }

-- | Update user metadata fields (NULL values preserve existing data)
--
-- Uses raw SQL because of complex COALESCE logic with nullable fields.
updateUserMetadata :: Id -> Update -> Hasql.Statement () (Maybe Id)
updateUserMetadata metadataId Update {..} =
  listToMaybe
    <$> interp
      True
      [sql|
      UPDATE user_metadata
      SET
        display_name = COALESCE(#{uDisplayName}, display_name),
        full_name = COALESCE(#{uFullName}, full_name),
        avatar_url = CASE
          WHEN #{hasAvatarUpdate} THEN #{avatarValue}
          ELSE avatar_url
        END,
        color_scheme = CASE
          WHEN #{hasColorSchemeUpdate} THEN #{colorSchemeValue}::color_scheme
          ELSE color_scheme
        END,
        updated_at = NOW()
      WHERE id = #{metadataId}
      RETURNING id
    |]
  where
    hasAvatarUpdate = case uAvatarUrl of
      Nothing -> False
      Just _ -> True
    avatarValue = join uAvatarUrl
    hasColorSchemeUpdate = case uColorScheme of
      Nothing -> False
      Just _ -> True
    -- When hasColorSchemeUpdate is True, uColorScheme is guaranteed to be Just
    -- We use a default that won't be used due to the CASE WHEN guard
    colorSchemeValue = fromMaybe Automatic uColorScheme

-- | Count total active users (excluding soft-deleted users)
--
-- Uses raw SQL because it requires counting from users table with deleted_at filter.
countUsers :: Hasql.Statement () Int64
countUsers =
  let query =
        interp
          False
          [sql| SELECT COUNT(*)::bigint FROM users WHERE deleted_at IS NULL |]
   in maybe 0 getOneColumn <$> query

-- | Count active users by role (excluding soft-deleted users)
--
-- Uses raw SQL because it joins user_metadata with users table.
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
--
-- Uses raw SQL because it involves CTE with multiple tables.
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
--
-- Uses raw SQL because it involves CTE with multiple tables.
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
--
-- Uses raw SQL because it operates on the users table directly.
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
--
-- Uses raw SQL because it queries the users table directly.
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
