{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @show_hosts@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex joins.
module Effects.Database.Tables.ShowHost
  ( -- * Host Role Type
    HostRole (..),

    -- * Table Definition
    ShowHost (..),
    showHostSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Result Types
    ShowHostWithUser (..),

    -- * Queries
    getShowHosts,
    getShowHostsWithUsers,
    insertShowHost,
    removeShowHost,
    isUserHostOfShow,
    isUserHostOfShowSlug,
    addHostToShow,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..), RecordInstance (..))
import Data.Time (UTCTime)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import OrphanInstances.UTCTime ()
import Rel8 hiding (Insert)

--------------------------------------------------------------------------------
-- Host Role Type

-- | Host role within a show.
data HostRole = Host | CoHost | Guest
  deriving stock (Generic, Show, Eq, Ord, Prelude.Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance DBType HostRole where
  typeInformation =
    parseTypeInformation
      ( \case
          "host" -> Right Host
          "co-host" -> Right CoHost
          "guest" -> Right Guest
          other -> Left $ "Invalid HostRole: " <> Text.unpack other
      )
      ( \case
          Host -> "host"
          CoHost -> "co-host"
          Guest -> "guest"
      )
      typeInformation

instance DBEq HostRole

instance Display HostRole where
  displayBuilder Host = "host"
  displayBuilder CoHost = "co-host"
  displayBuilder Guest = "guest"

instance DecodeValue HostRole where
  decodeValue = Decoders.enum decodeHostRole

decodeHostRole :: Text -> Maybe HostRole
decodeHostRole = \case
  "host" -> Just Host
  "co-host" -> Just CoHost
  "guest" -> Just Guest
  _ -> Nothing

instance EncodeValue HostRole where
  encodeValue = Encoders.enum $ \case
    Host -> "host"
    CoHost -> "co-host"
    Guest -> "guest"

--------------------------------------------------------------------------------
-- Table Definition

-- | The @show_hosts@ table definition using rel8's higher-kinded data pattern.
--
-- This is a junction table with a composite primary key (show_id, user_id).
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data ShowHost f = ShowHost
  { shmShowId :: Column f Shows.Id,
    shmUserId :: Column f User.Id,
    shmRole :: Column f HostRole,
    shmIsPrimary :: Column f Bool,
    shmJoinedAt :: Column f UTCTime,
    shmLeftAt :: Column f (Maybe UTCTime)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (ShowHost f)

deriving stock instance (f ~ Result) => Eq (ShowHost f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (ShowHost Result)

-- | Display instance for ShowHost Result.
instance Display (ShowHost Result) where
  displayBuilder sh =
    "ShowHost { showId = "
      <> displayBuilder (shmShowId sh)
      <> ", userId = "
      <> displayBuilder (shmUserId sh)
      <> ", role = "
      <> displayBuilder (shmRole sh)
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @ShowHost Result@.
type Model = ShowHost Result

-- | Table schema connecting the Haskell type to the database table.
showHostSchema :: TableSchema (ShowHost Name)
showHostSchema =
  TableSchema
    { name = "show_hosts",
      columns =
        ShowHost
          { shmShowId = "show_id",
            shmUserId = "user_id",
            shmRole = "role",
            shmIsPrimary = "is_primary",
            shmJoinedAt = "joined_at",
            shmLeftAt = "left_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for adding a host to a show.
data Insert = Insert
  { shiId :: Shows.Id,
    shiUserId :: User.Id,
    shiRole :: HostRole,
    shiIsPrimary :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Result Types

-- | Show host with user information (for SQL joins).
data ShowHostWithUser = ShowHostWithUser
  { showId :: Shows.Id,
    userId :: User.Id,
    role :: HostRole,
    isPrimary :: Bool,
    joinedAt :: UTCTime,
    leftAt :: Maybe UTCTime,
    userEmail :: Text,
    userCreatedAt :: UTCTime,
    userUpdatedAt :: UTCTime,
    displayName :: DisplayName,
    fullName :: Text,
    avatarUrl :: Maybe Text,
    bio :: Maybe Text,
    websiteUrl :: Maybe Text,
    instagramHandle :: Maybe Text,
    twitterHandle :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    bandcampUrl :: Maybe Text
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance ShowHostWithUser)
  deriving anyclass (DecodeRow, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Queries

-- | Get hosts for a show (active hosts only).
getShowHosts :: Shows.Id -> Hasql.Statement () [Model]
getShowHosts showId =
  run $
    select $
      orderBy ((shmIsPrimary >$< desc) <> (shmJoinedAt >$< asc)) do
        sh <- each showHostSchema
        where_ $ shmShowId sh ==. lit showId
        where_ $ isNull (shmLeftAt sh)
        pure sh

-- | Get show hosts with user information.
--
-- Uses raw SQL because this query involves multiple joins across users,
-- user_metadata, and host_details tables.
getShowHostsWithUsers :: Shows.Id -> Hasql.Statement () [ShowHostWithUser]
getShowHostsWithUsers showId =
  interp
    False
    [sql|
    SELECT
      sh.show_id,
      sh.user_id,
      sh.role,
      sh.is_primary,
      sh.joined_at,
      sh.left_at,

      u.email,
      u.created_at,
      u.updated_at,

      um.display_name,
      um.full_name,
      um.avatar_url,

      hd.bio,
      hd.website_url,
      hd.instagram_handle,
      hd.twitter_handle,
      hd.soundcloud_url,
      hd.bandcamp_url

    FROM show_hosts sh
    JOIN users u ON sh.user_id = u.id
    JOIN user_metadata um ON u.id = um.user_id
    LEFT JOIN host_details hd ON hd.user_id = sh.user_id
    WHERE sh.show_id = #{showId} AND sh.left_at IS NULL
    ORDER BY sh.is_primary DESC, sh.joined_at ASC
  |]

-- | Add host to show.
--
-- Uses raw SQL because of ON CONFLICT DO UPDATE clause.
insertShowHost :: Insert -> Hasql.Statement () ()
insertShowHost Insert {..} =
  interp
    False
    [sql|
    INSERT INTO show_hosts(show_id, user_id, role, is_primary, joined_at)
    VALUES (#{shiId}, #{shiUserId}, #{shiRole}, #{shiIsPrimary}, NOW())
    ON CONFLICT (show_id, user_id)
    DO UPDATE SET role = #{shiRole}, is_primary = #{shiIsPrimary}, left_at = NULL
  |]

-- | Remove host from show (set left_at timestamp).
--
-- Uses raw SQL for the UPDATE with NOW() timestamp.
removeShowHost :: Shows.Id -> User.Id -> Hasql.Statement () ()
removeShowHost showId userId =
  interp
    False
    [sql|
    UPDATE show_hosts
    SET left_at = NOW()
    WHERE show_id = #{showId} AND user_id = #{userId}
  |]

-- | Check if user is host of show.
isUserHostOfShow :: User.Id -> Shows.Id -> Hasql.Statement () Bool
isUserHostOfShow userId showId =
  let query =
        interp
          True
          [sql|
        SELECT EXISTS(
          SELECT 1 FROM show_hosts
          WHERE user_id = #{userId} AND show_id = #{showId} AND left_at IS NULL
        )
      |]
   in maybe False getOneColumn <$> query

-- | Check if user is host of show by slug (excludes soft-deleted shows).
isUserHostOfShowSlug :: User.Id -> Slug -> Hasql.Statement () Bool
isUserHostOfShowSlug userId slug =
  let query =
        interp
          True
          [sql|
        SELECT EXISTS(
          SELECT 1 FROM show_hosts
          JOIN shows on shows.id = show_hosts.show_id
          WHERE user_id = #{userId} AND slug = #{slug} AND left_at IS NULL AND shows.deleted_at IS NULL
        )
      |]
   in maybe False getOneColumn <$> query

-- | Add host to show (convenience function).
addHostToShow :: Shows.Id -> User.Id -> Hasql.Statement () ()
addHostToShow showId userId =
  insertShowHost
    Insert
      { shiId = showId,
        shiUserId = userId,
        shiRole = Host,
        shiIsPrimary = False
      }
