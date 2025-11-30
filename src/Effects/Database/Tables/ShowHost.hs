{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.ShowHost where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..), displayBuilder)
import Data.Time (UTCTime)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()

--------------------------------------------------------------------------------
-- Host Role Type

data HostRole = Host | CoHost | Guest
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

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
-- Database Model

data Model = Model
  { shmId :: Shows.Id,
    shmUserId :: User.Id,
    shmRole :: HostRole,
    shmIsPrimary :: Bool,
    shmJoinedAt :: UTCTime,
    shmLeftAt :: Maybe UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

data Insert = Insert
  { shiId :: Shows.Id,
    shiUserId :: User.Id,
    shiRole :: HostRole,
    shiIsPrimary :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)
  deriving (Display) via (RecordInstance Insert)

-- | Show host with user information (for SQL joins)
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
-- Database Queries

-- | Get hosts for a show
getShowHosts :: Shows.Id -> Hasql.Statement () [Model]
getShowHosts showId =
  interp
    False
    [sql|
    SELECT show_id, user_id, role, is_primary, joined_at, left_at
    FROM show_hosts
    WHERE show_id = #{showId} AND left_at IS NULL
    ORDER BY is_primary DESC, joined_at ASC
  |]

-- | Get show hosts with user information
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
    JOIN host_details hd ON hd.user_id = sh.user_id
    JOIN users u ON sh.user_id = u.id
    JOIN user_metadata um ON u.id = um.user_id
    WHERE sh.show_id = #{showId} AND sh.left_at IS NULL
    ORDER BY sh.is_primary DESC, sh.joined_at ASC
  |]

-- | Add host to show
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

-- | Remove host from show (set left_at timestamp)
removeShowHost :: Shows.Id -> User.Id -> Hasql.Statement () ()
removeShowHost showId userId =
  interp
    False
    [sql|
    UPDATE show_hosts
    SET left_at = NOW()
    WHERE show_id = #{showId} AND user_id = #{userId}
  |]

-- | Check if user is host of show
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

-- | Check if user is host of show
isUserHostOfShowSlug :: User.Id -> Slug -> Hasql.Statement () Bool
isUserHostOfShowSlug userId slug =
  let query =
        interp
          True
          [sql|
        SELECT EXISTS(
          SELECT 1 FROM show_hosts
          JOIN shows on shows.id = show_hosts.show_id
          WHERE user_id = #{userId} AND slug = #{slug} AND left_at IS NULL
        )
      |]
   in maybe False getOneColumn <$> query

-- | Add host to show (convenience function)
addHostToShow :: Shows.Id -> User.Id -> Hasql.Statement () ()
addHostToShow showId userId =
  insertShowHost
    Insert
      { shiId = showId,
        shiUserId = userId,
        shiRole = Host,
        shiIsPrimary = False
      }

-- | Remove host from show (convenience alias for removeShowHost)
removeHostFromShow :: Shows.Id -> User.Id -> Hasql.Statement () ()
removeHostFromShow = removeShowHost
