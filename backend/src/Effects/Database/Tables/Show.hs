{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.Show where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..), display, displayBuilder)
import Data.Time (Day, UTCTime)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.Genre (Genre)
import Domain.Types.Search (Search)
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneColumn (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Show Status and Frequency Types

data ShowStatus = Active | Inactive
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance Servant.ToHttpApiData ShowStatus where
  toQueryParam = \case
    Active -> "active"
    Inactive -> "inactive"

instance Servant.FromHttpApiData ShowStatus where
  parseQueryParam = maybe (Left "Invalid ShowStatus") Right . decodeShowStatus

instance Display ShowStatus where
  displayBuilder Active = "active"
  displayBuilder Inactive = "inactive"

instance DecodeValue ShowStatus where
  decodeValue = Decoders.enum decodeShowStatus

decodeShowStatus :: Text -> Maybe ShowStatus
decodeShowStatus = \case
  "active" -> Just Active
  "inactive" -> Just Inactive
  _ -> Nothing

instance EncodeValue ShowStatus where
  encodeValue = Encoders.enum $ \case
    Active -> "active"
    Inactive -> "inactive"

data ShowFrequency = Weekly | Biweekly | Monthly | Occasional | OneTime
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance Display ShowFrequency where
  displayBuilder Weekly = "weekly"
  displayBuilder Biweekly = "biweekly"
  displayBuilder Monthly = "monthly"
  displayBuilder Occasional = "occasional"
  displayBuilder OneTime = "one-time"

instance DecodeValue ShowFrequency where
  decodeValue = Decoders.enum decodeShowFrequency

decodeShowFrequency :: Text -> Maybe ShowFrequency
decodeShowFrequency = \case
  "weekly" -> Just Weekly
  "biweekly" -> Just Biweekly
  "monthly" -> Just Monthly
  "occasional" -> Just Occasional
  "one-time" -> Just OneTime
  _ -> Nothing

instance EncodeValue ShowFrequency where
  encodeValue = Encoders.enum $ \case
    Weekly -> "weekly"
    Biweekly -> "biweekly"
    Monthly -> "monthly"
    Occasional -> "occasional"
    OneTime -> "one-time"

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
-- ID Types

newtype ShowId = ShowId Int64
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

newtype ShowScheduleId = ShowScheduleId Int64
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

--------------------------------------------------------------------------------
-- Database Models

data ShowModel = ShowModel
  { id :: ShowId,
    title :: Text,
    slug :: Text,
    description :: Text,
    genre :: Maybe Text,
    logoUrl :: Maybe Text,
    bannerUrl :: Maybe Text,
    status :: ShowStatus,
    frequency :: ShowFrequency,
    durationMinutes :: Maybe Int64,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance ShowModel)

data ShowHostModel = ShowHostModel
  { shmShowId :: ShowId,
    shmUserId :: User.Id,
    shmRole :: HostRole,
    shmIsPrimary :: Bool,
    shmJoinedAt :: UTCTime,
    shmLeftAt :: Maybe UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance ShowHostModel)

data ShowScheduleModel = ShowScheduleModel
  { id :: ShowScheduleId,
    showId :: ShowId,
    dayOfWeek :: Int64,
    startTime :: Text,
    endTime :: Text,
    timezone :: Text,
    isActive :: Bool,
    effectiveFrom :: Day,
    effectiveUntil :: Maybe Day,
    createdAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

instance Display ShowScheduleModel where
  displayBuilder _ = "ShowScheduleModel"

-- | Show host with user information (for SQL joins)
data ShowHostWithUser = ShowHostWithUser
  { showId :: ShowId,
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
    metadataCreatedAt :: UTCTime,
    metadataUpdatedAt :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance ShowHostWithUser)
  deriving anyclass (DecodeRow, FromJSON, ToJSON)

-- | Host details model
data HostDetailsModel = HostDetailsModel
  { id :: Int64,
    userId :: User.Id,
    bio :: Maybe Text,
    websiteUrl :: Maybe Text,
    instagramHandle :: Maybe Text,
    twitterHandle :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    bandcampUrl :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance HostDetailsModel)
  deriving anyclass (DecodeRow, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Insert Types

data ShowInsert = ShowInsert
  { siTitle :: Text,
    siSlug :: Text,
    siDescription :: Text,
    siGenre :: Maybe Text,
    siLogoUrl :: Maybe Text,
    siBannerUrl :: Maybe Text,
    siStatus :: ShowStatus,
    siFrequency :: ShowFrequency,
    siDurationMinutes :: Maybe Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)
  deriving (Display) via (RecordInstance ShowInsert)

data ShowHostInsert = ShowHostInsert
  { shiShowId :: ShowId,
    shiUserId :: User.Id,
    shiRole :: HostRole,
    shiIsPrimary :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)
  deriving (Display) via (RecordInstance ShowHostInsert)

data ShowScheduleInsert = ShowScheduleInsert
  { ssiShowId :: ShowId,
    ssiDayOfWeek :: Int64,
    ssiStartTime :: Text,
    ssiEndTime :: Text,
    ssiTimezone :: Text,
    ssiIsActive :: Bool,
    ssiEffectiveFrom :: Day,
    ssiEffectiveUntil :: Maybe Day
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)

--------------------------------------------------------------------------------
-- Database Queries

-- | Get all active shows ordered by title
getActiveShows :: Hasql.Statement () [ShowModel]
getActiveShows =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, frequency, duration_minutes, created_at, updated_at
    FROM shows
    WHERE status = 'active'
    ORDER BY title
  |]

-- | Get show by slug
getShowBySlug :: Text -> Hasql.Statement () (Maybe ShowModel)
getShowBySlug slug =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, frequency, duration_minutes, created_at, updated_at
    FROM shows
    WHERE slug = #{slug}
  |]

-- | Get show by ID
getShowById :: ShowId -> Hasql.Statement () (Maybe ShowModel)
getShowById showId =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, frequency, duration_minutes, created_at, updated_at
    FROM shows
    WHERE id = #{showId}
  |]

-- | Get shows by status with pagination
getShowsByStatus :: Text -> Int64 -> Int64 -> Hasql.Statement () [ShowModel]
getShowsByStatus status limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, frequency, duration_minutes, created_at, updated_at
    FROM shows
    WHERE status = #{status}
    ORDER BY title
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get all shows with pagination
getAllShows :: Int64 -> Int64 -> Hasql.Statement () [ShowModel]
getAllShows limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, frequency, duration_minutes, created_at, updated_at
    FROM shows
    ORDER BY title
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get shows by genre with pagination
getShowsByGenre :: Genre -> Int64 -> Int64 -> Hasql.Statement () [ShowModel]
getShowsByGenre genre limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, frequency, duration_minutes, created_at, updated_at
    FROM shows
    WHERE genre = #{display genre}
    ORDER BY title
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get shows by genre and status with pagination
getShowsByGenreAndStatus :: Genre -> ShowStatus -> Int64 -> Int64 -> Hasql.Statement () [ShowModel]
getShowsByGenreAndStatus genre status limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, frequency, duration_minutes, created_at, updated_at
    FROM shows
    WHERE genre = #{genre} AND status = #{status}
    ORDER BY title
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Insert a new show
insertShow :: ShowInsert -> Hasql.Statement () ShowId
insertShow ShowInsert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO shows(title, slug, description, genre, logo_url, banner_url, status, frequency, duration_minutes, created_at, updated_at)
    VALUES (#{siTitle}, #{siSlug}, #{siDescription}, #{siGenre}, #{siLogoUrl}, #{siBannerUrl}, #{siStatus}, #{siFrequency}, #{siDurationMinutes}, NOW(), NOW())
    RETURNING id
  |]

-- | Update a show
updateShow :: ShowId -> ShowInsert -> Hasql.Statement () (Maybe ShowId)
updateShow showId ShowInsert {..} =
  interp
    False
    [sql|
    UPDATE shows
    SET title = #{siTitle}, slug = #{siSlug}, description = #{siDescription},
        genre = #{siGenre}, logo_url = #{siLogoUrl}, banner_url = #{siBannerUrl},
        status = #{siStatus}, frequency = #{siFrequency}, duration_minutes = #{siDurationMinutes},
        updated_at = NOW()
    WHERE id = #{showId}
    RETURNING id
  |]

--------------------------------------------------------------------------------
-- Show Host Queries

-- | Get hosts for a show
getShowHosts :: ShowId -> Hasql.Statement () [ShowHostModel]
getShowHosts showId =
  interp
    False
    [sql|
    SELECT show_id, user_id, role, is_primary, joined_at, left_at
    FROM show_hosts
    WHERE show_id = #{showId} AND left_at IS NULL
    ORDER BY is_primary DESC, joined_at ASC
  |]

-- | Get shows for a user (active host assignments)
getShowsForUser :: User.Id -> Hasql.Statement () [ShowModel]
getShowsForUser userId =
  interp
    False
    [sql|
    SELECT s.id, s.title, s.slug, s.description, s.genre, s.logo_url, s.banner_url, s.status, s.frequency, s.duration_minutes, s.created_at, s.updated_at
    FROM shows s
    JOIN show_hosts sh ON s.id = sh.show_id
    WHERE sh.user_id = #{userId} AND sh.left_at IS NULL
    ORDER BY s.title
  |]

-- | Search shows by text query with pagination
searchShows :: Search -> Int64 -> Int64 -> Hasql.Statement () [ShowModel]
searchShows searchTerm limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, frequency, duration_minutes, created_at, updated_at
    FROM shows
    WHERE (title ILIKE #{searchPattern} OR description ILIKE #{searchPattern} OR genre ILIKE #{searchPattern})
    ORDER BY
      CASE
        WHEN title ILIKE #{searchPattern} THEN 1
        WHEN description ILIKE #{searchPattern} THEN 2
        ELSE 3
      END,
      title
    LIMIT #{limit + 1} OFFSET #{offset}
  |]
  where
    searchPattern = "%" <> searchTerm <> "%"

-- | Add host to show
insertShowHost :: ShowHostInsert -> Hasql.Statement () ()
insertShowHost ShowHostInsert {..} =
  interp
    False
    [sql|
    INSERT INTO show_hosts(show_id, user_id, role, is_primary, joined_at)
    VALUES (#{shiShowId}, #{shiUserId}, #{shiRole}, #{shiIsPrimary}, NOW())
    ON CONFLICT (show_id, user_id)
    DO UPDATE SET role = #{shiRole}, is_primary = #{shiIsPrimary}, left_at = NULL
  |]

-- | Remove host from show (set left_at timestamp)
removeShowHost :: ShowId -> User.Id -> Hasql.Statement () ()
removeShowHost showId userId =
  interp
    False
    [sql|
    UPDATE show_hosts
    SET left_at = NOW()
    WHERE show_id = #{showId} AND user_id = #{userId}
  |]

-- | Check if user is host of show
isUserHostOfShow :: User.Id -> ShowId -> Hasql.Statement () Bool
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

--------------------------------------------------------------------------------
-- Show Schedule Queries

-- | Get schedules for a show
getShowSchedules :: ShowId -> Hasql.Statement () [ShowScheduleModel]
getShowSchedules showId =
  interp
    False
    [sql|
    SELECT id, show_id, day_of_week, start_time, end_time, timezone, is_active, effective_from, effective_until, created_at
    FROM show_schedules
    WHERE show_id = #{showId} AND is_active = true
      AND (effective_until IS NULL OR effective_until >= CURRENT_DATE)
    ORDER BY day_of_week, start_time
  |]

-- | Get current weekly schedule (all active shows) - just schedules
getCurrentWeeklySchedule :: Hasql.Statement () [ShowScheduleModel]
getCurrentWeeklySchedule =
  interp
    False
    [sql|
    SELECT ss.id, ss.show_id, ss.day_of_week, ss.start_time, ss.end_time, ss.timezone, ss.is_active, ss.effective_from, ss.effective_until, ss.created_at
    FROM show_schedules ss
    JOIN shows s ON s.id = ss.show_id
    WHERE s.status = 'active' AND ss.is_active = true
      AND (ss.effective_until IS NULL OR ss.effective_until >= CURRENT_DATE)
    ORDER BY ss.day_of_week, ss.start_time
  |]

-- | Insert a new show schedule
insertShowSchedule :: ShowScheduleInsert -> Hasql.Statement () ShowScheduleId
insertShowSchedule ShowScheduleInsert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO show_schedules(show_id, day_of_week, start_time, end_time, timezone, is_active, effective_from, effective_until, created_at)
    VALUES (#{ssiShowId}, #{ssiDayOfWeek}, #{ssiStartTime}, #{ssiEndTime}, #{ssiTimezone}, #{ssiIsActive}, #{ssiEffectiveFrom}, #{ssiEffectiveUntil}, NOW())
    RETURNING id
  |]

-- | Update show schedule
updateShowSchedule :: ShowScheduleId -> ShowScheduleInsert -> Hasql.Statement () (Maybe ShowScheduleId)
updateShowSchedule scheduleId ShowScheduleInsert {..} =
  interp
    False
    [sql|
    UPDATE show_schedules
    SET show_id = #{ssiShowId}, day_of_week = #{ssiDayOfWeek}, start_time = #{ssiStartTime},
        end_time = #{ssiEndTime}, timezone = #{ssiTimezone}, is_active = #{ssiIsActive},
        effective_from = #{ssiEffectiveFrom}, effective_until = #{ssiEffectiveUntil}
    WHERE id = #{scheduleId}
    RETURNING id
  |]

-- | Deactivate show schedule
deactivateShowSchedule :: ShowScheduleId -> Hasql.Statement () (Maybe ShowScheduleId)
deactivateShowSchedule scheduleId =
  interp
    False
    [sql|
    UPDATE show_schedules
    SET is_active = false
    WHERE id = #{scheduleId}
    RETURNING id
  |]

-- | Get show hosts with user information
getShowHostsWithUsers :: ShowId -> Hasql.Statement () [ShowHostWithUser]
getShowHostsWithUsers showId =
  interp
    False
    [sql|
    SELECT
      sh.show_id, sh.user_id, sh.role, sh.is_primary, sh.joined_at, sh.left_at,
      u.email, u.created_at, u.updated_at,
      um.display_name, um.full_name, um.avatar_url, um.created_at, um.updated_at
    FROM show_hosts sh
    JOIN users u ON sh.user_id = u.id
    JOIN user_metadata um ON u.id = um.user_id
    WHERE sh.show_id = #{showId} AND sh.left_at IS NULL
    ORDER BY sh.is_primary DESC, sh.joined_at ASC
  |]

-- | Get host details for a user
getHostDetails :: User.Id -> Hasql.Statement () (Maybe HostDetailsModel)
getHostDetails userId =
  interp
    False
    [sql|
    SELECT id, user_id, bio, website_url, instagram_handle, twitter_handle, soundcloud_url, bandcamp_url, created_at, updated_at
    FROM host_details
    WHERE user_id = #{userId}
  |]
