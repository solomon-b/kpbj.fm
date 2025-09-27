{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.Show where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..), displayBuilder)
import Data.Time (Day, UTCTime)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import GHC.Generics
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneColumn (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Show Status and Frequency Types

data ShowStatus = Active | Inactive | Hiatus | Archived
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance Display ShowStatus where
  displayBuilder Active = "active"
  displayBuilder Inactive = "inactive"
  displayBuilder Hiatus = "hiatus"
  displayBuilder Archived = "archived"

instance DecodeValue ShowStatus where
  decodeValue = Decoders.enum decodeShowStatus

decodeShowStatus :: Text -> Maybe ShowStatus
decodeShowStatus = \case
  "active" -> Just Active
  "inactive" -> Just Inactive
  "hiatus" -> Just Hiatus
  "archived" -> Just Archived
  _ -> Nothing

instance EncodeValue ShowStatus where
  encodeValue = Encoders.enum $ \case
    Active -> "active"
    Inactive -> "inactive"
    Hiatus -> "hiatus"
    Archived -> "archived"

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
  { smId :: ShowId,
    smTitle :: Text,
    smSlug :: Text,
    smDescription :: Text,
    smGenre :: Maybe Text,
    smLogoUrl :: Maybe Text,
    smBannerUrl :: Maybe Text,
    smStatus :: ShowStatus,
    smFrequency :: ShowFrequency,
    smDurationMinutes :: Maybe Int64,
    smCreatedAt :: UTCTime,
    smUpdatedAt :: UTCTime
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
  { ssmId :: ShowScheduleId,
    ssmShowId :: ShowId,
    ssmDayOfWeek :: Int64,
    ssmStartTime :: Text,
    ssmEndTime :: Text,
    ssmTimezone :: Text,
    ssmIsActive :: Bool,
    ssmEffectiveFrom :: Day,
    ssmEffectiveUntil :: Maybe Day,
    ssmCreatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

--------------------------------------------------------------------------------
-- Domain Types

data ShowDomain = ShowDomain
  { sdId :: ShowId,
    sdTitle :: Text,
    sdSlug :: Text,
    sdDescription :: Text,
    sdGenre :: Maybe Text,
    sdLogoUrl :: Maybe Text,
    sdBannerUrl :: Maybe Text,
    sdStatus :: ShowStatus,
    sdFrequency :: ShowFrequency,
    sdDurationMinutes :: Maybe Int64,
    sdCreatedAt :: UTCTime,
    sdUpdatedAt :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance ShowDomain)
  deriving anyclass (FromJSON, ToJSON)

data ShowHostDomain = ShowHostDomain
  { shdShowId :: ShowId,
    shdUserId :: User.Id,
    shdRole :: HostRole,
    shdIsPrimary :: Bool,
    shdJoinedAt :: UTCTime,
    shdLeftAt :: Maybe UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance ShowHostDomain)
  deriving anyclass (FromJSON, ToJSON)

data ShowScheduleDomain = ShowScheduleDomain
  { ssdId :: ShowScheduleId,
    ssdShowId :: ShowId,
    ssdDayOfWeek :: Int64,
    ssdStartTime :: Text,
    ssdEndTime :: Text,
    ssdTimezone :: Text,
    ssdIsActive :: Bool,
    ssdEffectiveFrom :: Day,
    ssdEffectiveUntil :: Maybe Day,
    ssdCreatedAt :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

-- | Show with hosts information
data ShowWithHosts = ShowWithHosts
  { swhShow :: ShowDomain,
    swhHosts :: [ShowHostWithUser]
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance ShowWithHosts)
  deriving anyclass (FromJSON, ToJSON)

-- | Show host with user information
data ShowHostWithUser = ShowHostWithUser
  { shwuHost :: ShowHostDomain,
    shwuUser :: UserMetadata.Domain
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance ShowHostWithUser)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Conversion Functions

toDomainShow :: ShowModel -> ShowDomain
toDomainShow ShowModel {..} =
  ShowDomain
    { sdId = smId,
      sdTitle = smTitle,
      sdSlug = smSlug,
      sdDescription = smDescription,
      sdGenre = smGenre,
      sdLogoUrl = smLogoUrl,
      sdBannerUrl = smBannerUrl,
      sdStatus = smStatus,
      sdFrequency = smFrequency,
      sdDurationMinutes = smDurationMinutes,
      sdCreatedAt = smCreatedAt,
      sdUpdatedAt = smUpdatedAt
    }

toDomainShowHost :: ShowHostModel -> ShowHostDomain
toDomainShowHost ShowHostModel {..} =
  ShowHostDomain
    { shdShowId = shmShowId,
      shdUserId = shmUserId,
      shdRole = shmRole,
      shdIsPrimary = shmIsPrimary,
      shdJoinedAt = shmJoinedAt,
      shdLeftAt = shmLeftAt
    }

toDomainShowSchedule :: ShowScheduleModel -> ShowScheduleDomain
toDomainShowSchedule ShowScheduleModel {..} =
  ShowScheduleDomain
    { ssdId = ssmId,
      ssdShowId = ssmShowId,
      ssdDayOfWeek = ssmDayOfWeek,
      ssdStartTime = ssmStartTime,
      ssdEndTime = ssmEndTime,
      ssdTimezone = ssmTimezone,
      ssdIsActive = ssmIsActive,
      ssdEffectiveFrom = ssmEffectiveFrom,
      ssdEffectiveUntil = ssmEffectiveUntil,
      ssdCreatedAt = ssmCreatedAt
    }

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

-- | Get shows by status
getShowsByStatus :: ShowStatus -> Hasql.Statement () [ShowModel]
getShowsByStatus status =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, frequency, duration_minutes, created_at, updated_at
    FROM shows
    WHERE status = #{status}
    ORDER BY title
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

-- | Check if user is host of show (TODO: Fix Bool DecodeRow issue)
isUserHostOfShow :: User.Id -> ShowId -> Hasql.Statement () (OneColumn Bool)
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
   in fromMaybe (OneColumn False) <$> query

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
