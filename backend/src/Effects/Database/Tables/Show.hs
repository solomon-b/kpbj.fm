{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.Show
  ( module Effects.Database.Tables.Show,
    module Effects.Database.Tables.ShowHost,
    module Effects.Database.Tables.ShowSchedule,
    module Effects.Database.Tables.HostDetails,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..), display, displayBuilder)
import Data.Time (UTCTime)
import Domain.Types.Genre (Genre)
import Domain.Types.Search (Search)
import Effects.Database.Tables.HostDetails
import Effects.Database.Tables.ShowHost
import Effects.Database.Tables.ShowSchedule
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneRow (..), interp, sql)
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
-- Show Host Queries (Junction table queries stay here)

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

-- | Get host details for all hosts of a show (joins through show_hosts to host_details)
getHostsForShow :: ShowId -> Hasql.Statement () [HostDetailsModel]
getHostsForShow showId =
  interp
    False
    [sql|
    SELECT hd.id, hd.user_id, hd.bio, hd.website_url, hd.instagram_handle, hd.twitter_handle, hd.soundcloud_url, hd.bandcamp_url, hd.created_at, hd.updated_at
    FROM host_details hd
    JOIN show_hosts sh ON hd.user_id = sh.user_id
    WHERE sh.show_id = #{showId} AND sh.left_at IS NULL
    ORDER BY sh.is_primary DESC, sh.joined_at ASC
  |]

--------------------------------------------------------------------------------
-- Backward compatibility aliases

-- | Alias for getSchedulesForShow (backward compatibility)
getShowSchedules :: ShowId -> Hasql.Statement () [ShowScheduleModel]
getShowSchedules = getSchedulesForShow

-- | Alias for getHostDetailsByUserId (backward compatibility)
getHostDetails :: User.Id -> Hasql.Statement () (Maybe HostDetailsModel)
getHostDetails = getHostDetailsByUserId
