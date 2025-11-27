{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.Shows where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..), display, displayBuilder)
import Data.Time (UTCTime)
import Domain.Types.Genre (Genre)
import Domain.Types.Limit (Limit)
import Domain.Types.Search (Search)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.HostDetails qualified as HostDetails
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Show Status Type

data Status = Active | Inactive
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance Servant.ToHttpApiData Status where
  toQueryParam = \case
    Active -> "active"
    Inactive -> "inactive"

instance Servant.FromHttpApiData Status where
  parseQueryParam = maybe (Left "Invalid Status") Right . decodeStatus

instance Display Status where
  displayBuilder Active = "active"
  displayBuilder Inactive = "inactive"

instance DecodeValue Status where
  decodeValue = Decoders.enum decodeStatus

decodeStatus :: Text -> Maybe Status
decodeStatus = \case
  "active" -> Just Active
  "inactive" -> Just Inactive
  _ -> Nothing

instance EncodeValue Status where
  encodeValue = Encoders.enum $ \case
    Active -> "active"
    Inactive -> "inactive"

--------------------------------------------------------------------------------
-- Database Models

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

data Model = Model
  { id :: Id,
    title :: Text,
    slug :: Slug,
    description :: Text,
    genre :: Maybe Text,
    logoUrl :: Maybe Text,
    bannerUrl :: Maybe Text,
    status :: Status,
    durationMinutes :: Maybe Int64,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

data Insert = Insert
  { siTitle :: Text,
    siSlug :: Slug,
    siDescription :: Text,
    siGenre :: Maybe Text,
    siLogoUrl :: Maybe Text,
    siBannerUrl :: Maybe Text,
    siStatus :: Status,
    siDurationMinutes :: Maybe Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Database Queries

-- | Get all active shows ordered by title
getActiveShows :: Hasql.Statement () [Model]
getActiveShows =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, duration_minutes, created_at, updated_at
    FROM shows
    WHERE status = 'active'
    ORDER BY title
  |]

-- | Get show by slug
getShowBySlug :: Slug -> Hasql.Statement () (Maybe Model)
getShowBySlug slug =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, duration_minutes, created_at, updated_at
    FROM shows
    WHERE slug = #{slug}
  |]

-- | Get show by ID
getShowById :: Id -> Hasql.Statement () (Maybe Model)
getShowById showId =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, duration_minutes, created_at, updated_at
    FROM shows
    WHERE id = #{showId}
  |]

-- | Get shows by status with pagination
getShowsByStatus :: Text -> Limit -> Int64 -> Hasql.Statement () [Model]
getShowsByStatus status limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, duration_minutes, created_at, updated_at
    FROM shows
    WHERE status = #{status}
    ORDER BY title
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get all shows with pagination
getAllShows :: Limit -> Int64 -> Hasql.Statement () [Model]
getAllShows limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, duration_minutes, created_at, updated_at
    FROM shows
    ORDER BY title
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get shows by genre with pagination
getShowsByGenre :: Genre -> Limit -> Int64 -> Hasql.Statement () [Model]
getShowsByGenre genre limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, duration_minutes, created_at, updated_at
    FROM shows
    WHERE genre = #{display genre}
    ORDER BY title
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get shows by genre and status with pagination
getShowsByGenreAndStatus :: Genre -> Status -> Limit -> Int64 -> Hasql.Statement () [Model]
getShowsByGenreAndStatus genre status limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, duration_minutes, created_at, updated_at
    FROM shows
    WHERE genre = #{genre} AND status = #{status}
    ORDER BY title
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Insert a new show
insertShow :: Insert -> Hasql.Statement () Id
insertShow Insert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO shows(title, slug, description, genre, logo_url, banner_url, status, duration_minutes, created_at, updated_at)
    VALUES (#{siTitle}, #{siSlug}, #{siDescription}, #{siGenre}, #{siLogoUrl}, #{siBannerUrl}, #{siStatus}, #{siDurationMinutes}, NOW(), NOW())
    RETURNING id
  |]

-- | Update a show
updateShow :: Id -> Insert -> Hasql.Statement () (Maybe Id)
updateShow showId Insert {..} =
  interp
    False
    [sql|
    UPDATE shows
    SET title = #{siTitle}, slug = #{siSlug}, description = #{siDescription},
        genre = #{siGenre}, logo_url = #{siLogoUrl}, banner_url = #{siBannerUrl},
        status = #{siStatus}, duration_minutes = #{siDurationMinutes},
        updated_at = NOW()
    WHERE id = #{showId}
    RETURNING id
  |]

--------------------------------------------------------------------------------
-- Show Host Queries (Junction table queries stay here)

-- | Get show by slug
getShowForUser :: User.Id -> Hasql.Statement () (Maybe Model)
getShowForUser userId =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, duration_minutes, created_at, updated_at
    FROM shows s
    JOIN show_hosts sh ON s.id = sh.show_id
    WHERE sh.user_id = #{userId} AND sh.left_at IS NULL
    LIMIT 1
  |]

-- | Get shows for a user (active host assignments)
getShowsForUser :: User.Id -> Hasql.Statement () [Model]
getShowsForUser userId =
  interp
    False
    [sql|
    SELECT s.id, s.title, s.slug, s.description, s.genre, s.logo_url, s.banner_url, s.status, s.duration_minutes, s.created_at, s.updated_at
    FROM shows s
    JOIN show_hosts sh ON s.id = sh.show_id
    WHERE sh.user_id = #{userId} AND sh.left_at IS NULL
    ORDER BY s.title
  |]

-- | Get all active shows (for admin dashboard)
getAllActiveShows :: Hasql.Statement () [Model]
getAllActiveShows =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, duration_minutes, created_at, updated_at
    FROM shows
    WHERE status = 'active'
    ORDER BY title
  |]

-- | Search shows by text query with pagination
searchShows :: Search -> Limit -> Int64 -> Hasql.Statement () [Model]
searchShows searchTerm limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, duration_minutes, created_at, updated_at
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
getHostsForShow :: Id -> Hasql.Statement () [HostDetails.Model]
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
