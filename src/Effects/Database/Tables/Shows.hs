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
import Domain.Types.Offset (Offset)
import Domain.Types.Search (Search)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow (..), DecodeValue (..), EncodeRow, EncodeValue (..), OneRow (..), interp, sql)
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
    siStatus :: Status
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
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, created_at, updated_at
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
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, created_at, updated_at
    FROM shows
    WHERE slug = #{slug}
  |]

-- | Get show by ID
getShowById :: Id -> Hasql.Statement () (Maybe Model)
getShowById showId =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, created_at, updated_at
    FROM shows
    WHERE id = #{showId}
  |]

-- | Get shows by status with pagination
getShowsByStatus :: Text -> Limit -> Offset -> Hasql.Statement () [Model]
getShowsByStatus status limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, created_at, updated_at
    FROM shows
    WHERE status = #{status}
    ORDER BY title
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get all shows with pagination
getAllShows :: Limit -> Offset -> Hasql.Statement () [Model]
getAllShows limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, created_at, updated_at
    FROM shows
    ORDER BY title
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get shows by genre with pagination
getShowsByGenre :: Genre -> Limit -> Offset -> Hasql.Statement () [Model]
getShowsByGenre genre limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, created_at, updated_at
    FROM shows
    WHERE genre = #{display genre}
    ORDER BY title
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get shows by genre and status with pagination
getShowsByGenreAndStatus :: Genre -> Status -> Limit -> Offset -> Hasql.Statement () [Model]
getShowsByGenreAndStatus genre status limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, created_at, updated_at
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
    INSERT INTO shows(title, slug, description, genre, logo_url, banner_url, status, created_at, updated_at)
    VALUES (#{siTitle}, #{siSlug}, #{siDescription}, #{siGenre}, #{siLogoUrl}, #{siBannerUrl}, #{siStatus}, NOW(), NOW())
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
        status = #{siStatus},
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
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, created_at, updated_at
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
    SELECT s.id, s.title, s.slug, s.description, s.genre, s.logo_url, s.banner_url, s.status, s.created_at, s.updated_at
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
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, created_at, updated_at
    FROM shows
    WHERE status = 'active'
    ORDER BY title
  |]

-- | Search shows by text query with pagination
searchShows :: Search -> Limit -> Offset -> Hasql.Statement () [Model]
searchShows searchTerm limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, created_at, updated_at
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

--------------------------------------------------------------------------------
-- Admin Queries

-- | Show with aggregated host information for admin listing.
--
-- This is a flat structure (not nested) to allow automatic DecodeRow deriving.
data ShowWithHostInfo = ShowWithHostInfo
  { swhiId :: Id,
    swhiTitle :: Text,
    swhiSlug :: Slug,
    swhiDescription :: Text,
    swhiGenre :: Maybe Text,
    swhiLogoUrl :: Maybe Text,
    swhiBannerUrl :: Maybe Text,
    swhiStatus :: Status,
    swhiCreatedAt :: UTCTime,
    swhiUpdatedAt :: UTCTime,
    swhiHostCount :: Int64,
    swhiHostNames :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance ShowWithHostInfo)

-- | Get all shows with host count and host names for admin listing.
--
-- Returns shows with aggregated host information, ordered by creation date.
-- Only counts active hosts (where left_at IS NULL).
getAllShowsWithHostInfo :: Limit -> Offset -> Hasql.Statement () [ShowWithHostInfo]
getAllShowsWithHostInfo limit offset =
  interp
    False
    [sql|
    SELECT
      s.id, s.title, s.slug, s.description, s.genre, s.logo_url, s.banner_url,
      s.status, s.created_at, s.updated_at,
      COUNT(DISTINCT sh.user_id) FILTER (WHERE sh.left_at IS NULL)::bigint as host_count,
      STRING_AGG(DISTINCT um.display_name, ', ' ORDER BY um.display_name) FILTER (WHERE sh.left_at IS NULL) as host_names
    FROM shows s
    LEFT JOIN show_hosts sh ON s.id = sh.show_id
    LEFT JOIN user_metadata um ON sh.user_id = um.user_id
    GROUP BY s.id
    ORDER BY s.created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get shows filtered by status with host info for admin listing.
getShowsByStatusWithHostInfo :: Status -> Limit -> Offset -> Hasql.Statement () [ShowWithHostInfo]
getShowsByStatusWithHostInfo status limit offset =
  interp
    False
    [sql|
    SELECT
      s.id, s.title, s.slug, s.description, s.genre, s.logo_url, s.banner_url,
      s.status, s.created_at, s.updated_at,
      COUNT(DISTINCT sh.user_id) FILTER (WHERE sh.left_at IS NULL)::bigint as host_count,
      STRING_AGG(DISTINCT um.display_name, ', ' ORDER BY um.display_name) FILTER (WHERE sh.left_at IS NULL) as host_names
    FROM shows s
    LEFT JOIN show_hosts sh ON s.id = sh.show_id
    LEFT JOIN user_metadata um ON sh.user_id = um.user_id
    WHERE s.status = #{status}
    GROUP BY s.id
    ORDER BY s.created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Search shows with host info for admin listing.
searchShowsWithHostInfo :: Search -> Limit -> Offset -> Hasql.Statement () [ShowWithHostInfo]
searchShowsWithHostInfo searchTerm limit offset =
  interp
    False
    [sql|
    SELECT
      s.id, s.title, s.slug, s.description, s.genre, s.logo_url, s.banner_url,
      s.status, s.created_at, s.updated_at,
      COUNT(DISTINCT sh.user_id) FILTER (WHERE sh.left_at IS NULL)::bigint as host_count,
      STRING_AGG(DISTINCT um.display_name, ', ' ORDER BY um.display_name) FILTER (WHERE sh.left_at IS NULL) as host_names
    FROM shows s
    LEFT JOIN show_hosts sh ON s.id = sh.show_id
    LEFT JOIN user_metadata um ON sh.user_id = um.user_id
    WHERE (s.title ILIKE #{searchPattern} OR s.description ILIKE #{searchPattern} OR s.genre ILIKE #{searchPattern})
    GROUP BY s.id
    ORDER BY
      CASE
        WHEN s.title ILIKE #{searchPattern} THEN 1
        WHEN s.description ILIKE #{searchPattern} THEN 2
        ELSE 3
      END,
      s.title
    LIMIT #{limit} OFFSET #{offset}
  |]
  where
    searchPattern = "%" <> searchTerm <> "%"
