{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Database table definition and queries for @shows@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex joins.
module Effects.Database.Tables.Shows
  ( -- * Status Type
    Status (..),

    -- * Id Type
    Id (..),

    -- * Table Definition
    Show (..),
    showSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getActiveShows,
    getShowBySlug,
    getShowById,
    getShowsByStatus,
    getAllShows,
    getShowsByGenre,
    getShowsByGenreAndStatus,
    getShowsFiltered,
    insertShow,
    updateShow,

    -- * Show Host Queries
    getShowForUser,
    getShowsForUser,
    getAllActiveShows,
    searchShows,

    -- * Admin Queries
    ShowWithHostInfo (..),
    getAllShowsWithHostInfo,
    getShowsByStatusWithHostInfo,
    searchShowsWithHostInfo,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..), RecordInstance (..))
import Data.Time (UTCTime)
import Domain.Types.Genre (Genre (..))
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Domain.Types.Search (Search (..))
import Domain.Types.ShowSortBy (ShowSortBy (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow (..), DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import OrphanInstances.UTCTime ()
import Rel8 hiding (Insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified
import Prelude hiding (Show, id)
import Prelude qualified

--------------------------------------------------------------------------------
-- Show Status Type

-- | Show publication status.
data Status = Active | Inactive
  deriving stock (Generic, Prelude.Show, Eq, Ord, Prelude.Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance DBType Status where
  typeInformation =
    parseTypeInformation
      ( \case
          "active" -> Right Active
          "inactive" -> Right Inactive
          other -> Left $ "Invalid Status: " <> Text.unpack other
      )
      ( \case
          Active -> "active"
          Inactive -> "inactive"
      )
      typeInformation

instance DBEq Status

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
-- Id Type

-- | Newtype wrapper for show primary keys.
--
-- Provides type safety to prevent mixing up IDs from different tables.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Prelude.Show, Eq, Ord, Num, DBType, DBEq, DBOrd)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Table Definition

-- | The @shows@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data Show f = Show
  { id :: Column f Id,
    title :: Column f Text,
    slug :: Column f Slug,
    description :: Column f Text,
    genre :: Column f (Maybe Text),
    logoUrl :: Column f (Maybe Text),
    bannerUrl :: Column f (Maybe Text),
    status :: Column f Status,
    createdAt :: Column f UTCTime,
    updatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Prelude.Show (Show f)

deriving stock instance (f ~ Result) => Eq (Show f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (Show Result)

-- | Display instance for Show Result.
instance Display (Show Result) where
  displayBuilder s =
    "Show { id = "
      <> displayBuilder (id s)
      <> ", title = "
      <> displayBuilder (title s)
      <> ", slug = "
      <> displayBuilder (slug s)
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @Show Result@.
type Model = Show Result

-- | Table schema connecting the Haskell type to the database table.
showSchema :: TableSchema (Show Name)
showSchema =
  TableSchema
    { name = "shows",
      columns =
        Show
          { id = "id",
            title = "title",
            slug = "slug",
            description = "description",
            genre = "genre",
            logoUrl = "logo_url",
            bannerUrl = "banner_url",
            status = "status",
            createdAt = "created_at",
            updatedAt = "updated_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new shows.
data Insert = Insert
  { siTitle :: Text,
    siSlug :: Slug,
    siDescription :: Text,
    siGenre :: Maybe Text,
    siLogoUrl :: Maybe Text,
    siBannerUrl :: Maybe Text,
    siStatus :: Status
  }
  deriving stock (Generic, Prelude.Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Queries

-- | Get all active shows ordered by title.
getActiveShows :: Hasql.Statement () [Model]
getActiveShows =
  run $
    select $
      orderBy (title >$< asc) do
        s <- each showSchema
        where_ $ status s ==. lit Active
        pure s

-- | Get show by slug.
getShowBySlug :: Slug -> Hasql.Statement () (Maybe Model)
getShowBySlug showSlug = fmap listToMaybe $ run $ select do
  s <- each showSchema
  where_ $ slug s ==. lit showSlug
  pure s

-- | Get show by ID.
getShowById :: Id -> Hasql.Statement () (Maybe Model)
getShowById showId = fmap listToMaybe $ run $ select do
  s <- each showSchema
  where_ $ id s ==. lit showId
  pure s

-- | Get shows by status with pagination.
getShowsByStatus :: Text -> Limit -> Offset -> Hasql.Statement () [Model]
getShowsByStatus statusText (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, created_at, updated_at
    FROM shows
    WHERE status = #{statusText}
    ORDER BY title
    LIMIT #{lim} OFFSET #{off}
  |]

-- | Get all shows with pagination.
getAllShows :: Limit -> Offset -> Hasql.Statement () [Model]
getAllShows (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy (title >$< asc) do
            each showSchema

-- | Get shows by genre with pagination.
getShowsByGenre :: Genre -> Limit -> Offset -> Hasql.Statement () [Model]
getShowsByGenre (Genre genreText) (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy (title >$< asc) do
            s <- each showSchema
            where_ $ genre s ==. lit (Just genreText)
            pure s

-- | Get shows by genre and status with pagination.
getShowsByGenreAndStatus :: Genre -> Status -> Limit -> Offset -> Hasql.Statement () [Model]
getShowsByGenreAndStatus (Genre genreText) showStatus (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy (title >$< asc) do
            s <- each showSchema
            where_ $ genre s ==. lit (Just genreText)
            where_ $ status s ==. lit showStatus
            pure s

-- | Get shows with optional genre/status filters and configurable sorting.
--
-- This is the primary query for the public shows listing page.
-- Supports sorting by name (A-Z, Z-A) or creation date (newest, oldest).
getShowsFiltered :: Maybe Genre -> Maybe Status -> ShowSortBy -> Limit -> Offset -> Hasql.Statement () [Model]
getShowsFiltered maybeGenre maybeStatus sortBy (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT id, title, slug, description, genre, logo_url, banner_url, status, created_at, updated_at
    FROM shows
    WHERE (#{maybeGenreText}::text IS NULL OR genre = #{maybeGenreText}::text)
      AND (#{maybeStatusText}::show_status IS NULL OR status = #{maybeStatusText}::show_status)
    ORDER BY
      CASE WHEN #{sortOrder} = 'name_az' THEN title END ASC,
      CASE WHEN #{sortOrder} = 'name_za' THEN title END DESC,
      CASE WHEN #{sortOrder} = 'created_newest' THEN created_at END DESC,
      CASE WHEN #{sortOrder} = 'created_oldest' THEN created_at END ASC
    LIMIT #{lim} OFFSET #{off}
  |]
  where
    maybeGenreText :: Maybe Text
    maybeGenreText = fmap (\(Genre g) -> g) maybeGenre

    maybeStatusText :: Maybe Text
    maybeStatusText = fmap statusToText maybeStatus

    statusToText :: Status -> Text
    statusToText Active = "active"
    statusToText Inactive = "inactive"

    sortOrder :: Text
    sortOrder = case sortBy of
      NameAZ -> "name_az"
      NameZA -> "name_za"
      CreatedNewest -> "created_newest"
      CreatedOldest -> "created_oldest"

-- | Insert a new show.
insertShow :: Insert -> Hasql.Statement () Id
insertShow Insert {..} =
  fmap head $
    run $
      insert
        Rel8.Insert
          { into = showSchema,
            rows =
              values
                [ Show
                    { id = coerce (nextval "shows_id_seq"),
                      title = lit siTitle,
                      slug = lit siSlug,
                      description = lit siDescription,
                      genre = lit siGenre,
                      logoUrl = lit siLogoUrl,
                      bannerUrl = lit siBannerUrl,
                      status = lit siStatus,
                      createdAt = now,
                      updatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning id
          }

-- | Update a show.
--
-- Uses raw SQL for consistency with update patterns.
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
-- Show Host Queries (Junction table queries - use raw SQL)

-- | Get show for a user (first active host assignment).
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

-- | Get shows for a user (active host assignments).
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

-- | Get all active shows (for admin dashboard).
getAllActiveShows :: Hasql.Statement () [Model]
getAllActiveShows =
  run $
    select $
      orderBy (title >$< asc) do
        s <- each showSchema
        where_ $ status s ==. lit Active
        pure s

-- | Search shows by text query with pagination.
--
-- Uses raw SQL because of ILIKE pattern matching and complex ordering.
searchShows :: Search -> Limit -> Offset -> Hasql.Statement () [Model]
searchShows (Search searchTerm) (Limit lim) (Offset off) =
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
    LIMIT #{lim + 1} OFFSET #{off}
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
  deriving stock (Generic, Prelude.Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance ShowWithHostInfo)

-- | Get all shows with host count and host names for admin listing.
--
-- Returns shows with aggregated host information, ordered by creation date.
-- Only counts active hosts (where left_at IS NULL).
getAllShowsWithHostInfo :: Limit -> Offset -> Hasql.Statement () [ShowWithHostInfo]
getAllShowsWithHostInfo (Limit lim) (Offset off) =
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
    LIMIT #{lim} OFFSET #{off}
  |]

-- | Get shows filtered by status with host info for admin listing.
getShowsByStatusWithHostInfo :: Status -> Limit -> Offset -> Hasql.Statement () [ShowWithHostInfo]
getShowsByStatusWithHostInfo showStatus (Limit lim) (Offset off) =
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
    WHERE s.status = #{showStatus}
    GROUP BY s.id
    ORDER BY s.created_at DESC
    LIMIT #{lim} OFFSET #{off}
  |]

-- | Search shows with host info for admin listing.
searchShowsWithHostInfo :: Search -> Limit -> Offset -> Hasql.Statement () [ShowWithHostInfo]
searchShowsWithHostInfo (Search searchTerm) (Limit lim) (Offset off) =
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
    LIMIT #{lim} OFFSET #{off}
  |]
  where
    searchPattern = "%" <> searchTerm <> "%"
