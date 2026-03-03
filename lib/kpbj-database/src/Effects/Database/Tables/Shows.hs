{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @shows@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex joins.
module Effects.Database.Tables.Shows
  ( -- * Status Type
    Status (..),
    decodeStatus,

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
    getShowBySlug,
    getShowById,
    getShowsFiltered,
    insertShow,
    updateShow,

    -- * Show Host Queries
    getShowsForUser,
    getAllActiveShows,
    searchShows,

    -- * Soft Delete
    softDeleteShow,

    -- * Admin Queries
    ShowWithHostInfo (..),
    getAllShowsWithHostInfo,
    getShowsByStatusWithHostInfo,
    searchShowsWithHostInfo,

    -- * Tag Junction Queries
    getTagsForShow,
    addTagToShow,
    removeAllTagsFromShow,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..), RecordInstance (..))
import Data.Time (UTCTime)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Domain.Types.Search (Search (..))
import Domain.Types.ShowSortBy (ShowSortBy (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.Util (nextId)
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
    description :: Column f (Maybe Text),
    logoUrl :: Column f (Maybe Text),
    status :: Column f Status,
    createdAt :: Column f UTCTime,
    updatedAt :: Column f UTCTime,
    deletedAt :: Column f (Maybe UTCTime)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Prelude.Show (Show f)

deriving stock instance (f ~ Result) => Eq (Show f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (Show Result)

-- | Display instance for Show Result.
instance Display (Show Result) where
  displayBuilder showRec =
    "Show { id = "
      <> displayBuilder (id showRec {- HLINT ignore "Redundant id" -})
      <> ", title = "
      <> displayBuilder (title showRec)
      <> ", slug = "
      <> displayBuilder (slug showRec)
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
            logoUrl = "logo_url",
            status = "status",
            createdAt = "created_at",
            updatedAt = "updated_at",
            deletedAt = "deleted_at"
          }
    }

--------------------------------------------------------------------------------
-- Junction Table (show_tag_assignments)

-- | The @show_tag_assignments@ junction table definition (internal, not exported).
data ShowTagAssignment f = ShowTagAssignment
  { staShowId :: Column f Id,
    staTagId :: Column f ShowTags.Id
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- | Schema for the @show_tag_assignments@ junction table.
showTagAssignmentSchema :: TableSchema (ShowTagAssignment Name)
showTagAssignmentSchema =
  TableSchema
    { name = "show_tag_assignments",
      columns =
        ShowTagAssignment
          { staShowId = "show_id",
            staTagId = "tag_id"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new shows.
data Insert = Insert
  { siTitle :: Text,
    siSlug :: Slug,
    siDescription :: Maybe Text,
    siLogoUrl :: Maybe Text,
    siStatus :: Status
  }
  deriving stock (Generic, Prelude.Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Queries

-- | Get show by slug (excludes soft-deleted shows).
getShowBySlug :: Slug -> Hasql.Statement () (Maybe Model)
getShowBySlug showSlug = fmap listToMaybe $ run $ select do
  s <- each showSchema
  where_ $ slug s ==. lit showSlug
  where_ $ isNull (deletedAt s)
  pure s

-- | Get show by ID (excludes soft-deleted shows).
getShowById :: Id -> Hasql.Statement () (Maybe Model)
getShowById showId = fmap listToMaybe $ run $ select do
  showRec <- each showSchema
  where_ $ id showRec ==. lit showId {- HLINT ignore "Redundant id" -}
  where_ $ isNull (deletedAt showRec)
  pure showRec

-- | Get shows with optional tag/status filters and configurable sorting.
--
-- This is the primary query for the public shows listing page.
-- Supports sorting by name (A-Z, Z-A) or creation date (newest, oldest).
-- When a tag filter is provided, uses JOIN to filter by tag assignment.
-- Excludes soft-deleted shows.
getShowsFiltered :: Maybe ShowTags.Id -> Maybe Status -> ShowSortBy -> Limit -> Offset -> Hasql.Statement () [Model]
getShowsFiltered maybeTagId maybeStatus sortBy (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT s.id, s.title, s.slug, s.description, s.logo_url, s.status, s.created_at, s.updated_at, s.deleted_at
    FROM shows s
    WHERE s.deleted_at IS NULL
      AND EXISTS (SELECT 1 FROM show_hosts sh WHERE sh.show_id = s.id AND sh.left_at IS NULL)
      AND (#{maybeTagIdInt}::bigint IS NULL OR EXISTS (
        SELECT 1 FROM show_tag_assignments sta WHERE sta.show_id = s.id AND sta.tag_id = #{maybeTagIdInt}::bigint
      ))
      AND (#{maybeStatusText}::show_status IS NULL OR s.status = #{maybeStatusText}::show_status)
    ORDER BY
      CASE WHEN #{sortOrder} = 'name_az' THEN s.title END ASC,
      CASE WHEN #{sortOrder} = 'name_za' THEN s.title END DESC,
      CASE WHEN #{sortOrder} = 'created_newest' THEN s.created_at END DESC,
      CASE WHEN #{sortOrder} = 'created_oldest' THEN s.created_at END ASC
    LIMIT #{lim} OFFSET #{off}
  |]
  where
    maybeTagIdInt :: Maybe Int64
    maybeTagIdInt = fmap (\(ShowTags.Id tid) -> tid) maybeTagId

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
insertShow :: Insert -> Hasql.Statement () (Maybe Id)
insertShow Insert {..} =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = showSchema,
            rows =
              values
                [ Show
                    { id = nextId "shows_id_seq",
                      title = lit siTitle,
                      slug = lit siSlug,
                      description = lit siDescription,
                      logoUrl = lit siLogoUrl,
                      status = lit siStatus,
                      createdAt = now,
                      updatedAt = now,
                      deletedAt = Rel8.null
                    }
                ],
            onConflict = Abort,
            returning = Returning id
          }

-- | Update a show.
updateShow :: Id -> Insert -> Hasql.Statement () (Maybe Id)
updateShow showId Insert {..} =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = showSchema,
            from = pure (),
            set = \_ showRec ->
              showRec
                { title = lit siTitle,
                  slug = lit siSlug,
                  description = lit siDescription,
                  logoUrl = lit siLogoUrl,
                  status = lit siStatus,
                  updatedAt = now
                },
            updateWhere = \_ showRec -> id showRec ==. lit showId {- HLINT ignore "Redundant id" -},
            returning = Returning id
          }

-- | Soft delete a show by setting deleted_at timestamp.
--
-- Returns the show ID if successfully deleted, Nothing if already deleted or not found.
softDeleteShow :: Id -> Hasql.Statement () (Maybe Id)
softDeleteShow showId =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = showSchema,
            from = pure (),
            set = \_ showRec ->
              showRec
                { deletedAt = nullify now
                },
            updateWhere = \_ showRec ->
              id showRec ==. lit showId {- HLINT ignore "Redundant id" -}
                &&. isNull (deletedAt showRec),
            returning = Returning id
          }

--------------------------------------------------------------------------------
-- Show Host Queries

-- | Get shows for a user (active host assignments, excludes soft-deleted shows).
getShowsForUser :: User.Id -> Hasql.Statement () [Model]
getShowsForUser userId =
  run $
    select $
      orderBy (title >$< asc) do
        s <- each showSchema
        sh <- each ShowHost.showHostSchema
        where_ $ id s ==. ShowHost.shmShowId sh {- HLINT ignore "Redundant id" -}
        where_ $ ShowHost.shmUserId sh ==. lit userId
        where_ $ isNull (ShowHost.shmLeftAt sh)
        where_ $ isNull (deletedAt s)
        pure s

-- | Get all active shows (for admin dashboard, excludes soft-deleted shows).
getAllActiveShows :: Hasql.Statement () [Model]
getAllActiveShows =
  run $
    select $
      orderBy (title >$< asc) do
        s <- each showSchema
        where_ $ status s ==. lit Active
        where_ $ isNull (deletedAt s)
        pure s

-- | Search shows by text query with pagination.
--
-- Uses raw SQL because of ILIKE pattern matching and complex ordering.
-- Searches title and description fields. Excludes soft-deleted shows.
searchShows :: Search -> Limit -> Offset -> Hasql.Statement () [Model]
searchShows (Search searchTerm) (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT s.id, s.title, s.slug, s.description, s.logo_url, s.status, s.created_at, s.updated_at, s.deleted_at
    FROM shows s
    WHERE s.deleted_at IS NULL
      AND EXISTS (SELECT 1 FROM show_hosts sh WHERE sh.show_id = s.id AND sh.left_at IS NULL)
      AND (s.title ILIKE #{searchPattern} OR s.description ILIKE #{searchPattern})
    ORDER BY
      CASE
        WHEN s.title ILIKE #{searchPattern} THEN 1
        WHEN s.description ILIKE #{searchPattern} THEN 2
        ELSE 3
      END,
      s.title
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
    swhiDescription :: Maybe Text,
    swhiLogoUrl :: Maybe Text,
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
-- Only counts active hosts (where left_at IS NULL). Excludes soft-deleted shows.
getAllShowsWithHostInfo :: Limit -> Offset -> Hasql.Statement () [ShowWithHostInfo]
getAllShowsWithHostInfo (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT
      s.id, s.title, s.slug, s.description, s.logo_url,
      s.status, s.created_at, s.updated_at,
      COUNT(DISTINCT sh.user_id) FILTER (WHERE sh.left_at IS NULL)::bigint as host_count,
      STRING_AGG(DISTINCT um.display_name, ', ' ORDER BY um.display_name) FILTER (WHERE sh.left_at IS NULL) as host_names
    FROM shows s
    LEFT JOIN show_hosts sh ON s.id = sh.show_id
    LEFT JOIN user_metadata um ON sh.user_id = um.user_id
    WHERE s.deleted_at IS NULL
    GROUP BY s.id
    ORDER BY s.created_at DESC
    LIMIT #{lim} OFFSET #{off}
  |]

-- | Get shows filtered by status with host info for admin listing.
--
-- Excludes soft-deleted shows.
getShowsByStatusWithHostInfo :: Status -> Limit -> Offset -> Hasql.Statement () [ShowWithHostInfo]
getShowsByStatusWithHostInfo showStatus (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT
      s.id, s.title, s.slug, s.description, s.logo_url,
      s.status, s.created_at, s.updated_at,
      COUNT(DISTINCT sh.user_id) FILTER (WHERE sh.left_at IS NULL)::bigint as host_count,
      STRING_AGG(DISTINCT um.display_name, ', ' ORDER BY um.display_name) FILTER (WHERE sh.left_at IS NULL) as host_names
    FROM shows s
    LEFT JOIN show_hosts sh ON s.id = sh.show_id
    LEFT JOIN user_metadata um ON sh.user_id = um.user_id
    WHERE s.status = #{showStatus} AND s.deleted_at IS NULL
    GROUP BY s.id
    ORDER BY s.created_at DESC
    LIMIT #{lim} OFFSET #{off}
  |]

-- | Search shows with host info for admin listing.
--
-- Excludes soft-deleted shows.
searchShowsWithHostInfo :: Search -> Limit -> Offset -> Hasql.Statement () [ShowWithHostInfo]
searchShowsWithHostInfo (Search searchTerm) (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT
      s.id, s.title, s.slug, s.description, s.logo_url,
      s.status, s.created_at, s.updated_at,
      COUNT(DISTINCT sh.user_id) FILTER (WHERE sh.left_at IS NULL)::bigint as host_count,
      STRING_AGG(DISTINCT um.display_name, ', ' ORDER BY um.display_name) FILTER (WHERE sh.left_at IS NULL) as host_names
    FROM shows s
    LEFT JOIN show_hosts sh ON s.id = sh.show_id
    LEFT JOIN user_metadata um ON sh.user_id = um.user_id
    WHERE s.deleted_at IS NULL
      AND (s.title ILIKE #{searchPattern} OR s.description ILIKE #{searchPattern})
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

--------------------------------------------------------------------------------
-- Tag Junction Queries

-- | Get all tags for a show.
getTagsForShow :: Id -> Hasql.Statement () [ShowTags.Model]
getTagsForShow showId =
  run $
    select $
      orderBy (ShowTags.stName >$< asc) do
        sta <- each showTagAssignmentSchema
        where_ $ staShowId sta ==. lit showId
        tag <- each ShowTags.showTagSchema
        where_ $ ShowTags.stId tag ==. staTagId sta
        pure tag

-- | Add a tag to a show.
--
-- Uses ON CONFLICT DO NOTHING for idempotent behavior.
addTagToShow :: Id -> ShowTags.Id -> Hasql.Statement () ()
addTagToShow showId tagId =
  run_ $
    insert
      Rel8.Insert
        { into = showTagAssignmentSchema,
          rows = values [ShowTagAssignment {staShowId = lit showId, staTagId = lit tagId}],
          onConflict = DoNothing,
          returning = NoReturning
        }

-- | Remove all tag assignments from a show.
--
-- Used when updating a show's tags (clear all, then re-add).
removeAllTagsFromShow :: Id -> Hasql.Statement () ()
removeAllTagsFromShow showId =
  run_ $
    delete
      Rel8.Delete
        { from = showTagAssignmentSchema,
          using = pure (),
          deleteWhere = \_ sta -> staShowId sta ==. lit showId,
          returning = NoReturning
        }
