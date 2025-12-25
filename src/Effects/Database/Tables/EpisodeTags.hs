{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Database table definition and queries for @episode_tags@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex joins.
-- Follows the same pattern as 'ShowTags'.
module Effects.Database.Tables.EpisodeTags
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    EpisodeTag (..),
    episodeTagSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Result Types
    EpisodeTagWithCount (..),

    -- * Queries
    getAllEpisodeTags,
    getEpisodeTagByName,
    getEpisodeTagById,
    insertEpisodeTag,
    getEpisodeTagsWithCounts,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..), RecordInstance (..))
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Rel8 hiding (Insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for episode tag primary keys.
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

-- | The @episode_tags@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data EpisodeTag f = EpisodeTag
  { etId :: Column f Id,
    etName :: Column f Text,
    etCreatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (EpisodeTag f)

deriving stock instance (f ~ Result) => Eq (EpisodeTag f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (EpisodeTag Result)

-- | Display instance for EpisodeTag Result.
instance Display (EpisodeTag Result) where
  displayBuilder tag =
    "EpisodeTag { id = "
      <> displayBuilder (etId tag)
      <> ", name = "
      <> displayBuilder (etName tag)
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @EpisodeTag Result@.
type Model = EpisodeTag Result

-- | Table schema connecting the Haskell type to the database table.
episodeTagSchema :: TableSchema (EpisodeTag Name)
episodeTagSchema =
  TableSchema
    { name = "episode_tags",
      columns =
        EpisodeTag
          { etId = "id",
            etName = "name",
            etCreatedAt = "created_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new episode tags.
newtype Insert = Insert {etiName :: Text}
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Result Types

-- | Episode tag with count of episodes using it.
--
-- Used for the filter UI to show only tags that have associated episodes.
data EpisodeTagWithCount = EpisodeTagWithCount
  { etwcId :: Id,
    etwcName :: Text,
    etwcCreatedAt :: UTCTime,
    etwcCount :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

-- | Display instance for EpisodeTagWithCount.
instance Display EpisodeTagWithCount where
  displayBuilder tag =
    "EpisodeTagWithCount { id = "
      <> displayBuilder (etwcId tag)
      <> ", name = "
      <> displayBuilder (etwcName tag)
      <> ", count = "
      <> displayBuilder (etwcCount tag)
      <> " }"

--------------------------------------------------------------------------------
-- Queries

-- | Get all episode tags ordered by name.
getAllEpisodeTags :: Hasql.Statement () [Model]
getAllEpisodeTags = run $ select $ orderBy (etName >$< asc) do
  each episodeTagSchema

-- | Get episode tag by name.
getEpisodeTagByName :: Text -> Hasql.Statement () (Maybe Model)
getEpisodeTagByName tagName = fmap listToMaybe $ run $ select do
  tag <- each episodeTagSchema
  where_ $ etName tag ==. lit tagName
  pure tag

-- | Get episode tag by ID.
getEpisodeTagById :: Id -> Hasql.Statement () (Maybe Model)
getEpisodeTagById tagId = fmap listToMaybe $ run $ select do
  tag <- each episodeTagSchema
  where_ $ etId tag ==. lit tagId
  pure tag

-- | Insert a new episode tag and return its ID.
insertEpisodeTag :: Insert -> Hasql.Statement () Id
insertEpisodeTag Insert {..} =
  fmap head $
    run $
      insert
        Rel8.Insert
          { into = episodeTagSchema,
            rows =
              values
                [ EpisodeTag
                    { etId = coerce (nextval "episode_tags_id_seq"),
                      etName = lit etiName,
                      etCreatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning etId
          }

-- | Get all episode tags with counts of published episodes using each tag.
--
-- Only returns tags that have at least one published episode associated.
-- Ordered by count descending, then name ascending.
getEpisodeTagsWithCounts :: Hasql.Statement () [EpisodeTagWithCount]
getEpisodeTagsWithCounts =
  interp
    False
    [sql|
      SELECT
        et.id,
        et.name,
        et.created_at,
        COUNT(eta.episode_id)::bigint as count
      FROM episode_tags et
      INNER JOIN episode_tag_assignments eta ON et.id = eta.tag_id
      INNER JOIN episodes e ON eta.episode_id = e.id
      WHERE e.status = 'published'
      GROUP BY et.id, et.name, et.created_at
      HAVING COUNT(eta.episode_id) > 0
      ORDER BY COUNT(eta.episode_id) DESC, et.name ASC
    |]
