{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Database table definition and queries for @show_tags@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex joins.
-- Follows the same pattern as 'ShowBlogTags'.
module Effects.Database.Tables.ShowTags
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    ShowTag (..),
    showTagSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Result Types
    ShowTagWithCount (..),

    -- * Queries
    getShowTagByName,
    insertShowTag,
    getShowTagsWithCounts,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
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

-- | Newtype wrapper for show tag primary keys.
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

-- | The @show_tags@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data ShowTag f = ShowTag
  { stId :: Column f Id,
    stName :: Column f Text,
    stCreatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (ShowTag f)

deriving stock instance (f ~ Result) => Eq (ShowTag f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (ShowTag Result)

-- | Display instance for ShowTag Result.
instance Display (ShowTag Result) where
  displayBuilder tag =
    "ShowTag { id = "
      <> displayBuilder (stId tag)
      <> ", name = "
      <> displayBuilder (stName tag)
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @ShowTag Result@.
type Model = ShowTag Result

-- | Table schema connecting the Haskell type to the database table.
showTagSchema :: TableSchema (ShowTag Name)
showTagSchema =
  TableSchema
    { name = "show_tags",
      columns =
        ShowTag
          { stId = "id",
            stName = "name",
            stCreatedAt = "created_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new show tags.
newtype Insert = Insert {stiName :: Text}
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Result Types

-- | Show tag with count of active shows using it.
--
-- Used for the filter UI to show only tags that have associated shows.
data ShowTagWithCount = ShowTagWithCount
  { stwcId :: Id,
    stwcName :: Text,
    stwcCreatedAt :: UTCTime,
    stwcCount :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

-- | Display instance for ShowTagWithCount.
instance Display ShowTagWithCount where
  displayBuilder tag =
    "ShowTagWithCount { id = "
      <> displayBuilder (stwcId tag)
      <> ", name = "
      <> displayBuilder (stwcName tag)
      <> ", count = "
      <> displayBuilder (stwcCount tag)
      <> " }"

--------------------------------------------------------------------------------
-- Queries

-- | Get show tag by name.
getShowTagByName :: Text -> Hasql.Statement () (Maybe Model)
getShowTagByName tagName = fmap listToMaybe $ run $ select do
  tag <- each showTagSchema
  where_ $ stName tag ==. lit tagName
  pure tag

-- | Insert a new show tag and return its ID.
insertShowTag :: Insert -> Hasql.Statement () Id
insertShowTag Insert {..} =
  fmap head $
    run $
      insert
        Rel8.Insert
          { into = showTagSchema,
            rows =
              values
                [ ShowTag
                    { stId = coerce (nextval "show_tags_id_seq"),
                      stName = lit stiName,
                      stCreatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning stId
          }

-- | Get all show tags with counts of active shows using each tag.
--
-- Only returns tags that have at least one active show associated.
-- Excludes soft-deleted shows.
-- Ordered by count descending, then name ascending.
getShowTagsWithCounts :: Hasql.Statement () [ShowTagWithCount]
getShowTagsWithCounts =
  interp
    False
    [sql|
      SELECT
        st.id,
        st.name,
        st.created_at,
        COUNT(sta.show_id)::bigint as count
      FROM show_tags st
      INNER JOIN show_tag_assignments sta ON st.id = sta.tag_id
      INNER JOIN shows s ON sta.show_id = s.id
      WHERE s.status = 'active' AND s.deleted_at IS NULL
      GROUP BY st.id, st.name, st.created_at
      HAVING COUNT(sta.show_id) > 0
      ORDER BY COUNT(sta.show_id) DESC, st.name ASC
    |]
