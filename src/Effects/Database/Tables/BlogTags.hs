{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Database table definition and queries for @blog_tags@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex joins.
module Effects.Database.Tables.BlogTags
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    BlogTag (..),
    blogTagSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getAllTags,
    getTagByName,
    getTagById,
    insertTag,
    getTagsWithCounts,

    -- * Result Types
    BlogTagWithCount (..),
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

-- | Newtype wrapper for blog tag primary keys.
--
-- Provides type safety to prevent mixing up IDs from different tables.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq, DBOrd)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Table Definition

-- | The @blog_tags@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data BlogTag f = BlogTag
  { btmId :: Column f Id,
    btmName :: Column f Text,
    btmCreatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (BlogTag f)

deriving stock instance (f ~ Result) => Eq (BlogTag f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (BlogTag Result)

-- | Display instance for BlogTag Result using Show-based formatting.
instance Display (BlogTag Result) where
  displayBuilder tag =
    "BlogTag { id = "
      <> displayBuilder (btmId tag)
      <> ", name = "
      <> displayBuilder (btmName tag)
      <> ", createdAt = "
      <> displayBuilder (show $ btmCreatedAt tag)
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @BlogTag Result@.
type Model = BlogTag Result

-- | Table schema connecting the Haskell type to the database table.
blogTagSchema :: TableSchema (BlogTag Name)
blogTagSchema =
  TableSchema
    { name = "blog_tags",
      columns =
        BlogTag
          { btmId = "id",
            btmName = "name",
            btmCreatedAt = "created_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new blog tags.
newtype Insert = Insert {btiName :: Text}
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Queries

-- | Get all tags ordered by name.
getAllTags :: Hasql.Statement () [Model]
getAllTags = run $ select $ orderBy (btmName >$< asc) do
  each blogTagSchema

-- | Get tag by name.
getTagByName :: Text -> Hasql.Statement () (Maybe Model)
getTagByName tagName = fmap listToMaybe $ run $ select do
  tag <- each blogTagSchema
  where_ $ btmName tag ==. lit tagName
  pure tag

-- | Get tag by ID.
getTagById :: Id -> Hasql.Statement () (Maybe Model)
getTagById tagId = fmap listToMaybe $ run $ select do
  tag <- each blogTagSchema
  where_ $ btmId tag ==. lit tagId
  pure tag

-- | Insert a new tag and return its ID.
insertTag :: Insert -> Hasql.Statement () Id
insertTag Insert {..} =
  -- Safe because INSERT ... RETURNING always returns exactly the rows inserted
  fmap head $
    run $
      insert
        Rel8.Insert
          { into = blogTagSchema,
            rows =
              values
                [ BlogTag
                    { btmId = coerce (nextval "blog_tags_id_seq"),
                      btmName = lit btiName,
                      btmCreatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning btmId
          }

--------------------------------------------------------------------------------
-- Complex Queries (raw SQL)

-- | Tag with usage count for aggregate queries.
data BlogTagWithCount = BlogTagWithCount
  { btwcId :: Id,
    btwcName :: Text,
    btwcCreatedAt :: UTCTime,
    btwcCount :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

-- | Get tags with their usage counts (only tags used by published posts).
--
-- Uses raw SQL because this query involves multiple joins and aggregation
-- that would be verbose to express in rel8.
getTagsWithCounts :: Hasql.Statement () [BlogTagWithCount]
getTagsWithCounts =
  interp
    False
    [sql|
    SELECT bt.id, bt.name, bt.created_at, COUNT(bpt.post_id)::bigint as post_count
    FROM blog_tags bt
    LEFT JOIN blog_post_tags bpt ON bt.id = bpt.tag_id
    LEFT JOIN blog_posts bp ON bpt.post_id = bp.id AND bp.status = 'published'
    GROUP BY bt.id, bt.name, bt.created_at
    HAVING COUNT(bpt.post_id) > 0
    ORDER BY COUNT(bpt.post_id) DESC, bt.name
  |]
