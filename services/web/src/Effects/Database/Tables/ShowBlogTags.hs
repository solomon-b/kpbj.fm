{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Database table definition and queries for @show_blog_tags@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex joins.
module Effects.Database.Tables.ShowBlogTags
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    ShowBlogTag (..),
    showBlogTagSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getShowBlogTagByName,
    insertShowBlogTag,
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
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..))
import Hasql.Statement qualified as Hasql
import Rel8 hiding (Insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for show blog tag primary keys.
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

-- | The @show_blog_tags@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data ShowBlogTag f = ShowBlogTag
  { sbtmId :: Column f Id,
    sbtmName :: Column f Text,
    sbtmCreatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (ShowBlogTag f)

deriving stock instance (f ~ Result) => Eq (ShowBlogTag f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (ShowBlogTag Result)

-- | Display instance for ShowBlogTag Result.
instance Display (ShowBlogTag Result) where
  displayBuilder tag =
    "ShowBlogTag { id = "
      <> displayBuilder (sbtmId tag)
      <> ", name = "
      <> displayBuilder (sbtmName tag)
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @ShowBlogTag Result@.
type Model = ShowBlogTag Result

-- | Table schema connecting the Haskell type to the database table.
showBlogTagSchema :: TableSchema (ShowBlogTag Name)
showBlogTagSchema =
  TableSchema
    { name = "show_blog_tags",
      columns =
        ShowBlogTag
          { sbtmId = "id",
            sbtmName = "name",
            sbtmCreatedAt = "created_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new show blog tags.
newtype Insert = Insert {sbtiName :: Text}
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Queries

-- | Get show blog tag by name.
getShowBlogTagByName :: Text -> Hasql.Statement () (Maybe Model)
getShowBlogTagByName tagName = fmap listToMaybe $ run $ select do
  tag <- each showBlogTagSchema
  where_ $ sbtmName tag ==. lit tagName
  pure tag

-- | Insert a new show blog tag and return its ID.
insertShowBlogTag :: Insert -> Hasql.Statement () Id
insertShowBlogTag Insert {..} =
  fmap head $
    run $
      insert
        Rel8.Insert
          { into = showBlogTagSchema,
            rows =
              values
                [ ShowBlogTag
                    { sbtmId = coerce (nextval "show_blog_tags_id_seq"),
                      sbtmName = lit sbtiName,
                      sbtmCreatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning sbtmId
          }
