{-# LANGUAGE StandaloneDeriving #-}

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
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display (..), RecordInstance (..))
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..))
import Rel8 (Column, DBEq, DBOrd, DBType, Name, Rel8able, Result, TableSchema (..))
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
