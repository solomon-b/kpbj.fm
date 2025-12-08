{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Database table definition and queries for @event_tags@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex joins.
module Effects.Database.Tables.EventTags
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    EventTag (..),
    eventTagSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getAllEventTags,
    getEventTagById,
    getEventTagByName,
    insertEventTag,
    getEventTagsWithCounts,

    -- * Result Types
    EventTagWithCount (..),
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

-- | Newtype wrapper for event tag primary keys.
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

-- | The @event_tags@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data EventTag f = EventTag
  { etmId :: Column f Id,
    etmName :: Column f Text,
    etmCreatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (EventTag f)

deriving stock instance (f ~ Result) => Eq (EventTag f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (EventTag Result)

-- | Display instance for EventTag Result using Show-based formatting.
instance Display (EventTag Result) where
  displayBuilder tag =
    "EventTag { id = "
      <> displayBuilder (etmId tag)
      <> ", name = "
      <> displayBuilder (etmName tag)
      <> ", createdAt = "
      <> displayBuilder (show $ etmCreatedAt tag)
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @EventTag Result@.
type Model = EventTag Result

-- | Table schema connecting the Haskell type to the database table.
eventTagSchema :: TableSchema (EventTag Name)
eventTagSchema =
  TableSchema
    { name = "event_tags",
      columns =
        EventTag
          { etmId = "id",
            etmName = "name",
            etmCreatedAt = "created_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new event tags.
newtype Insert = Insert {etiName :: Text}
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Queries

-- | Get all event tags ordered by name.
getAllEventTags :: Hasql.Statement () [Model]
getAllEventTags = run $ select $ orderBy (etmName >$< asc) do
  each eventTagSchema

-- | Get event tag by ID.
getEventTagById :: Id -> Hasql.Statement () (Maybe Model)
getEventTagById tagId = fmap listToMaybe $ run $ select do
  tag <- each eventTagSchema
  where_ $ etmId tag ==. lit tagId
  pure tag

-- | Get event tag by name.
getEventTagByName :: Text -> Hasql.Statement () (Maybe Model)
getEventTagByName tagName = fmap listToMaybe $ run $ select do
  tag <- each eventTagSchema
  where_ $ etmName tag ==. lit tagName
  pure tag

-- | Insert a new event tag and return its ID.
insertEventTag :: Insert -> Hasql.Statement () Id
insertEventTag Insert {..} =
  -- Safe because INSERT ... RETURNING always returns exactly the rows inserted
  fmap head $
    run $
      insert
        Rel8.Insert
          { into = eventTagSchema,
            rows =
              values
                [ EventTag
                    { etmId = coerce (nextval "event_tags_id_seq"),
                      etmName = lit etiName,
                      etmCreatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning etmId
          }

--------------------------------------------------------------------------------
-- Complex Queries (raw SQL)

-- | Tag with usage count for aggregate queries.
data EventTagWithCount = EventTagWithCount
  { etwcTag :: Text,
    etwcCount :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance EventTagWithCount)

-- | Get event tags with their usage counts (only tags used by published events).
--
-- Uses raw SQL because this query involves multiple joins and aggregation
-- that would be verbose to express in rel8.
getEventTagsWithCounts :: Hasql.Statement () [EventTagWithCount]
getEventTagsWithCounts =
  interp
    False
    [sql|
    SELECT et.name, COUNT(*)::bigint as tag_count
    FROM event_tags et
    JOIN event_tag_assignments eta ON et.id = eta.tag_id
    JOIN events e ON eta.event_id = e.id
    WHERE e.status = 'published'
    GROUP BY et.name
    ORDER BY tag_count DESC, et.name
  |]
