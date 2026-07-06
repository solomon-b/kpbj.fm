{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Database table definition and queries for @event_images@.
--
-- Uses rel8 for simple queries. Mirrors 'Effects.Database.Tables.ProductImages'
-- but links to an event and carries an additional visible @caption@ column.
module Effects.Database.Tables.EventImages
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    EventImage (..),
    eventImageSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getById,
    getByEventId,
    insertImage,
    deleteImage,
    updateSortOrder,
    updateImageMeta,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..))
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for event image primary keys.
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

-- | The @event_images@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data EventImage f = EventImage
  { eviId :: Column f Id,
    eviEventId :: Column f Events.Id,
    eviImagePath :: Column f Text,
    eviCaption :: Column f Text,
    eviAltText :: Column f Text,
    eviSortOrder :: Column f Int64,
    eviCreatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (EventImage f)

deriving stock instance (f ~ Result) => Eq (EventImage f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (EventImage Result)

-- | Display instance for EventImage Result.
instance Display (EventImage Result) where
  displayBuilder image =
    "EventImage { id = "
      <> displayBuilder image.eviId
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @EventImage Result@.
type Model = EventImage Result

-- | Table schema connecting the Haskell type to the database table.
eventImageSchema :: TableSchema (EventImage Name)
eventImageSchema =
  TableSchema
    { name = "event_images",
      columns =
        EventImage
          { eviId = "id",
            eviEventId = "event_id",
            eviImagePath = "image_path",
            eviCaption = "caption",
            eviAltText = "alt_text",
            eviSortOrder = "sort_order",
            eviCreatedAt = "created_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new event images.
data Insert = Insert
  { iiEventId :: Events.Id,
    iiImagePath :: Text,
    iiCaption :: Text,
    iiAltText :: Text,
    iiSortOrder :: Int64
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Get all images for an event, ordered by sort_order.
getByEventId :: Events.Id -> Hasql.Statement () [Model]
getByEventId eventId =
  run $
    select $
      orderBy ((eviSortOrder >$< asc) <> (eviId >$< asc)) do
        image <- each eventImageSchema
        where_ $ eviEventId image ==. lit eventId
        pure image

-- | Insert a new event image.
insertImage :: Insert -> Hasql.Statement () (Maybe Id)
insertImage Insert {..} =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = eventImageSchema,
            rows =
              values
                [ EventImage
                    { eviId = nextId "event_images_id_seq",
                      eviEventId = lit iiEventId,
                      eviImagePath = lit iiImagePath,
                      eviCaption = lit iiCaption,
                      eviAltText = lit iiAltText,
                      eviSortOrder = lit iiSortOrder,
                      eviCreatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning eviId
          }

-- | Delete an event image by ID.
deleteImage :: Id -> Hasql.Statement () (Maybe Id)
deleteImage imageId =
  fmap listToMaybe $
    run $
      delete
        Rel8.Delete
          { from = eventImageSchema,
            using = pure (),
            deleteWhere = \_ image -> eviId image ==. lit imageId,
            returning = Returning eviId
          }

-- | Get a single event image by its ID.
getById :: Id -> Hasql.Statement () (Maybe Model)
getById imageId =
  fmap listToMaybe $
    run $
      select do
        image <- each eventImageSchema
        where_ $ eviId image ==. lit imageId
        pure image

-- | Update the sort order of an event image.
updateSortOrder :: Id -> Int64 -> Hasql.Statement () ()
updateSortOrder imageId newSortOrder =
  run_ $
    update
      Rel8.Update
        { target = eventImageSchema,
          from = pure (),
          set = \_ image -> image {eviSortOrder = lit newSortOrder},
          updateWhere = \_ image -> eviId image ==. lit imageId,
          returning = NoReturning
        }

-- | Update the sort order, caption, and alt text of an event image.
updateImageMeta ::
  Id ->
  -- | New sort order.
  Int64 ->
  -- | New caption.
  Text ->
  -- | New alt text.
  Text ->
  Hasql.Statement () ()
updateImageMeta imageId newSortOrder newCaption newAltText =
  run_ $
    update
      Rel8.Update
        { target = eventImageSchema,
          from = pure (),
          set = \_ image ->
            image
              { eviSortOrder = lit newSortOrder,
                eviCaption = lit newCaption,
                eviAltText = lit newAltText
              },
          updateWhere = \_ image -> eviId image ==. lit imageId,
          returning = NoReturning
        }
