{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Database table definition and queries for @events@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex joins.
module Effects.Database.Tables.Events
  ( -- * Status Type
    Status (..),

    -- * Id Type
    Id (..),

    -- * Table Definition
    Event (..),
    eventSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getPublishedEvents,
    getEventBySlug,
    getEventById,
    getEventsByAuthor,
    insertEvent,
    updateEvent,
    deleteEvent,
    getAllEvents,

    -- * Result Types
    EventWithAuthor (..),
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
import Data.Text.Display (Display (..), RecordInstance (..), display)
import Data.Time (UTCTime)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Event Status Type

-- | Event publication status.
data Status = Draft | Published
  deriving stock (Generic, Show, Eq, Ord, Prelude.Enum, Bounded, Read)
  deriving anyclass (FromJSON, ToJSON)

instance DBType Status where
  typeInformation =
    parseTypeInformation
      ( \case
          "draft" -> Right Draft
          "published" -> Right Published
          other -> Left $ "Invalid Status: " <> Text.unpack other
      )
      ( \case
          Draft -> "draft"
          Published -> "published"
      )
      typeInformation

instance DBEq Status

instance Display Status where
  displayBuilder Draft = "draft"
  displayBuilder Published = "published"

instance DecodeValue Status where
  decodeValue = Decoders.enum decodeStatus

decodeStatus :: Text -> Maybe Status
decodeStatus = \case
  "draft" -> Just Draft
  "published" -> Just Published
  _ -> Nothing

instance EncodeValue Status where
  encodeValue = Encoders.enum $ \case
    Draft -> "draft"
    Published -> "published"

instance Servant.FromHttpApiData Status where
  parseUrlPiece "draft" = Right Draft
  parseUrlPiece "published" = Right Published
  parseUrlPiece invalid = Left $ "Invalid Status: " <> invalid

instance Servant.ToHttpApiData Status where
  toUrlPiece = display

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for event primary keys.
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

-- | The @events@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data Event f = Event
  { emId :: Column f Id,
    emTitle :: Column f Text,
    emSlug :: Column f Slug,
    emDescription :: Column f Text,
    emStartsAt :: Column f UTCTime,
    emEndsAt :: Column f UTCTime,
    emLocationName :: Column f Text,
    emLocationAddress :: Column f Text,
    emStatus :: Column f Status,
    emAuthorId :: Column f User.Id,
    emPosterImageUrl :: Column f (Maybe Text),
    emCreatedAt :: Column f UTCTime,
    emUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (Event f)

deriving stock instance (f ~ Result) => Eq (Event f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (Event Result)

-- | Display instance for Event Result.
instance Display (Event Result) where
  displayBuilder event =
    "Event { id = "
      <> displayBuilder (emId event)
      <> ", title = "
      <> displayBuilder (emTitle event)
      <> ", slug = "
      <> displayBuilder (emSlug event)
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @Event Result@.
type Model = Event Result

-- | Table schema connecting the Haskell type to the database table.
eventSchema :: TableSchema (Event Name)
eventSchema =
  TableSchema
    { name = "events",
      columns =
        Event
          { emId = "id",
            emTitle = "title",
            emSlug = "slug",
            emDescription = "description",
            emStartsAt = "starts_at",
            emEndsAt = "ends_at",
            emLocationName = "location_name",
            emLocationAddress = "location_address",
            emStatus = "status",
            emAuthorId = "author_id",
            emPosterImageUrl = "poster_image_url",
            emCreatedAt = "created_at",
            emUpdatedAt = "updated_at"
          }
    }

--------------------------------------------------------------------------------
-- Result Types

-- | Event with author information.
data EventWithAuthor = EventWithAuthor
  { ewaEvent :: Model,
    ewaAuthor :: UserMetadata.Model
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance EventWithAuthor)

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new events.
data Insert = Insert
  { eiTitle :: Text,
    eiSlug :: Slug,
    eiDescription :: Text,
    eiStartsAt :: UTCTime,
    eiEndsAt :: UTCTime,
    eiLocationName :: Text,
    eiLocationAddress :: Text,
    eiStatus :: Status,
    eiAuthorId :: User.Id,
    eiPosterImageUrl :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Queries

-- | Get published events.
getPublishedEvents :: Limit -> Offset -> Hasql.Statement () [Model]
getPublishedEvents (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy (emStartsAt >$< asc) do
            event <- each eventSchema
            where_ $ emStatus event ==. lit Published
            pure event

-- | Get event by slug.
getEventBySlug :: Slug -> Hasql.Statement () (Maybe Model)
getEventBySlug slug = fmap listToMaybe $ run $ select do
  event <- each eventSchema
  where_ $ emSlug event ==. lit slug
  pure event

-- | Get event by ID.
getEventById :: Id -> Hasql.Statement () (Maybe Model)
getEventById eventId = fmap listToMaybe $ run $ select do
  event <- each eventSchema
  where_ $ emId event ==. lit eventId
  pure event

-- | Get events by author.
getEventsByAuthor :: User.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getEventsByAuthor authorId (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy (emStartsAt >$< desc) do
            event <- each eventSchema
            where_ $ emAuthorId event ==. lit authorId
            pure event

-- | Insert a new event.
insertEvent :: Insert -> Hasql.Statement () Id
insertEvent Insert {..} =
  fmap head $
    run $
      insert
        Rel8.Insert
          { into = eventSchema,
            rows =
              values
                [ Event
                    { emId = coerce (nextval "events_id_seq"),
                      emTitle = lit eiTitle,
                      emSlug = lit eiSlug,
                      emDescription = lit eiDescription,
                      emStartsAt = lit eiStartsAt,
                      emEndsAt = lit eiEndsAt,
                      emLocationName = lit eiLocationName,
                      emLocationAddress = lit eiLocationAddress,
                      emStatus = lit eiStatus,
                      emAuthorId = lit eiAuthorId,
                      emPosterImageUrl = lit eiPosterImageUrl,
                      emCreatedAt = now,
                      emUpdatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning emId
          }

-- | Update an event.
--
-- Uses raw SQL for consistency with update patterns.
updateEvent :: Id -> Insert -> Hasql.Statement () (Maybe Id)
updateEvent eventId Insert {..} =
  interp
    False
    [sql|
    UPDATE events
    SET title = #{eiTitle}, slug = #{eiSlug}, description = #{eiDescription},
        starts_at = #{eiStartsAt}, ends_at = #{eiEndsAt}, location_name = #{eiLocationName}, location_address = #{eiLocationAddress},
        status = #{eiStatus}, poster_image_url = #{eiPosterImageUrl}, updated_at = NOW()
    WHERE id = #{eventId}
    RETURNING id
  |]

-- | Delete an event.
deleteEvent :: Id -> Hasql.Statement () (Maybe Id)
deleteEvent eventId =
  fmap listToMaybe $
    run $
      delete
        Rel8.Delete
          { from = eventSchema,
            using = pure (),
            deleteWhere = \_ event -> emId event ==. lit eventId,
            returning = Returning emId
          }

-- | Get all events for dashboard (staff/admin access - all statuses).
getAllEvents :: Limit -> Offset -> Hasql.Statement () [Model]
getAllEvents (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy (emStartsAt >$< desc) do
            each eventSchema
