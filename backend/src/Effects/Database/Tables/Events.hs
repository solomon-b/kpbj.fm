{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Effects.Database.Tables.Events where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..), display, displayBuilder)
import Data.Time (MonthOfYear, UTCTime, Year)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import GHC.Generics
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Event Status Type

data EventStatus = Draft | Published
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance Display EventStatus where
  displayBuilder Draft = "draft"
  displayBuilder Published = "published"

instance DecodeValue EventStatus where
  decodeValue = Decoders.enum decodeEventStatus

decodeEventStatus :: Text -> Maybe EventStatus
decodeEventStatus = \case
  "draft" -> Just Draft
  "published" -> Just Published
  _ -> Nothing

instance EncodeValue EventStatus where
  encodeValue = Encoders.enum $ \case
    Draft -> "draft"
    Published -> "published"

instance Servant.FromHttpApiData EventStatus where
  parseUrlPiece "draft" = Right Draft
  parseUrlPiece "published" = Right Published
  parseUrlPiece invalid = Left $ "Invalid EventStatus: " <> invalid

instance Servant.ToHttpApiData EventStatus where
  toUrlPiece = display

--------------------------------------------------------------------------------
-- Event Models

newtype EventId = EventId Int64
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      ToJSON,
      FromJSON,
      Display,
      DecodeValue,
      EncodeValue
    )

newtype EventTagId = EventTagId Int64
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      ToJSON,
      FromJSON,
      Display,
      DecodeValue,
      EncodeValue
    )

-- | Database Model for the @events@ table
data EventModel = EventModel
  { emId :: EventId,
    emTitle :: Text,
    emSlug :: Text,
    emDescription :: Text,
    emStartsAt :: UTCTime,
    emEndsAt :: UTCTime,
    emLocationName :: Text,
    emLocationAddress :: Text,
    emStatus :: EventStatus,
    emAuthorId :: User.Id,
    emCreatedAt :: UTCTime,
    emUpdatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance EventModel)

-- | Database Model for the @event_tags@ table
data EventTagModel = EventTagModel
  { etmId :: EventTagId,
    etmName :: Text,
    etmCreatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance EventTagModel)

-- | API Domain Type for @Event@
data EventDomain = EventDomain
  { edId :: EventId,
    edTitle :: Text,
    edSlug :: Text,
    edDescription :: Text,
    edStartsAt :: UTCTime,
    edEndsAt :: UTCTime,
    edLocationName :: Text,
    edLocationAddress :: Text,
    edStatus :: EventStatus,
    edAuthorId :: User.Id,
    edCreatedAt :: UTCTime,
    edUpdatedAt :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance EventDomain)
  deriving anyclass (FromJSON, ToJSON)

-- | API Domain Type for @EventTag@
data EventTagDomain = EventTagDomain
  { etdId :: EventTagId,
    etdName :: Text,
    etdCreatedAt :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance EventTagDomain)
  deriving anyclass (FromJSON, ToJSON)

-- | Event with author information
data EventWithAuthor = EventWithAuthor
  { ewaEvent :: EventDomain,
    ewaAuthor :: UserMetadata.Domain
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance EventWithAuthor)
  deriving anyclass (FromJSON, ToJSON)

-- | Event with tags
data EventWithTags = EventWithTags
  { ewtEvent :: EventDomain,
    ewtTags :: [EventTagDomain]
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance EventWithTags)
  deriving anyclass (FromJSON, ToJSON)

-- | Event with complete information (author + tags)
data EventComplete = EventComplete
  { ecEvent :: EventDomain,
    ecAuthor :: UserMetadata.Domain,
    ecTags :: [EventTagDomain]
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance EventComplete)
  deriving anyclass (FromJSON, ToJSON)

toDomainEvent :: EventModel -> EventDomain
toDomainEvent EventModel {..} =
  EventDomain
    { edId = emId,
      edTitle = emTitle,
      edSlug = emSlug,
      edDescription = emDescription,
      edStartsAt = emStartsAt,
      edEndsAt = emEndsAt,
      edLocationName = emLocationName,
      edLocationAddress = emLocationAddress,
      edStatus = emStatus,
      edAuthorId = emAuthorId,
      edCreatedAt = emCreatedAt,
      edUpdatedAt = emUpdatedAt
    }

toDomainEventTag :: EventTagModel -> EventTagDomain
toDomainEventTag EventTagModel {..} =
  EventTagDomain
    { etdId = etmId,
      etdName = etmName,
      etdCreatedAt = etmCreatedAt
    }

--------------------------------------------------------------------------------
-- Insert Types

data EventInsert = EventInsert
  { eiTitle :: Text,
    eiSlug :: Text,
    eiDescription :: Text,
    eiStartsAt :: UTCTime,
    eiEndsAt :: UTCTime,
    eiLocationName :: Text,
    eiLocationAddress :: Text,
    eiStatus :: EventStatus,
    eiAuthorId :: User.Id
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via EventInsert
  deriving (Display) via (RecordInstance EventInsert)

newtype EventTagInsert = EventTagInsert
  { etiName :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via EventTagInsert
  deriving (Display) via (RecordInstance EventTagInsert)

--------------------------------------------------------------------------------
-- Database Queries

-- | Get published events, optionally filtered by tag
getPublishedEvents :: Maybe Text -> Int64 -> Int64 -> Hasql.Statement () [EventModel]
getPublishedEvents maybeTagName limit offset =
  interp
    False
    [sql|
    SELECT DISTINCT e.id, e.title, e.slug, e.description, e.starts_at, e.ends_at, e.location_name, e.location_address, e.status, e.author_id, e.created_at, e.updated_at
    FROM events e
    LEFT JOIN event_tag_assignments eta ON e.id = eta.event_id
    LEFT JOIN event_tags et ON eta.tag_id = et.id
    WHERE e.status = 'published'
      AND (#{maybeTagName}::text IS NULL OR et.name = #{maybeTagName}::text)
    ORDER BY e.starts_at ASC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get event by slug
getEventBySlug :: Text -> Hasql.Statement () (Maybe EventModel)
getEventBySlug slug =
  interp
    False
    [sql|
    SELECT id, title, slug, description, starts_at, ends_at, location_name, location_address, status, author_id, created_at, updated_at
    FROM events
    WHERE slug = #{slug}
  |]

-- | Get event by ID
getEventById :: EventId -> Hasql.Statement () (Maybe EventModel)
getEventById eventId =
  interp
    False
    [sql|
    SELECT id, title, slug, description, starts_at, ends_at, location_name, location_address, status, author_id, created_at, updated_at
    FROM events
    WHERE id = #{eventId}
  |]

-- | Get events by author
getEventsByAuthor :: User.Id -> Int64 -> Int64 -> Hasql.Statement () [EventModel]
getEventsByAuthor authorId limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, description, starts_at, ends_at, location_name, location_address, status, author_id, created_at, updated_at
    FROM events
    WHERE author_id = #{authorId}
    ORDER BY starts_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Insert a new event
insertEvent :: EventInsert -> Hasql.Statement () EventId
insertEvent EventInsert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO events(title, slug, description, starts_at, ends_at, location_name, location_address, status, author_id, created_at, updated_at)
    VALUES (#{eiTitle}, #{eiSlug}, #{eiDescription}, #{eiStartsAt}, #{eiEndsAt}, #{eiLocationName}, #{eiLocationAddress}, #{eiStatus}, #{eiAuthorId}, NOW(), NOW())
    RETURNING id
  |]

-- | Update an event
updateEvent :: EventId -> EventInsert -> Hasql.Statement () (Maybe EventId)
updateEvent eventId EventInsert {..} =
  interp
    False
    [sql|
    UPDATE events
    SET title = #{eiTitle}, slug = #{eiSlug}, description = #{eiDescription},
        starts_at = #{eiStartsAt}, ends_at = #{eiEndsAt}, location_name = #{eiLocationName}, location_address = #{eiLocationAddress},
        status = #{eiStatus}, updated_at = NOW()
    WHERE id = #{eventId}
    RETURNING id
  |]

-- | Delete an event
deleteEvent :: EventId -> Hasql.Statement () (Maybe EventId)
deleteEvent eventId =
  interp
    False
    [sql|
    DELETE FROM events
    WHERE id = #{eventId}
    RETURNING id
  |]

-- | Get all event tags
getAllEventTags :: Hasql.Statement () [EventTagModel]
getAllEventTags =
  interp
    False
    [sql|
    SELECT id, name, created_at
    FROM event_tags
    ORDER BY name
  |]

-- | Get tags for a specific event
getEventTags :: EventId -> Hasql.Statement () [EventTagModel]
getEventTags eventId =
  interp
    False
    [sql|
    SELECT et.id, et.name, et.created_at
    FROM event_tags et
    JOIN event_tag_assignments eta ON et.id = eta.tag_id
    WHERE eta.event_id = #{eventId}
    ORDER BY et.name
  |]

-- | Insert a new event tag
insertEventTag :: EventTagInsert -> Hasql.Statement () EventTagId
insertEventTag EventTagInsert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO event_tags(name, created_at)
    VALUES (#{etiName}, NOW())
    RETURNING id
  |]

-- | Assign a tag to an event
assignTagToEvent :: EventId -> EventTagId -> Hasql.Statement () ()
assignTagToEvent eventId tagId =
  interp
    False
    [sql|
    INSERT INTO event_tag_assignments(event_id, tag_id)
    VALUES (#{eventId}, #{tagId})
    ON CONFLICT DO NOTHING
  |]

-- | Remove a tag from an event
removeTagFromEvent :: EventId -> EventTagId -> Hasql.Statement () ()
removeTagFromEvent eventId tagId =
  interp
    False
    [sql|
    DELETE FROM event_tag_assignments
    WHERE event_id = #{eventId} AND tag_id = #{tagId}
  |]

-- Tag with count for queries
data EventTagWithCount = EventTagWithCount
  { etwcTag :: Text,
    etwcCount :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance EventTagWithCount)

-- | Get event tags with their counts
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

-- | Get events for a specific month and year, optionally filtered by tag
getEventsForMonth :: Maybe Text -> Year -> MonthOfYear -> Hasql.Statement () [EventModel]
getEventsForMonth maybeTagName year month =
  interp
    False
    [sql|
    SELECT DISTINCT e.id, e.title, e.slug, e.description, e.starts_at, e.ends_at, e.location_name, e.location_address, e.status, e.author_id, e.created_at, e.updated_at
    FROM events e
    LEFT JOIN event_tag_assignments eta ON e.id = eta.event_id
    LEFT JOIN event_tags et ON eta.tag_id = et.id
    WHERE e.status = 'published'
      AND EXTRACT(YEAR FROM e.starts_at) = #{fromIntegral year :: Int64}
      AND EXTRACT(MONTH FROM e.starts_at) = #{fromIntegral month :: Int64}
      AND (#{maybeTagName}::text IS NULL OR et.name = #{maybeTagName}::text)
    ORDER BY e.starts_at ASC
  |]

-- | Get events for a specific week (ISO week number), optionally filtered by tag
getEventsForWeek :: Maybe Text -> Year -> Int -> Hasql.Statement () [EventModel]
getEventsForWeek maybeTagName year weekNum =
  interp
    False
    [sql|
    SELECT DISTINCT e.id, e.title, e.slug, e.description, e.starts_at, e.ends_at, e.location_name, e.location_address, e.status, e.author_id, e.created_at, e.updated_at
    FROM events e
    LEFT JOIN event_tag_assignments eta ON e.id = eta.event_id
    LEFT JOIN event_tags et ON eta.tag_id = et.id
    WHERE e.status = 'published'
      AND EXTRACT(YEAR FROM e.starts_at) = #{fromIntegral year :: Int64}
      AND EXTRACT(WEEK FROM e.starts_at) = #{fromIntegral weekNum :: Int64}
      AND (#{maybeTagName}::text IS NULL OR et.name = #{maybeTagName}::text)
    ORDER BY e.starts_at ASC
  |]
