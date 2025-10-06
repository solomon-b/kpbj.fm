{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.EventTags where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import GHC.Generics
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Event Tag Models

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

-- | Database Model for the @event_tags@ table
data EventTagModel = EventTagModel
  { etmId :: EventTagId,
    etmName :: Text,
    etmCreatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance EventTagModel)

-- | Tag with count for queries
data EventTagWithCount = EventTagWithCount
  { etwcTag :: Text,
    etwcCount :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance EventTagWithCount)

--------------------------------------------------------------------------------
-- Insert Types

newtype EventTagInsert = EventTagInsert
  { etiName :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via EventTagInsert
  deriving (Display) via (RecordInstance EventTagInsert)

--------------------------------------------------------------------------------
-- Database Queries

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
