{-# LANGUAGE DeriveAnyClass #-}

-- | Shared types for playout API endpoints.
--
-- Liquidsoap uses these endpoints to fetch the timeline and to report plays.
module API.Playout.Types
  ( PlayedRequest (..),
    sanitizeAnnotateValue,
    NowResponse (..),
    SegmentResponse (..),
    ItemResponse (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON (..), object, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Response type for the @/now@ endpoint.
--
-- The requested segment and the segment after it. Liquidsoap plays the first
-- and downloads the second ahead of time. Serializes to
-- @{"current": {...}, "next": {...}}@, with a null @"next"@ when there is no
-- second segment.
data NowResponse = NowResponse
  { nrSegment :: SegmentResponse,
    -- | Nothing when the timeline has no segment at the end of the first.
    nrNext :: Maybe SegmentResponse
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON NowResponse where
  toJSON response =
    object ["current" .= response.nrSegment, "next" .= response.nrNext]

-- | One segment of the timeline. Liquidsoap plays the items in order.
--
-- Serializes to @{"id": "...", "segment_start": "...", "segment_end": "...",
-- "ends_in_seconds": 1200.5, "items": [...]}@.
data SegmentResponse = SegmentResponse
  { -- | Changes only when what airs in the segment changes. Liquidsoap
    -- compares it to tell whether the segment it holds is still current.
    srId :: Text,
    srSegmentStart :: UTCTime,
    srSegmentEnd :: UTCTime,
    -- | The seconds from the request time to the end of the segment.
    srEndsInSeconds :: Double,
    srItems :: [ItemResponse]
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON SegmentResponse where
  toJSON segment =
    object
      [ "id" .= segment.srId,
        "segment_start" .= segment.srSegmentStart,
        "segment_end" .= segment.srSegmentEnd,
        "ends_in_seconds" .= segment.srEndsInSeconds,
        "items" .= segment.srItems
      ]

-- | One item of a 'SegmentResponse'.
--
-- The type is @episode@, @ephemeral@, @filler@, or the source type of a break
-- track. The break item id and the cue out are null when they do not apply.
data ItemResponse = ItemResponse
  { irType :: Text,
    irUrl :: Text,
    irTitle :: Text,
    irArtist :: Text,
    irDurationSeconds :: Int64,
    irCueIn :: Int64,
    irCueOut :: Maybe Int64,
    irBreakItemId :: Maybe Int64,
    irPlannedStart :: UTCTime
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON ItemResponse where
  toJSON item =
    object
      [ "type" .= item.irType,
        "url" .= item.irUrl,
        "title" .= item.irTitle,
        "artist" .= item.irArtist,
        "duration_seconds" .= item.irDurationSeconds,
        "cue_in" .= item.irCueIn,
        "cue_out" .= item.irCueOut,
        "break_item_id" .= item.irBreakItemId,
        "planned_start" .= item.irPlannedStart
      ]

--------------------------------------------------------------------------------

-- | Request body for POST /api/playout/played.
--
-- Sent by Liquidsoap when a track starts playing on the stream.
data PlayedRequest = PlayedRequest
  { prTitle :: Text,
    prArtist :: Maybe Text,
    prSourceType :: Text,
    prSourceUrl :: Text,
    prStartedAt :: UTCTime,
    -- | The break item id from the track's annotations. Liquidsoap sends an
    -- empty string for a track that is not a break item.
    prBreakItemId :: Maybe Text,
    -- | The start time of this airing in the timeline. An older client does
    -- not send it, and it decodes as Nothing.
    prPlannedStart :: Maybe UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON)

--------------------------------------------------------------------------------

-- | Sanitize a text value for use in a Liquidsoap @annotate:@ URI.
--
-- The annotate format uses double quotes as value delimiters. A newline, a
-- carriage return, or a backslash can also break the request string. This
-- function removes these characters, so the request always parses.
--
-- Every value that reaches an @annotate:@ URI goes through this, the media URL
-- included. A URL cannot hold one of these characters today, because an object
-- key is a slug, a date, a UUID, and an extension. This holds if that changes.
sanitizeAnnotateValue :: Text -> Text
sanitizeAnnotateValue = Text.filter (`notElem` ['"', '\n', '\r', '\\'])
