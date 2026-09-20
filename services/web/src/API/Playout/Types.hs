{-# LANGUAGE DeriveAnyClass #-}

-- | Shared types for playout API endpoints.
--
-- These endpoints are used by Liquidsoap to fetch audio URLs for playback.
module API.Playout.Types
  ( PlayoutMetadata (title, artist),
    mkPlayoutMetadata,
    PlayedRequest (..),
    sanitizeAnnotateValue,
    PlayoutTrack (..),
    FallbackResponse,
    BreakResponse,
    NowPlayingResponse (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON (..), object, (.=))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Metadata for stream display (ICY metadata).
data PlayoutMetadata = PlayoutMetadata
  { title :: Text,
    artist :: Text
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------

-- | A single track in a playout response with its source type.
--
-- Used by the fallback endpoint to return multiple tracks (station ID + ephemeral).
-- Serializes to flat JSON: @{"url": "...", "title": "...", "artist": "...", "source_type": "..."}@
data PlayoutTrack = PlayoutTrack
  { ptUrl :: Text,
    ptTitle :: Text,
    ptArtist :: Text,
    ptSourceType :: Text
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON PlayoutTrack where
  toJSON track =
    object
      [ "url" .= sanitizeAnnotateValue track.ptUrl,
        "title" .= track.ptTitle,
        "artist" .= track.ptArtist,
        "source_type" .= track.ptSourceType
      ]

-- | Fallback response is a list of tracks (station ID + ephemeral).
--
-- Serializes to a JSON array. Empty array means no content available.
type FallbackResponse = [PlayoutTrack]

-- | Break response is a list of tracks for one break window.
--
-- A station ID first, then the PSAs and advertisement spots that fit in the
-- rest of the window.
--
-- Serializes to a JSON array. Empty array means no break is due, or that no
-- content is available for one. Liquidsoap plays on without cutting the
-- current source in either case, so the two need not be distinguished.
type BreakResponse = [PlayoutTrack]

--------------------------------------------------------------------------------

-- | Response type for the "now playing" endpoint.
--
-- Serializes to either:
-- - @{"url": "https://...", "title": "...", "artist": "..."}@ when audio is available
-- - @null@ when no audio is available
data NowPlayingResponse
  = NowPlaying Text PlayoutMetadata
  | NothingPlaying
  deriving stock (Generic, Show, Eq)

instance ToJSON NowPlayingResponse where
  toJSON NothingPlaying = toJSON (Nothing :: Maybe Text)
  toJSON (NowPlaying url meta) =
    object
      [ "url" .= sanitizeAnnotateValue url,
        "title" .= meta.title,
        "artist" .= meta.artist
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
    prStartedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromJSON)

--------------------------------------------------------------------------------

-- | Smart constructor for 'PlayoutMetadata'.
--
-- Sanitizes title and artist for safe use in Liquidsoap @annotate:@ URIs.
mkPlayoutMetadata ::
  -- | Track title
  Text ->
  -- | Artist name
  Text ->
  PlayoutMetadata
mkPlayoutMetadata t a =
  PlayoutMetadata
    { title = sanitizeAnnotateValue t,
      artist = sanitizeAnnotateValue a
    }

-- | Sanitize a text value for use in a Liquidsoap @annotate:@ URI.
--
-- The annotate format uses double quotes as value delimiters and newlines
-- as command separators in the telnet protocol. Characters that would break
-- the format are stripped to prevent malformed commands.
--
-- Every value that reaches an @annotate:@ URI goes through this, the media URL
-- included. A URL cannot hold one of these characters today, because an object
-- key is a slug, a date, a UUID, and an extension. This holds if that changes.
sanitizeAnnotateValue :: Text -> Text
sanitizeAnnotateValue = Text.filter (`notElem` ['"', '\n', '\r', '\\'])
