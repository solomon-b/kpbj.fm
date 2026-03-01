{-# LANGUAGE DeriveAnyClass #-}

-- | Shared types for playout API endpoints.
--
-- These endpoints are used by Liquidsoap to fetch audio URLs for playback.
module API.Playout.Types
  ( PlayoutResponse (..),
    PlayoutMetadata (title, artist),
    mkPlayoutMetadata,
    PlayedRequest (..),
    sanitizeAnnotateValue,
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

-- | Response type for playout endpoints.
--
-- Serializes to either:
-- - @{"url": "https://...", "title": "...", "artist": "..."}@ when audio is available
-- - @null@ when no audio is available
data PlayoutResponse
  = -- | Audio URL is available for playback with metadata
    PlayoutAvailable Text PlayoutMetadata
  | -- | No audio currently available
    PlayoutUnavailable
  deriving stock (Generic, Show, Eq)

instance ToJSON PlayoutResponse where
  toJSON PlayoutUnavailable = toJSON (Nothing :: Maybe Text)
  toJSON (PlayoutAvailable url meta) =
    object
      [ "url" .= url,
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
  Text -> -- ^ Track title
  Text -> -- ^ Artist name
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
sanitizeAnnotateValue :: Text -> Text
sanitizeAnnotateValue = Text.filter (`notElem` ['"', '\n', '\r', '\\'])
