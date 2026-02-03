{-# LANGUAGE DeriveAnyClass #-}

-- | Shared types for playout API endpoints.
--
-- These endpoints are used by Liquidsoap to fetch audio URLs for playback.
module API.Playout.Types
  ( PlayoutResponse (..),
    PlayoutMetadata (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
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
