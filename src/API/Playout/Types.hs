{-# LANGUAGE DeriveAnyClass #-}

-- | Shared types for playout API endpoints.
--
-- These endpoints are used by Liquidsoap to fetch audio URLs for playback.
module API.Playout.Types
  ( PlayoutResponse (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Response type for playout endpoints.
--
-- Serializes to either:
-- - @{"url": "https://..."}@ when audio is available
-- - @null@ when no audio is available
data PlayoutResponse
  = -- | Audio URL is available for playback
    PlayoutAvailable Text
  | -- | No audio currently available
    PlayoutUnavailable
  deriving stock (Generic, Show, Eq)

instance ToJSON PlayoutResponse where
  toJSON PlayoutUnavailable = toJSON (Nothing :: Maybe Text)
  toJSON (PlayoutAvailable url) = object ["url" .= url]
