-- | Origin type for CSRF protection.
--
-- The Origin represents the scheme + host + port of a request origin
-- as sent in the HTTP Origin header by browsers for cross-origin requests.
module Domain.Types.Origin
  ( -- * Types
    Origin (..),

    -- * Validation
    isAllowedOrigin,
    isLocalhostOrigin,
  )
where

--------------------------------------------------------------------------------

import App.Config (Hostname (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display)
import Data.Text.Encoding qualified as Text
import Servant (FromHttpApiData (..))

--------------------------------------------------------------------------------
-- Types

-- | An HTTP Origin value (scheme + host + optional port).
--
-- Examples:
-- - "https://kpbj.fm"
-- - "http://localhost:4000"
-- - "https://example.com:8443"
newtype Origin = Origin {unOrigin :: Text}
  deriving stock (Show, Eq)
  deriving newtype (Ord, Display, FromJSON, ToJSON)

instance FromHttpApiData Origin where
  parseUrlPiece = Right . Origin
  parseHeader = Right . Origin . Text.decodeUtf8

--------------------------------------------------------------------------------
-- Validation

-- | Check if an origin is in the allowed list or is localhost.
--
-- Comparison is case-insensitive and ignores trailing slashes.
-- Localhost origins (any port) are always allowed for development.
isAllowedOrigin :: Origin -> Hostname -> Bool
isAllowedOrigin origin hostname =
  isLocalhostOrigin origin
    || normalizeOrigin (unOrigin origin)
      == (normalizeOrigin $ getHostName hostname)

-- | Check if an origin is localhost (any port).
--
-- Matches both "localhost" and "127.0.0.1" with any port.
isLocalhostOrigin :: Origin -> Bool
isLocalhostOrigin (Origin o) =
  let normalized = Text.toLower o
   in Text.isPrefixOf "http://localhost:" normalized
        || Text.isPrefixOf "http://127.0.0.1:" normalized
        || normalized == "http://localhost"
        || normalized == "http://127.0.0.1"

--------------------------------------------------------------------------------
-- Helpers

-- | Normalize an origin for comparison.
--
-- - Converts to lowercase
-- - Removes trailing slashes
normalizeOrigin :: Text -> Text
normalizeOrigin = Text.toLower . Text.dropWhileEnd (== '/')
