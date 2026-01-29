-- | Origin type for CSRF protection.
--
-- The Origin represents the scheme + host + port of a request origin
-- as sent in the HTTP Origin header by browsers for cross-origin requests.
module Domain.Types.Origin
  ( -- * Types
    Origin (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import Data.Text (Text)
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
  deriving newtype (Ord, Display, FromJSON, ToJSON, IsString)

instance FromHttpApiData Origin where
  parseUrlPiece = Right . Origin
  parseHeader = Right . Origin . Text.decodeUtf8
