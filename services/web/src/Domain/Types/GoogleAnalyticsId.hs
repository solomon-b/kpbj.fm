module Domain.Types.GoogleAnalyticsId
  ( GoogleAnalyticsId (..),
  )
where

--------------------------------------------------------------------------------

import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Display (Display)

--------------------------------------------------------------------------------

-- | Google Analytics Measurement ID (gtag).
--
-- This is the tracking ID for Google Analytics 4, typically in the format "G-XXXXXXXXXX".
-- Set via the GOOGLE_ANALYTICS_GTAG environment variable.
newtype GoogleAnalyticsId = GoogleAnalyticsId {unGoogleAnalyticsId :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Display, IsString)
