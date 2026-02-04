module App.Analytics
  ( -- * Analytics Configuration
    AnalyticsConfig (..),
    loadAnalyticsConfig,

    -- * Re-exports
    GoogleAnalyticsId (..),
  )
where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Has qualified as Has
import Data.Text qualified as Text
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId (..))
import System.Environment (lookupEnv)

--------------------------------------------------------------------------------

-- | Analytics configuration for the application.
--
-- Currently supports Google Analytics 4 via the gtag.js script.
-- Set the APP_GOOGLE_ANALYTICS_GTAG environment variable to enable tracking.
newtype AnalyticsConfig = AnalyticsConfig
  { googleAnalyticsId :: Maybe GoogleAnalyticsId
  }

instance Has.Has (Maybe GoogleAnalyticsId) AnalyticsConfig where
  getter = googleAnalyticsId
  modifier f cfg = cfg {googleAnalyticsId = f (googleAnalyticsId cfg)}

--------------------------------------------------------------------------------

-- | Load analytics configuration from environment variables (Phase 1).
--
-- Environment variables:
--   - APP_GOOGLE_ANALYTICS_GTAG: Google Analytics 4 Measurement ID (e.g., "G-XXXXXXXXXX")
--
-- If not set, analytics tracking is disabled.
loadAnalyticsConfig :: (MonadIO m) => m AnalyticsConfig
loadAnalyticsConfig = liftIO $ do
  mGtag <- lookupEnv "APP_GOOGLE_ANALYTICS_GTAG"
  pure AnalyticsConfig {googleAnalyticsId = GoogleAnalyticsId . Text.pack <$> mGtag}
