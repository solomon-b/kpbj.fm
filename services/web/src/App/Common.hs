{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module App.Common where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Config (Environment)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav)
import Component.DashboardFrame qualified as DashboardFrame
import Component.Frame (loadContentOnly, loadFrame, loadFrameWithUser)
import Control.Monad.Reader (asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Has (getter)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.StreamSettings qualified as StreamSettings
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log.Class qualified as Log
import Lucid qualified

--------------------------------------------------------------------------------

-- | Get user info from session cookie.
--
-- Uses environment-specific cookie names to avoid collisions between
-- staging and production (since staging.kpbj.fm is a subdomain of kpbj.fm,
-- production cookies with Domain=.kpbj.fm would otherwise be sent to staging).
getUserInfo :: Maybe Cookie -> AppM (Maybe (User.Model, UserMetadata.Model))
getUserInfo (coerce -> cookie) = do
  env <- asks (getter @Environment)
  case cookie >>= Auth.lookupSessionId env of
    Nothing -> pure Nothing
    Just sessionId ->
      Auth.getAuth sessionId >>= \case
        Right (Just Auth.Authz {authzUser = user}) ->
          execQuery (UserMetadata.getUserMetadata user.mId) >>= \case
            Right userMetadata ->
              pure ((user,) <$> userMetadata)
            _ -> do
              Log.logAttention "Failed to query user_metadata" (Aeson.object ["user.id" .= user.mId])
              pure Nothing
        _ -> pure Nothing

-- | Render template with proper HTMX handling and suspension status.
--
-- Shows a warning banner at the top of every page for suspended users.
-- Pass NotSuspended for unauthenticated users or when suspension status is unknown.
-- Includes Google Analytics tracking if GOOGLE_ANALYTICS_GTAG is configured.
-- Fetches stream settings from the database for the music player.
renderTemplate ::
  HxRequest ->
  Maybe UserMetadata.Model ->
  Lucid.Html () ->
  AppM (Lucid.Html ())
renderTemplate hxRequest mUserInfo templateContent = do
  mGoogleAnalyticsId <- asks getter
  case hxRequest of
    IsHxRequest ->
      loadContentOnly templateContent
    IsNotHxRequest -> do
      -- Fetch stream settings for the music player
      streamSettings <- getStreamSettings
      case mUserInfo of
        Just userInfo ->
          loadFrameWithUser mGoogleAnalyticsId streamSettings userInfo templateContent
        Nothing ->
          loadFrame mGoogleAnalyticsId streamSettings templateContent

-- | Get stream settings from the database with fallback defaults.
--
-- If stream settings cannot be fetched (database error or missing row),
-- returns default hardcoded values to ensure the player always works.
getStreamSettings :: AppM StreamSettings.Model
getStreamSettings = do
  result <- execQuery StreamSettings.getStreamSettings
  case result of
    Right (Just settings) -> pure settings
    _ -> do
      -- Log and return default settings if fetch fails
      Log.logAttention "Failed to fetch stream settings, using defaults" ()
      pure defaultStreamSettings

-- | Render template for unauthenticated pages (login, register, etc.)
--
-- This is a simpler version of renderTemplate for pages that don't need
-- user context. It still fetches stream settings for the music player.
renderUnauthTemplate ::
  HxRequest ->
  Lucid.Html () ->
  AppM (Lucid.Html ())
renderUnauthTemplate hxRequest templateContent = do
  case hxRequest of
    IsHxRequest -> loadContentOnly templateContent
    IsNotHxRequest -> do
      mGoogleAnalyticsId <- asks getter
      streamSettings <- getStreamSettings
      loadFrame mGoogleAnalyticsId streamSettings templateContent

-- | Default stream settings used when database fetch fails.
--
-- Uses epoch time (1970-01-01 00:00:00 UTC) as a sentinel value for updatedAt.
defaultStreamSettings :: StreamSettings.Model
defaultStreamSettings =
  StreamSettings.StreamSetting
    { StreamSettings.ssId = 1,
      StreamSettings.ssStreamUrl = "https://kpbj.hasnoskills.com/listen/kpbj_test_station/radio.mp3",
      StreamSettings.ssMetadataUrl = "https://kpbj.hasnoskills.com/api/nowplaying/kpbj_test_station",
      StreamSettings.ssUpdatedAt = epochTime,
      StreamSettings.ssUpdatedBy = Nothing
    }
  where
    epochTime = read "1970-01-01 00:00:00 UTC"

-- | Render dashboard template with separate full-page layout.
--
-- The dashboard uses its own frame with a left sidebar navigation,
-- completely separate from the main site's header/navigation.
-- Stats appear in the top bar (schedule info, counts, etc.).
-- The action button appears in the top bar (e.g., "New Episode", "New Post").
-- Includes Google Analytics tracking if GOOGLE_ANALYTICS_GTAG is configured.
renderDashboardTemplate ::
  HxRequest ->
  UserMetadata.Model ->
  [Shows.Model] ->
  Maybe Shows.Model ->
  DashboardNav ->
  Maybe (Lucid.Html ()) ->
  Maybe (Lucid.Html ()) ->
  Lucid.Html () ->
  AppM (Lucid.Html ())
renderDashboardTemplate hxRequest userInfo allShows selectedShow activeNav statsContent actionButton templateContent = do
  mGoogleAnalyticsId <- asks getter
  case hxRequest of
    IsHxRequest ->
      DashboardFrame.loadDashboardContentOnly templateContent
    IsNotHxRequest ->
      DashboardFrame.loadDashboardFrame mGoogleAnalyticsId userInfo allShows selectedShow activeNav statsContent actionButton templateContent
