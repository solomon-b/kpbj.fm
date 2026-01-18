{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module App.Common where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import Component.DashboardFrame (DashboardNav)
import Component.DashboardFrame qualified as DashboardFrame
import Component.Frame (loadContentOnly, loadFrame, loadFrameWithUser)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Has (Has, getter)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId)
import Domain.Types.HxRequest (HxRequest (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log.Class qualified as Log
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

-- | Get user info
getUserInfo ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m
  ) =>
  Maybe Cookie ->
  m (Maybe (User.Model, UserMetadata.Model))
getUserInfo (coerce -> cookie) =
  Auth.userLoginState cookie >>= \case
    Auth.IsNotLoggedIn ->
      pure Nothing
    Auth.IsLoggedIn user ->
      execQuerySpan (UserMetadata.getUserMetadata user.mId) >>= \case
        Right userMetadata ->
          pure ((user,) <$> userMetadata)
        _ -> do
          Log.logAttention "Failed to query user_metadata" (Aeson.object ["user.id" .= user.mId])
          pure Nothing

-- | Render template with proper HTMX handling and suspension status.
--
-- Shows a warning banner at the top of every page for suspended users.
-- Pass NotSuspended for unauthenticated users or when suspension status is unknown.
-- Includes Google Analytics tracking if GOOGLE_ANALYTICS_GTAG is configured.
renderTemplate ::
  ( Log.MonadLog m,
    MonadCatch m,
    MonadReader env m,
    Has (Maybe GoogleAnalyticsId) env
  ) =>
  HxRequest ->
  Maybe UserMetadata.Model ->
  Lucid.Html () ->
  m (Lucid.Html ())
renderTemplate hxRequest mUserInfo templateContent = do
  mGoogleAnalyticsId <- asks getter
  case (mUserInfo, hxRequest) of
    (Just userInfo, IsNotHxRequest) ->
      loadFrameWithUser mGoogleAnalyticsId userInfo templateContent
    (_, IsHxRequest) ->
      loadContentOnly templateContent
    (_, IsNotHxRequest) ->
      loadFrame mGoogleAnalyticsId templateContent

-- | Render dashboard template with separate full-page layout.
--
-- The dashboard uses its own frame with a left sidebar navigation,
-- completely separate from the main site's header/navigation.
-- Stats appear in the top bar (schedule info, counts, etc.).
-- The action button appears in the top bar (e.g., "New Episode", "New Post").
-- Includes Google Analytics tracking if GOOGLE_ANALYTICS_GTAG is configured.
renderDashboardTemplate ::
  ( Log.MonadLog m,
    MonadCatch m,
    MonadReader env m,
    Has (Maybe GoogleAnalyticsId) env
  ) =>
  HxRequest ->
  UserMetadata.Model ->
  [Shows.Model] ->
  Maybe Shows.Model ->
  DashboardNav ->
  Maybe (Lucid.Html ()) ->
  Maybe (Lucid.Html ()) ->
  Lucid.Html () ->
  m (Lucid.Html ())
renderDashboardTemplate hxRequest userInfo allShows selectedShow activeNav statsContent actionButton templateContent = do
  mGoogleAnalyticsId <- asks getter
  case hxRequest of
    IsHxRequest ->
      DashboardFrame.loadDashboardContentOnly templateContent
    IsNotHxRequest ->
      DashboardFrame.loadDashboardFrame mGoogleAnalyticsId userInfo allShows selectedShow activeNav statsContent actionButton templateContent
