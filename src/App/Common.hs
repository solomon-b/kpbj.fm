{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module App.Common where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Config (Environment)
import App.Cookie qualified as Cookie
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
  case cookie >>= Cookie.lookupSessionId env of
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
renderTemplate ::
  HxRequest ->
  Maybe UserMetadata.Model ->
  Lucid.Html () ->
  AppM (Lucid.Html ())
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
