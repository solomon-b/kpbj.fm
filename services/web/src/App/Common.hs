{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module App.Common where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Config (Environment)
import App.CustomContext (StreamConfig)
import App.Monad (AppM)
import Component.FeedLink (FeedLink)
import Component.Frame (loadContentOnly, loadFrame, loadFrameWithUser)
import Control.Monad.Reader (asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Has (getter)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..))
import Effects.Database.Execute (execQuery)
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
-- Reads stream config from environment for the music player.
renderTemplate ::
  HxRequest ->
  Maybe UserMetadata.Model ->
  Lucid.Html () ->
  AppM (Lucid.Html ())
renderTemplate hxRequest mUserInfo =
  renderTemplateWithFeeds hxRequest mUserInfo []

-- | 'renderTemplate' for a page that offers RSS feeds.
--
-- The full page gets one RSS discovery link in the head for each feed. An
-- HTMX fragment has no head, so the feeds have no effect on it.
renderTemplateWithFeeds ::
  HxRequest ->
  Maybe UserMetadata.Model ->
  [FeedLink] ->
  Lucid.Html () ->
  AppM (Lucid.Html ())
renderTemplateWithFeeds hxRequest mUserInfo feedLinks templateContent = do
  mGoogleAnalyticsId <- asks getter
  case hxRequest of
    IsHxRequest ->
      loadContentOnly templateContent
    IsNotHxRequest -> do
      streamCfg <- asks (getter @StreamConfig)
      case mUserInfo of
        Just userInfo ->
          loadFrameWithUser mGoogleAnalyticsId streamCfg userInfo feedLinks templateContent
        Nothing ->
          loadFrame mGoogleAnalyticsId streamCfg feedLinks templateContent

-- | Render template for unauthenticated pages (login, register, etc.)
--
-- This is a simpler version of renderTemplate for pages that don't need
-- user context. It still reads stream config for the music player.
renderUnauthTemplate ::
  HxRequest ->
  Lucid.Html () ->
  AppM (Lucid.Html ())
renderUnauthTemplate hxRequest templateContent = do
  case hxRequest of
    IsHxRequest -> loadContentOnly templateContent
    IsNotHxRequest -> do
      mGoogleAnalyticsId <- asks getter
      streamCfg <- asks (getter @StreamConfig)
      loadFrame mGoogleAnalyticsId streamCfg [] templateContent
