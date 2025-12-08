{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Redirect (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Get.Templates.Auth (notAuthorizedTemplate, notLoggedInTemplate)
import API.Links (dashboardEpisodesLinks, dashboardLinks)
import API.Types
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectTemplate, redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.List (uncons)
import Data.String.Interpolate (i)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI dashboardLinks.home

dashboardEpisodesGetUrl :: Shows.Model -> Links.URI
dashboardEpisodesGetUrl showModel = Links.linkURI $ dashboardEpisodesLinks.list showModel.slug

--------------------------------------------------------------------------------

-- | Handler that redirects to the first show's episodes page
handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to dashboard episodes redirect" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (_, userMetadata)
      | not (UserMetadata.isHostOrHigher userMetadata.mUserRole) -> do
          Log.logInfo "User without Host role tried to access dashboard episodes" userMetadata.mDisplayName
          renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate
    Just (user, userMetadata) -> do
      -- Admins see all shows, hosts see their assigned shows
      showsResult <-
        if UserMetadata.isAdmin userMetadata.mUserRole
          then execQuerySpan Shows.getAllActiveShows
          else execQuerySpan (Shows.getShowsForUser (User.mId user))
      case showsResult of
        Left _err -> do
          Log.logAttention "Failed to fetch shows" (User.mId user)
          let banner = BannerParams Warning "Error" "Unable to load shows. Please try again."
          renderTemplate hxRequest (Just userMetadata) (redirectWithBanner [i|/#{hostDashboardGetUrl}|] banner)
        Right userShows ->
          case uncons userShows of
            Nothing -> do
              let banner = BannerParams Info "No Shows" "No shows available."
              renderTemplate hxRequest (Just userMetadata) (redirectWithBanner [i|/#{hostDashboardGetUrl}|] banner)
            Just (firstShow, _) -> do
              Log.logInfo "Redirecting to first show's episodes" firstShow.slug
              renderTemplate hxRequest (Just userMetadata) (redirectTemplate [i|/#{dashboardEpisodesGetUrl firstShow}|])
