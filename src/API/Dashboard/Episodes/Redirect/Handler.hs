{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Redirect.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (apiLinks, dashboardEpisodesLinks)
import API.Types
import App.Common (renderTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (handleHtmlErrors)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectTemplate, redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
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
hostDashboardGetUrl = Links.linkURI apiLinks.rootGet

dashboardEpisodesGetUrl :: Shows.Model -> Links.URI
dashboardEpisodesGetUrl showModel = Links.linkURI $ dashboardEpisodesLinks.list showModel.slug Nothing

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
handler _tracer cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Episodes redirect" apiLinks.rootGet $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Fetch shows (admins see all, hosts see their own)
    userShows <- fetchShowsForUser user userMetadata

    -- 3. Redirect to first show's episodes page
    case uncons userShows of
      Nothing -> do
        let banner = BannerParams Info "No Shows" "No shows available."
        renderTemplate hxRequest (Just userMetadata) (redirectWithBanner [i|/#{hostDashboardGetUrl}|] banner)
      Just (firstShow, _) -> do
        Log.logInfo "Redirecting to first show's episodes" firstShow.slug
        renderTemplate hxRequest (Just userMetadata) (redirectTemplate [i|/#{dashboardEpisodesGetUrl firstShow}|])

-- | Fetch shows based on user role (admins see all, hosts see their own)
fetchShowsForUser ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    Has Tracer env
  ) =>
  User.Model ->
  UserMetadata.Model ->
  m [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then fromRight [] <$> execQuerySpan Shows.getAllActiveShows
    else fromRight [] <$> execQuerySpan (Shows.getShowsForUser (User.mId user))
