{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Slug.Episode.New.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Episode.New.Get.Templates.Form (episodeUploadForm)
import API.Links (apiLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Config (Environment)
import App.Domains (audioUploadUrl)
import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Has qualified as Has
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the new episode upload form.
data EpisodeNewGetViewData = EpisodeNewGetViewData
  { engvUserMetadata :: UserMetadata.Model,
    engvAllShows :: [Shows.Model],
    engvShowModel :: Shows.Model,
    engvUpcomingDates :: [ShowSchedule.UpcomingShowDate],
    engvUploadUrl :: Text
  }

-- | Business logic: fetch show, fetch upcoming dates.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  ExceptT HandlerError AppM EpisodeNewGetViewData
action user userMetadata showSlug = do
  -- 1. Fetch shows for sidebar (admins see all, hosts see their own)
  allShows <- lift $ fetchShowsForUser user userMetadata

  -- 2. Fetch the show
  showModel <- fetchShowOrNotFound showSlug

  -- 3. Fetch upcoming dates for the show
  upcomingDates <- lift $ fetchUpcomingDates showModel.id

  -- 4. Get upload URL (bypasses Cloudflare in production)
  env <- asks (Has.getter @Environment)
  let uploadUrl = audioUploadUrl env

  pure
    EpisodeNewGetViewData
      { engvUserMetadata = userMetadata,
        engvAllShows = allShows,
        engvShowModel = showModel,
        engvUpcomingDates = upcomingDates,
        engvUploadUrl = uploadUrl
      }

handler ::
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showSlug cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Episode upload form" apiLinks.rootGet $ do
    -- 1. Require authentication and authorization (host of show or staff+)
    (user, userMetadata) <- requireAuth cookie
    requireShowHostOrStaff user.mId showSlug userMetadata
    vd <- action user userMetadata showSlug
    let content = episodeUploadForm vd.engvUploadUrl vd.engvShowModel vd.engvUpcomingDates vd.engvUserMetadata
    lift $ renderDashboardTemplate hxRequest vd.engvUserMetadata vd.engvAllShows (Just vd.engvShowModel) NavEpisodes Nothing Nothing content

-- | Fetch shows based on user role (admins see all, hosts see their own)
fetchShowsForUser ::
  User.Model ->
  UserMetadata.Model ->
  AppM [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isStaffOrHigher userMetadata.mUserRole
    then fromRight [] <$> execQuery Shows.getAllActiveShows
    else fromRight [] <$> execQuery (Shows.getShowsForUser (User.mId user))

-- | Fetch show by slug or throw NotFound
fetchShowOrNotFound ::
  Slug ->
  ExceptT HandlerError AppM Shows.Model
fetchShowOrNotFound showSlug =
  fromMaybeM (throwNotFound "Show") $
    fromRightM throwDatabaseError $
      execQuery (Shows.getShowBySlug showSlug)

-- | Fetch upcoming unscheduled dates for a show
fetchUpcomingDates ::
  Shows.Id ->
  AppM [ShowSchedule.UpcomingShowDate]
fetchUpcomingDates showId =
  fromRight [] <$> execQuery (ShowSchedule.getUpcomingUnscheduledShowDates showId 4)
