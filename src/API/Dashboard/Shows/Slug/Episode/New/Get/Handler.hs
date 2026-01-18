{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Slug.Episode.New.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Episode.New.Get.Templates.Form (episodeUploadForm)
import API.Links (apiLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Data.Either (fromRight)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer showSlug cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Episode upload form" apiLinks.rootGet $ do
    -- 1. Require authentication and authorization (host of show or staff+)
    (user, userMetadata) <- requireAuth cookie
    requireShowHostOrStaff user.mId showSlug userMetadata

    -- 2. Fetch shows for sidebar (admins see all, hosts see their own)
    allShows <- fetchShowsForUser user userMetadata

    -- 3. Fetch the show
    showModel <- fetchShowOrNotFound showSlug

    -- 4. Fetch upcoming dates for the show
    upcomingDates <- fetchUpcomingDates showModel.id

    -- 5. Render the form
    let content = episodeUploadForm showModel upcomingDates userMetadata
    renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavEpisodes Nothing Nothing content

-- | Fetch shows based on user role (admins see all, hosts see their own)
fetchShowsForUser ::
  User.Model ->
  UserMetadata.Model ->
  AppM [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isStaffOrHigher userMetadata.mUserRole
    then fromRight [] <$> execQuerySpan Shows.getAllActiveShows
    else fromRight [] <$> execQuerySpan (Shows.getShowsForUser (User.mId user))

-- | Fetch show by slug or throw NotFound
fetchShowOrNotFound ::
  Slug ->
  AppM Shows.Model
fetchShowOrNotFound showSlug =
  execQuerySpan (Shows.getShowBySlug showSlug) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just showModel) -> pure showModel

-- | Fetch upcoming unscheduled dates for a show
fetchUpcomingDates ::
  Shows.Id ->
  AppM [ShowSchedule.UpcomingShowDate]
fetchUpcomingDates showId =
  fromRight [] <$> execQuerySpan (ShowSchedule.getUpcomingUnscheduledShowDates showId 4)
