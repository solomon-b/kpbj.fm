{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Slug.Episode.New.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Episode.New.Get.Templates.Form (episodeUploadForm)
import API.Links (apiLinks)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Config (Environment)
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Domains (audioUploadUrl)
import App.Handler.Combinators (authorizeShow)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Has qualified as Has
import Data.Text (Text)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the new episode upload form.
data EpisodeNewGetViewData = EpisodeNewGetViewData
  { engvUserMetadata :: UserMetadata.Model,
    engvShowModel :: Shows.Model,
    engvUpcomingDates :: [ShowSchedule.UpcomingShowDate],
    engvUploadUrl :: Text
  }

-- | Business logic: fetch show, fetch upcoming dates.
action ::
  UserMetadata.Model ->
  Slug ->
  ExceptT HandlerError AppM EpisodeNewGetViewData
action userMetadata showSlug = do
  -- 1. Fetch the show
  showModel <- fetchShowOrNotFound showSlug

  -- 2. Fetch upcoming dates for the show
  upcomingDates <- lift $ fetchUpcomingDates showModel.id

  -- 3. Get upload URL (bypasses Cloudflare in production)
  env <- asks (Has.getter @Environment)
  let uploadUrl = audioUploadUrl env

  pure
    EpisodeNewGetViewData
      { engvUserMetadata = userMetadata,
        engvShowModel = showModel,
        engvUpcomingDates = upcomingDates,
        engvUploadUrl = uploadUrl
      }

handler ::
  Slug ->
  Satisfies 'UserMetadata.Host RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showSlug host@(Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Episode upload form" apiLinks.rootGet $ do
    void $ authorizeShow host showSlug
    vd <- action auth.raMetadata showSlug
    let content = episodeUploadForm vd.engvUploadUrl vd.engvShowModel vd.engvUpcomingDates vd.engvUserMetadata
    lift $ renderDashboardTemplate hxRequest vd.engvUserMetadata (ForShow vd.engvShowModel) NavEpisodes Nothing Nothing content

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
