{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Slug.Edit.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Edit.Get.Templates.Form (EpisodeEditContext (..), template)
import API.Links (dashboardEpisodesLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Config (Environment)
import App.Domains (audioUploadUrl)
import App.Handler.Combinators (requireAuth)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Has (getter)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (getCurrentTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.EpisodeTags qualified as EpisodeTags
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Servant qualified

--------------------------------------------------------------------------------

handler ::
  Slug ->
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler showSlug episodeNumber cookie (foldHxReq -> hxRequest) =
  handleRedirectErrors "Episode edit" (dashboardEpisodesLinks.list showSlug Nothing) $ do
    -- 1. Require authentication
    (user, userMetadata) <- requireAuth cookie

    -- 2. Get storage backend
    backend <- asks getter

    -- 3. Fetch episode context (episode, show, tracks, isHost) in transaction
    (episode, showModel, tracks, isHost) <- fetchEpisodeContext showSlug episodeNumber user userMetadata

    -- 4. Check authorization
    let isAuthorized = episode.createdBy == user.mId || isHost || UserMetadata.isStaffOrHigher userMetadata.mUserRole
    unless isAuthorized $
      throwNotAuthorized "You can only edit your own episodes." (Just userMetadata.mUserRole)

    -- 5. Fetch additional data for the edit form
    Log.logInfo "Authorized user accessing episode edit form" episode.id
    currentTime <- liftIO getCurrentTime

    episodeTags <- fetchEpisodeTags episode.id
    mCurrentSlot <- fetchCurrentSlot episode
    upcomingDates <- fetchUpcomingDates showModel.id
    allShows <- fetchUserShows user userMetadata

    -- 6. Get upload URL (bypasses Cloudflare in production)
    env <- asks (Has.getter @Environment)
    let uploadUrl = audioUploadUrl env

    -- 7. Render the edit form
    let editContext =
          EpisodeEditContext
            { eecUploadUrl = uploadUrl,
              eecBackend = backend,
              eecCurrentTime = currentTime,
              eecShow = showModel,
              eecEpisode = episode,
              eecTracks = tracks,
              eecTags = episodeTags,
              eecCurrentSlot = mCurrentSlot,
              eecUpcomingDates = upcomingDates,
              eecIsStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
            }
        editTemplate = template editContext
        statsContent = Lucid.span_ [] $ Lucid.toHtml $ "Episode #" <> display episode.episodeNumber

    html <- renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavEpisodes (Just statsContent) Nothing editTemplate
    pure $ Servant.noHeader html

--------------------------------------------------------------------------------
-- Inline Helpers

-- | Context returned from the initial episode fetch
type EpisodeContext = (Episodes.Model, Shows.Model, [EpisodeTrack.Model], Bool)

fetchEpisodeContext ::
  Slug ->
  Episodes.EpisodeNumber ->
  User.Model ->
  UserMetadata.Model ->
  AppM EpisodeContext
fetchEpisodeContext showSlug episodeNumber user userMetadata = do
  mResult <- execTransaction $ runMaybeT $ do
    episode <- MaybeT $ HT.statement () (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber)
    showResult <- MaybeT $ HT.statement () (Shows.getShowById episode.showId)
    tracks <- lift $ HT.statement () (EpisodeTrack.getTracksForEpisode episode.id)
    isHost <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then pure True
        else lift $ HT.statement () (ShowHost.isUserHostOfShow user.mId episode.showId)
    MaybeT $ pure $ Just (episode, showResult, tracks, isHost)
  case mResult of
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Episode"
    Right (Just ctx) -> pure ctx

fetchEpisodeTags ::
  Episodes.Id ->
  AppM [EpisodeTags.Model]
fetchEpisodeTags episodeId =
  execQuery (Episodes.getTagsForEpisode episodeId) >>= \case
    Left err -> do
      Log.logAttention "Failed to fetch episode tags" (show err)
      pure []
    Right tags -> pure tags

fetchCurrentSlot ::
  Episodes.Model ->
  AppM (Maybe ShowSchedule.UpcomingShowDate)
fetchCurrentSlot episode =
  execQuery (ShowSchedule.getScheduleTemplateById episode.scheduleTemplateId) >>= \case
    Left err -> do
      Log.logAttention "Failed to fetch schedule template" (show err)
      pure Nothing
    Right Nothing -> pure Nothing
    Right (Just scheduleTemplate) ->
      pure $ Just $ ShowSchedule.makeUpcomingShowDateFromTemplate scheduleTemplate episode.scheduledAt

fetchUpcomingDates ::
  Shows.Id ->
  AppM [ShowSchedule.UpcomingShowDate]
fetchUpcomingDates showId =
  execQuery (ShowSchedule.getUpcomingUnscheduledShowDates showId (Limit 52)) >>= \case
    Left err -> do
      Log.logAttention "Failed to fetch upcoming dates" (show err)
      pure []
    Right dates -> pure dates

fetchUserShows ::
  User.Model ->
  UserMetadata.Model ->
  AppM [Shows.Model]
fetchUserShows user userMetadata = do
  let query =
        if UserMetadata.isAdmin userMetadata.mUserRole
          then Shows.getAllActiveShows
          else Shows.getShowsForUser user.mId
  execQuery query >>= \case
    Left err -> do
      Log.logAttention "Failed to fetch user shows" (show err)
      pure []
    Right result -> pure result
