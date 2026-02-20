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
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
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
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Slug ->
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler showSlug episodeNumber cookie (foldHxReq -> hxRequest) =
  handleRedirectErrors "Episode edit" (dashboardEpisodesLinks.list showSlug Nothing) $ do
    (user, userMetadata) <- requireAuth cookie
    vd <- action user userMetadata showSlug episodeNumber
    let statsContent = Lucid.span_ [] $ Lucid.toHtml $ "Episode #" <> display vd.eevEditContext.eecEpisode.episodeNumber
    html <-
      lift $
        renderDashboardTemplate
          hxRequest
          vd.eevUserMetadata
          vd.eevAllShows
          (Just vd.eevShowModel)
          NavEpisodes
          (Just statsContent)
          Nothing
          (template vd.eevEditContext)
    pure $ Servant.noHeader html

--------------------------------------------------------------------------------

-- | All data needed to render the episode edit page.
data EpisodeEditViewData = EpisodeEditViewData
  { eevEditContext :: EpisodeEditContext,
    eevUserMetadata :: UserMetadata.Model,
    eevAllShows :: [Shows.Model],
    eevShowModel :: Shows.Model
  }

-- | Business logic: fetch episode context, authorization, build edit context.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  Episodes.EpisodeNumber ->
  ExceptT HandlerError AppM EpisodeEditViewData
action user userMetadata showSlug episodeNumber = do
  -- 1. Get storage backend
  backend <- asks getter

  -- 2. Fetch episode context (episode, show, tracks, isHost) in transaction
  (episode, showModel, tracks, isHost) <- fetchEpisodeContext showSlug episodeNumber user userMetadata

  -- 3. Check authorization
  let isAuthorized = episode.createdBy == user.mId || isHost || UserMetadata.isStaffOrHigher userMetadata.mUserRole
  unless isAuthorized $
    throwNotAuthorized "You can only edit your own episodes." (Just userMetadata.mUserRole)

  -- 4. Fetch additional data for the edit form
  Log.logInfo "Authorized user accessing episode edit form" episode.id
  currentTime <- liftIO getCurrentTime

  episodeTags <- lift $ fetchEpisodeTags episode.id
  mCurrentSlot <- lift $ fetchCurrentSlot episode
  upcomingDates <- lift $ fetchUpcomingDates showModel.id
  allShows <- lift $ fetchUserShows user userMetadata

  -- 5. Get upload URL (bypasses Cloudflare in production)
  env <- asks (Has.getter @Environment)
  let uploadUrl = audioUploadUrl env

  -- 6. Build view data
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

  pure
    EpisodeEditViewData
      { eevEditContext = editContext,
        eevUserMetadata = userMetadata,
        eevAllShows = allShows,
        eevShowModel = showModel
      }

--------------------------------------------------------------------------------
-- Inline Helpers

-- | Context returned from the initial episode fetch
type EpisodeContext = (Episodes.Model, Shows.Model, [EpisodeTrack.Model], Bool)

fetchEpisodeContext ::
  Slug ->
  Episodes.EpisodeNumber ->
  User.Model ->
  UserMetadata.Model ->
  ExceptT HandlerError AppM EpisodeContext
fetchEpisodeContext showSlug episodeNumber user userMetadata = do
  mResult <-
    fromRightM throwDatabaseError $
      execTransaction $
        runMaybeT $ do
          episode <- MaybeT $ HT.statement () (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber)
          showResult <- MaybeT $ HT.statement () (Shows.getShowById episode.showId)
          tracks <- lift $ HT.statement () (EpisodeTrack.getTracksForEpisode episode.id)
          isHost <-
            if UserMetadata.isAdmin userMetadata.mUserRole
              then pure True
              else lift $ HT.statement () (ShowHost.isUserHostOfShow user.mId episode.showId)
          MaybeT $ pure $ Just (episode, showResult, tracks, isHost)
  case mResult of
    Nothing -> throwNotFound "Episode"
    Just ctx -> pure ctx

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
