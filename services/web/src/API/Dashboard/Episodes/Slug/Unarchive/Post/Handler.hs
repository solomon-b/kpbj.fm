{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Slug.Unarchive.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Templates.EpisodeRow (renderEpisodeTableRow)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Maybe (isNothing)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Handler for restoring an archived episode.
--
-- This reverses the archive, so the episode returns to the public site. Only
-- staff or higher roles can run it, which matches the archive itself.
--
-- The response is the episode's dashboard row, which HTMX swaps in place. The
-- row then renders without the ARCHIVED badge and offers Archive again.
handler ::
  Slug ->
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler showSlug episodeNumber cookie =
  handleBannerErrors "Episode unarchive" $ do
    (_user, userMeta) <- requireAuth cookie
    requireStaffNotSuspended
      "Only staff members can unarchive episodes."
      userMeta
    (showModel, episode) <- action showSlug episodeNumber
    pure $ do
      renderEpisodeTableRow userMeta showModel episode
      renderBanner Success "Episode Unarchived" "The episode is public again."

-- | Business logic: fetch, verify, restore, and read the row back.
action ::
  Slug ->
  Episodes.EpisodeNumber ->
  ExceptT HandlerError AppM (Shows.Model, Episodes.Model)
action showSlug episodeNumber = do
  archived <- fetchArchivedEpisode showSlug episodeNumber
  showModel <- fetchShow archived.showId
  requireAirTimeIsFree archived

  execQuery (Episodes.restoreEpisode archived.id) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> do
      Log.logInfo "Unarchive failed: episode was not archived" (Aeson.object ["episodeId" .= archived.id])
      throwValidationError "This episode is not archived."
    Right (Just _) -> do
      Log.logInfo "Episode unarchived" (Aeson.object ["episodeId" .= archived.id])
      restored <- fetchRestoredEpisode archived.id
      pure (showModel, restored)

--------------------------------------------------------------------------------
-- Data Fetching

-- | Fetch the episode, and refuse when it is already public.
fetchArchivedEpisode ::
  Slug ->
  Episodes.EpisodeNumber ->
  ExceptT HandlerError AppM Episodes.Model
fetchArchivedEpisode showSlug episodeNumber = do
  episode <-
    fromMaybeM (throwNotFound "Episode") $
      fromRightM throwDatabaseError $
        execQuery (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber Episodes.IncludeArchived)
  if isNothing episode.deletedAt
    then throwValidationError "This episode is not archived."
    else pure episode

-- | Refuse the restore when another live episode took the air time.
--
-- @unique_episode_scheduled_at@ covers the live rows only, so a second episode
-- can claim the slot while this one sits archived. The index would reject the
-- restore, and staff would see a database error rather than the reason. An
-- episode with no air time cannot collide, because NULL values never match.
requireAirTimeIsFree :: Episodes.Model -> ExceptT HandlerError AppM ()
requireAirTimeIsFree episode = case episode.scheduledAt of
  Nothing -> pure ()
  Just airTime -> do
    mHolder <-
      fromRightM throwDatabaseError $
        execQuery (Episodes.getLiveEpisodeAtAirTime episode.showId airTime episode.id)
    case mHolder of
      Nothing -> pure ()
      Just holder ->
        throwValidationError $
          "Episode #"
            <> display holder.episodeNumber
            <> " now holds this air time. Reschedule one of them, then unarchive this episode."

fetchShow :: Shows.Id -> ExceptT HandlerError AppM Shows.Model
fetchShow showId =
  fromMaybeM (throwNotFound "Show") $
    fromRightM throwDatabaseError $
      execQuery (Shows.getShowById showId)

-- | Read the episode back so the row renders its new state.
fetchRestoredEpisode :: Episodes.Id -> ExceptT HandlerError AppM Episodes.Model
fetchRestoredEpisode episodeId =
  fromMaybeM (throwNotFound "Episode") $
    fromRightM throwDatabaseError $
      execQuery (Episodes.getEpisodeById episodeId)
