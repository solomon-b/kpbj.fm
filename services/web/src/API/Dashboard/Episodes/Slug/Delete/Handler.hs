{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Slug.Delete.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Templates.EpisodeRow (renderEpisodeTableRow)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError (..), logHandlerError, throwDatabaseError, throwHandlerFailure, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Handler for archiving episodes (soft delete).
--
-- Only staff or higher roles can archive episodes. This allows admins to
-- remove content from public view while preserving the database record
-- for compliance, legal, or moderation purposes.
--
-- The response is the episode's row, which HTMX swaps in place. The row then
-- renders dimmed with the ARCHIVED badge, and it offers Unarchive. An empty
-- response would drop the row from the page, which was right when an archived
-- episode was invisible to everyone. Staff read this list to moderate, so the
-- row has to stay.
handler ::
  Slug ->
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler showSlug episodeNumber cookie =
  handleArchiveErrors $ do
    (_user, userMeta) <- requireAuth cookie
    requireStaffNotSuspended
      "Only staff members can archive episodes."
      userMeta
    (showModel, episode) <- action showSlug episodeNumber
    pure $ do
      renderEpisodeTableRow userMeta showModel episode
      renderBanner Success "Episode Archived" "The episode is off the public site."

-- | Business logic: fetch, verify, archive.
action ::
  Slug ->
  Episodes.EpisodeNumber ->
  ExceptT HandlerError AppM (Shows.Model, Episodes.Model)
action showSlug episodeNumber = do
  -- 1. Fetch show and episode
  episode <- fetchEpisode showSlug episodeNumber
  showModel <- fetchShow episode.showId

  -- 2. Execute archive
  execQuery (Episodes.deleteEpisode episode.id) >>= \case
    Left err -> do
      Log.logInfo "Archive failed: Database error" (Aeson.object ["error" .= show err, "episodeId" .= episode.id])
      throwDatabaseError err
    Right Nothing -> do
      Log.logInfo "Archive failed: Episode not found during archive" (Aeson.object ["episodeId" .= episode.id])
      throwHandlerFailure "Episode not found during archive operation."
    Right (Just archived) -> do
      Log.logInfo "Episode archived successfully" (Aeson.object ["episodeId" .= episode.id])
      pure (showModel, archived)

--------------------------------------------------------------------------------
-- Data Fetching

fetchEpisode ::
  Slug ->
  Episodes.EpisodeNumber ->
  ExceptT HandlerError AppM Episodes.Model
fetchEpisode showSlug episodeNumber =
  fromMaybeM (throwNotFound "Episode") $
    fromRightM throwDatabaseError $
      execQuery (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber Episodes.ExcludeArchived)

fetchShow :: Shows.Id -> ExceptT HandlerError AppM Shows.Model
fetchShow showId =
  fromMaybeM (throwNotFound "Show") $
    fromRightM throwDatabaseError $
      execQuery (Shows.getShowById showId)

--------------------------------------------------------------------------------
-- Error Handling

-- | Handle errors for archive operations.
--
-- Early errors (auth, not found) return just a banner since we don't have row context.
-- Late errors (during archive) are handled inline with row preservation.
handleArchiveErrors ::
  ExceptT HandlerError AppM (Lucid.Html ()) ->
  AppM (Lucid.Html ())
handleArchiveErrors archiveAction =
  runExceptT archiveAction >>= \case
    Right html -> pure html
    Left err -> do
      logHandlerError "Episode archive" err
      pure $ renderBanner Error "Archive Failed" (errorMessage err)

errorMessage :: HandlerError -> Text
errorMessage = \case
  NotAuthenticated -> "You must be logged in to archive episodes."
  NotAuthorized msg _ -> msg
  NotFound resource -> resource <> " not found."
  DatabaseError _ -> "Database error. Please try again or contact support."
  ValidationError msg -> msg
  UserSuspended -> "Your account is suspended."
  HandlerFailure msg -> msg
