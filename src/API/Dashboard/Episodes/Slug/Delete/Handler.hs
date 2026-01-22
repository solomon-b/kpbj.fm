{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Slug.Delete.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Templates.EpisodeRow (renderEpisodeTableRow)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError (..), catchHandlerError, logHandlerError, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

-- | Handler for archiving episodes (soft delete).
--
-- Only staff or higher roles can archive episodes. This allows admins to
-- remove content from public view while preserving the database record
-- for compliance, legal, or moderation purposes.
handler ::
  Tracer ->
  Slug ->
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler _tracer showSlug episodeNumber cookie =
  handleArchiveErrors $ do
    -- 1. Require authentication
    (_user, userMeta) <- requireAuth cookie

    -- 2. Require staff role (not suspended)
    requireStaffNotSuspended
      "Only staff members can archive episodes. Hosts can discard draft episodes instead."
      userMeta

    -- 3. Fetch show and episode
    showModel <- fetchShow showSlug
    episode <- fetchEpisode showSlug episodeNumber

    -- 4. Execute archive
    archiveEpisode showModel episode userMeta

--------------------------------------------------------------------------------
-- Data Fetching

fetchShow ::
  Slug ->
  AppM Shows.Model
fetchShow showSlug =
  execQuerySpan (Shows.getShowBySlug showSlug) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just showModel) -> pure showModel

fetchEpisode ::
  Slug ->
  Episodes.EpisodeNumber ->
  AppM Episodes.Model
fetchEpisode showSlug episodeNumber =
  execQuerySpan (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Episode"
    Right (Just episode) -> pure episode

--------------------------------------------------------------------------------
-- Archive Execution

archiveEpisode ::
  Shows.Model ->
  Episodes.Model ->
  UserMetadata.Model ->
  AppM (Lucid.Html ())
archiveEpisode showModel episode userMeta = do
  execQuerySpan (Episodes.deleteEpisode episode.id) >>= \case
    Left err -> do
      Log.logInfo "Archive failed: Database error" (Aeson.object ["error" .= show err, "episodeId" .= episode.id])
      pure $ renderErrorWithRow userMeta showModel episode "Failed to archive episode due to a database error."
    Right Nothing -> do
      Log.logInfo "Archive failed: Episode not found during archive" (Aeson.object ["episodeId" .= episode.id])
      pure $ renderErrorWithRow userMeta showModel episode "Episode not found during archive operation."
    Right (Just _) -> do
      Log.logInfo "Episode archived successfully" (Aeson.object ["episodeId" .= episode.id])
      pure $ do
        emptyResponse
        renderBanner Success "Episode Archived" "The episode has been archived."

--------------------------------------------------------------------------------
-- Error Handling

-- | Handle errors for archive operations.
--
-- Early errors (auth, not found) return just a banner since we don't have row context.
-- Late errors (during archive) are handled inline with row preservation.
handleArchiveErrors ::
  AppM (Lucid.Html ()) ->
  AppM (Lucid.Html ())
handleArchiveErrors action =
  action `catchHandlerError` \err -> do
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

--------------------------------------------------------------------------------
-- Rendering Helpers

-- | Render an error banner AND the episode row (to prevent row removal on error)
renderErrorWithRow :: UserMetadata.Model -> Shows.Model -> Episodes.Model -> Text -> Lucid.Html ()
renderErrorWithRow userMeta showModel episode errorMsg = do
  renderEpisodeTableRow userMeta showModel episode
  renderBanner Error "Archive Failed" errorMsg

-- | Empty response for successful deletes (row is removed by HTMX)
emptyResponse :: Lucid.Html ()
emptyResponse = ""
