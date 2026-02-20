{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Slug.Delete.Handler where

--------------------------------------------------------------------------------

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
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Handler for archiving episodes (soft delete).
--
-- Only staff or higher roles can archive episodes. This allows admins to
-- remove content from public view while preserving the database record
-- for compliance, legal, or moderation purposes.
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
    action showSlug episodeNumber
    pure $ do
      emptyResponse
      renderBanner Success "Episode Archived" "The episode has been archived."

-- | Business logic: fetch, verify, archive.
action ::
  Slug ->
  Episodes.EpisodeNumber ->
  ExceptT HandlerError AppM ()
action showSlug episodeNumber = do
  -- 1. Fetch show and episode
  episode <- fetchEpisode showSlug episodeNumber

  -- 2. Execute archive
  execQuery (Episodes.deleteEpisode episode.id) >>= \case
    Left err -> do
      Log.logInfo "Archive failed: Database error" (Aeson.object ["error" .= show err, "episodeId" .= episode.id])
      throwDatabaseError err
    Right Nothing -> do
      Log.logInfo "Archive failed: Episode not found during archive" (Aeson.object ["episodeId" .= episode.id])
      throwHandlerFailure "Episode not found during archive operation."
    Right (Just _) ->
      Log.logInfo "Episode archived successfully" (Aeson.object ["episodeId" .= episode.id])

--------------------------------------------------------------------------------
-- Data Fetching

fetchEpisode ::
  Slug ->
  Episodes.EpisodeNumber ->
  ExceptT HandlerError AppM Episodes.Model
fetchEpisode showSlug episodeNumber =
  fromMaybeM (throwNotFound "Episode") $
    fromRightM throwDatabaseError $
      execQuery (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber)

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

-- | Empty response for successful deletes (row is removed by HTMX)
emptyResponse :: Lucid.Html ()
emptyResponse = ""
