{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Slug.DiscardDraft.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Templates.EpisodeRow (renderEpisodeTableRow)
import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (HandlerError (..), catchHandlerError, logHandlerError, throwDatabaseError, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (when)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as Txn
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

-- | Handler for discarding draft episodes.
--
-- Only draft episodes can be discarded. Hosts can discard drafts for shows
-- they are assigned to, or drafts they created. This performs a hard delete.
handler ::
  Slug ->
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler showSlug episodeNumber cookie =
  handleDiscardErrors $ do
    -- 1. Require authentication
    (user, userMeta) <- requireAuth cookie

    -- 2. Require show host or staff (not suspended)
    requireShowHostOrStaff user.mId showSlug userMeta

    -- 3. Fetch show and episode
    showModel <- fetchShow showSlug
    episode <- fetchEpisode showSlug episodeNumber

    -- 4. Validate episode is a draft
    requireDraftStatus episode

    -- 5. Execute hard delete
    hardDeleteEpisode showModel episode userMeta

--------------------------------------------------------------------------------
-- Data Fetching

fetchShow ::
  Slug ->
  AppM Shows.Model
fetchShow showSlug =
  execQuery (Shows.getShowBySlug showSlug) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just showModel) -> pure showModel

fetchEpisode ::
  Slug ->
  Episodes.EpisodeNumber ->
  AppM Episodes.Model
fetchEpisode showSlug episodeNumber =
  execQuery (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Episode"
    Right (Just episode) -> pure episode

--------------------------------------------------------------------------------
-- Validation

requireDraftStatus ::
  Episodes.Model ->
  AppM ()
requireDraftStatus episode =
  when (episode.status /= Episodes.Draft) $ do
    Log.logInfo
      "Discard draft failed: Episode is not a draft"
      (Aeson.object ["episodeId" .= episode.id, "status" .= show episode.status])
    throwValidationError "Only draft episodes can be discarded. Published episodes must be archived by staff."

--------------------------------------------------------------------------------
-- Delete Execution

hardDeleteEpisode ::
  Shows.Model ->
  Episodes.Model ->
  UserMetadata.Model ->
  AppM (Lucid.Html ())
hardDeleteEpisode showModel episode userMeta = do
  execTransaction (deleteEpisodeTransaction episode.id) >>= \case
    Left err -> do
      Log.logInfo "Discard draft failed: Database error" (Aeson.object ["error" .= show err, "episodeId" .= episode.id])
      pure $ renderErrorWithRow userMeta showModel episode "Failed to discard episode due to a database error."
    Right Nothing -> do
      Log.logInfo "Discard draft failed: Episode not found during delete" (Aeson.object ["episodeId" .= episode.id])
      pure $ renderErrorWithRow userMeta showModel episode "Episode not found during discard operation."
    Right (Just _) -> do
      Log.logInfo "Draft episode discarded successfully" (Aeson.object ["episodeId" .= episode.id])
      pure $ do
        emptyResponse
        renderBanner Success "Draft Discarded" "The draft episode has been discarded."

-- | Transaction to delete episode tracks and then the episode itself.
deleteEpisodeTransaction :: Episodes.Id -> Txn.Transaction (Maybe Episodes.Id)
deleteEpisodeTransaction episodeId = do
  -- First delete all tracks for this episode
  _ <- Txn.statement () (EpisodeTrack.deleteAllTracksForEpisode episodeId)
  -- Then hard delete the episode
  Txn.statement () (Episodes.hardDeleteEpisode episodeId)

--------------------------------------------------------------------------------
-- Error Handling

handleDiscardErrors ::
  AppM (Lucid.Html ()) ->
  AppM (Lucid.Html ())
handleDiscardErrors action =
  action `catchHandlerError` \err -> do
    logHandlerError "Episode discard" err
    pure $ renderBanner Error "Discard Failed" (errorMessage err)

errorMessage :: HandlerError -> Text
errorMessage = \case
  NotAuthenticated -> "You must be logged in to discard episodes."
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
  renderBanner Error "Discard Failed" errorMsg

-- | Empty response for successful deletes (row is removed by HTMX)
emptyResponse :: Lucid.Html ()
emptyResponse = ""
