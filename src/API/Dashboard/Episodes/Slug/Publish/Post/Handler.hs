{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Episodes.Slug.Publish.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Templates.EpisodeRow (renderEpisodeTableRow)
import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (HandlerError (..), catchHandlerError, logHandlerError, throwDatabaseError, throwNotAuthorized, throwNotFound, throwValidationError)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Slug ->
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  m (Lucid.Html ())
handler _tracer showSlug episodeNumber cookie =
  handlePublishErrors $ do
    -- 1. Require authentication
    (user, userMeta) <- requireAuth cookie

    -- 2. Require show host or staff (not suspended)
    requireShowHostOrStaff user.mId showSlug userMeta

    -- 3. Fetch show and episode
    showModel <- fetchShow showSlug
    episode <- fetchEpisode showSlug episodeNumber

    -- 4. Validate episode can be published
    requireNotAlreadyPublished episode

    -- 5. Check past-dated episodes (staff only)
    currentTime <- liftIO getCurrentTime
    requireNotPastUnlessStaff currentTime episode userMeta

    -- 6. Execute publish
    publishEpisode showModel episode userMeta

--------------------------------------------------------------------------------
-- Data Fetching

fetchShow ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadThrow m,
    Has Tracer env
  ) =>
  Slug ->
  m Shows.Model
fetchShow showSlug =
  execQuerySpan (Shows.getShowBySlug showSlug) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just showModel) -> pure showModel

fetchEpisode ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadThrow m,
    Has Tracer env
  ) =>
  Slug ->
  Episodes.EpisodeNumber ->
  m Episodes.Model
fetchEpisode showSlug episodeNumber =
  execQuerySpan (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Episode"
    Right (Just episode) -> pure episode

--------------------------------------------------------------------------------
-- Validation

requireNotAlreadyPublished ::
  (MonadThrow m, Log.MonadLog m) =>
  Episodes.Model ->
  m ()
requireNotAlreadyPublished episode =
  when (episode.status == Episodes.Published) $ do
    Log.logInfo "Publish failed: Episode already published" (Aeson.object ["episodeId" .= episode.id])
    throwValidationError "This episode is already published."

requireNotPastUnlessStaff ::
  (MonadThrow m, Log.MonadLog m) =>
  UTCTime ->
  Episodes.Model ->
  UserMetadata.Model ->
  m ()
requireNotPastUnlessStaff currentTime episode userMeta = do
  let isPast = episode.scheduledAt <= currentTime
      isStaff = UserMetadata.isStaffOrHigher userMeta.mUserRole
  when (isPast && not isStaff) $ do
    Log.logInfo
      "Publish failed: Past episode, user not staff"
      (Aeson.object ["episodeId" .= episode.id])
    throwNotAuthorized "This episode's scheduled date has passed. Only staff or admin users can publish past episodes."

--------------------------------------------------------------------------------
-- Publish Execution

publishEpisode ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Shows.Model ->
  Episodes.Model ->
  UserMetadata.Model ->
  m (Lucid.Html ())
publishEpisode showModel episode userMeta = do
  execQuerySpan (Episodes.publishEpisode episode.id) >>= \case
    Left err -> do
      Log.logInfo "Publish failed: Database error" (Aeson.object ["error" .= show err, "episodeId" .= episode.id])
      pure $ renderErrorWithRow userMeta showModel episode "Failed to publish episode due to a database error."
    Right Nothing -> do
      Log.logInfo "Publish failed: Episode not found during publish" (Aeson.object ["episodeId" .= episode.id])
      pure $ renderErrorWithRow userMeta showModel episode "Episode not found during publish operation."
    Right (Just _) -> do
      Log.logInfo "Episode published successfully" (Aeson.object ["episodeId" .= episode.id])
      -- Return the updated row with Published status
      let publishedEpisode = episode {Episodes.status = Episodes.Published}
      pure $ renderEpisodeTableRow userMeta showModel publishedEpisode

--------------------------------------------------------------------------------
-- Error Handling

handlePublishErrors ::
  (MonadCatch m, Log.MonadLog m) =>
  m (Lucid.Html ()) ->
  m (Lucid.Html ())
handlePublishErrors action =
  action `catchHandlerError` \err -> do
    logHandlerError "Episode publish" err
    pure $ renderBanner Error "Publish Failed" (errorMessage err)

errorMessage :: HandlerError -> Text
errorMessage = \case
  NotAuthenticated -> "You must be logged in to publish episodes."
  NotAuthorized msg -> msg
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
  renderBanner Error "Publish Failed" errorMsg
