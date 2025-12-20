{-# LANGUAGE OverloadedRecordDot #-}

module API.Shows.Slug.Episode.Delete.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Get.Templates.Episode (renderEpisodeTableRow)
import App.Common (getUserInfo)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
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
handler _tracer showSlug episodeNumber cookie = do
  -- Fetch the show by slug
  execQuerySpan (Shows.getShowBySlug showSlug) >>= \case
    Left err -> do
      Log.logInfo "Archive failed: Failed to fetch show" (Aeson.object ["error" .= show err])
      -- Can't render card without show, return just error banner
      pure $ renderBanner Error "Archive Failed" "Database error. Please try again or contact support."
    Right Nothing -> do
      Log.logInfo "Archive failed: Show not found" (Aeson.object ["showSlug" .= showSlug])
      pure $ renderBanner Error "Archive Failed" "Show not found."
    Right (Just showModel) -> do
      getUserInfo cookie >>= \case
        Nothing -> do
          Log.logInfo_ "No user session"
          -- Can't render card without episode, return simple error
          pure $ renderBanner Error "Archive Failed" "You must be logged in to archive episodes."
        Just (_user, userMeta) -> do
          execQuerySpan (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber) >>= \case
            Left err -> do
              Log.logInfo "Archive failed: Failed to fetch episode" (Aeson.object ["error" .= show err])
              pure $ renderBanner Error "Archive Failed" "Database error. Please try again or contact support."
            Right Nothing -> do
              Log.logInfo "Archive failed: Episode not found" (Aeson.object ["episodeNumber" .= episodeNumber])
              pure $ renderBanner Error "Archive Failed" "Episode not found."
            Right (Just episode) -> do
              -- Only staff or higher can archive episodes
              let isStaff = UserMetadata.isStaffOrHigher userMeta.mUserRole

              if isStaff && not (UserMetadata.isSuspended userMeta)
                then softDeleteEpisode userMeta showModel episode
                else do
                  Log.logInfo "Archive failed: Not authorized (staff+ required)" (Aeson.object ["userId" .= userMeta.mUserId, "episodeId" .= episode.id])
                  pure $ renderErrorWithRow userMeta showModel episode "Only staff members can archive episodes. Hosts can discard draft episodes instead."

softDeleteEpisode ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  UserMetadata.Model ->
  Shows.Model ->
  Episodes.Model ->
  m (Lucid.Html ())
softDeleteEpisode userMeta showModel episode = do
  execQuerySpan (Episodes.deleteEpisode episode.id) >>= \case
    Left err -> do
      Log.logInfo "Archive failed: Database error" (Aeson.object ["error" .= show err, "episodeId" .= episode.id])
      pure $ renderErrorWithRow userMeta showModel episode "Failed to archive episode due to a database error."
    Right Nothing -> do
      Log.logInfo "Archive failed: Episode not found during archive" (Aeson.object ["episodeId" .= episode.id])
      pure $ renderErrorWithRow userMeta showModel episode "Episode not found during archive operation."
    Right (Just _) -> do
      Log.logInfo "Episode archived successfully" (Aeson.object ["episodeId" .= episode.id])
      -- Return empty response to remove the row
      pure emptyResponse

-- | Render an error banner AND the episode row (to prevent row removal on error)
renderErrorWithRow :: UserMetadata.Model -> Shows.Model -> Episodes.Model -> Text -> Lucid.Html ()
renderErrorWithRow userMeta showModel episode errorMsg = do
  renderEpisodeTableRow userMeta showModel episode
  renderBanner Error "Archive Failed" errorMsg

-- | Empty response for successful deletes (row is removed by HTMX)
emptyResponse :: Lucid.Html ()
emptyResponse = ""
