{-# LANGUAGE OverloadedRecordDot #-}

module API.Shows.Slug.Episode.Delete where

--------------------------------------------------------------------------------

import API.Shows.Slug.Episode.Delete.Templates.ErrorBanner (emptyResponse, renderErrorBannerWithCard)
import App.Common (getUserInfo)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "DELETE /shows/:show_id/:show_slug/episodes/:episode_id/:episode_slug"
    ( "shows"
        :> Servant.Capture "show_id" Shows.Id
        :> Servant.Capture "show_slug" Slug
        :> "episodes"
        :> Servant.Capture "episode_id" Episodes.Id
        :> Servant.Capture "episode_slug" Slug
        :> Servant.Header "Cookie" Cookie
        :> Servant.Delete '[HTML] (Lucid.Html ())
    )

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
  Shows.Id ->
  Slug ->
  Episodes.Id ->
  Slug ->
  Maybe Cookie ->
  m (Lucid.Html ())
handler _tracer showId _showSlug episodeId _episodeSlug cookie = do
  -- Fetch the show by ID
  execQuerySpan (Shows.getShowById showId) >>= \case
    Left err -> do
      Log.logInfo "Archive failed: Failed to fetch show" (Aeson.object ["error" .= show err])
      -- Can't render card without show, return just error banner
      pure $ renderBanner Error "Archive Failed" "Database error. Please try again or contact support."
    Right Nothing -> do
      Log.logInfo "Archive failed: Show not found" (Aeson.object ["showId" .= showId])
      pure $ renderBanner Error "Archive Failed" "Show not found."
    Right (Just showModel) -> do
      getUserInfo cookie >>= \case
        Nothing -> do
          Log.logInfo_ "No user session"
          -- Can't render card without episode, return simple error
          pure $ renderBanner Error "Archive Failed" "You must be logged in to archive episodes."
        Just (user, userMeta) -> do
          execQuerySpan (Episodes.getEpisodeById episodeId) >>= \case
            Left err -> do
              Log.logInfo "Archive failed: Failed to fetch episode" (Aeson.object ["error" .= show err])
              pure $ renderBanner Error "Archive Failed" "Database error. Please try again or contact support."
            Right Nothing -> do
              Log.logInfo "Archive failed: Episode not found" (Aeson.object ["episodeId" .= episodeId])
              pure $ renderBanner Error "Archive Failed" "Episode not found."
            Right (Just episode) -> do
              -- Check authorization: staff, creator, or host
              let isStaff = UserMetadata.isStaffOrHigher userMeta.mUserRole
                  isCreator = episode.createdBy == user.mId

              isHost <- if isStaff || isCreator then pure True else checkIfHost user episode

              if isStaff || ((isCreator || isHost) && not (UserMetadata.isSuspended userMeta))
                then softDeleteEpisode showModel episode
                else do
                  Log.logInfo "Delete failed: Not authorized" (Aeson.object ["userId" .= user.mId, "episodeId" .= episode.id])
                  pure $ renderErrorBannerWithCard showModel episode "You don't have permission to delete this episode."

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
  Shows.Model ->
  Episodes.Model ->
  m (Lucid.Html ())
softDeleteEpisode showModel episode = do
  execQuerySpan (Episodes.deleteEpisode episode.id) >>= \case
    Left err -> do
      Log.logInfo "Delete failed: Database error" (Aeson.object ["error" .= show err, "episodeId" .= episode.id])
      pure $ renderErrorBannerWithCard showModel episode "Failed to delete episode due to a database error."
    Right Nothing -> do
      Log.logInfo "Delete failed: Episode not found during delete" (Aeson.object ["episodeId" .= episode.id])
      pure $ renderErrorBannerWithCard showModel episode "Episode not found during delete operation."
    Right (Just _) -> do
      Log.logInfo "Episode deleted successfully" (Aeson.object ["episodeId" .= episode.id])
      pure emptyResponse

checkIfHost ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  User.Model ->
  Episodes.Model ->
  m Bool
checkIfHost user episode = do
  result <- execQuerySpan (Episodes.isUserHostOfEpisodeShow user.mId episode.id)
  case result of
    Left _ -> pure False
    Right authorized -> pure authorized
