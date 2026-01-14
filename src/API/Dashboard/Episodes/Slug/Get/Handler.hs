{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Slug.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Either (fromRight)
import Data.Has (Has, getter)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
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
    Has HSQL.Pool.Pool env,
    Has StorageBackend env
  ) =>
  Tracer ->
  Slug ->
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug episodeNumber cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Episode detail" apiLinks.rootGet $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Get storage backend
    backend <- asks getter

    -- 3. Fetch the episode
    episode <- fetchEpisodeOrNotFound showSlug episodeNumber

    -- 4. Fetch the show
    showModel <- fetchShowOrNotFound episode.showId

    -- 5. Verify user has access to this show
    requireShowAccess user userMetadata showModel

    -- 6. Fetch tracks and tags for the episode
    tracks <- fromRight [] <$> execQuerySpan (EpisodeTrack.getTracksForEpisode episode.id)
    tags <- fromRight [] <$> execQuerySpan (Episodes.getTagsForEpisode episode.id)

    -- 7. Get user's shows for sidebar
    userShows <- fetchShowsForUser user userMetadata

    -- 8. Render template
    let content = template backend userMetadata showModel episode tracks tags
    renderDashboardTemplate hxRequest userMetadata userShows (Just showModel) NavEpisodes Nothing Nothing content

-- | Fetch episode by show slug and episode number
fetchEpisodeOrNotFound ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    Has Tracer env
  ) =>
  Slug ->
  Episodes.EpisodeNumber ->
  m Episodes.Model
fetchEpisodeOrNotFound showSlug episodeNumber =
  execQuerySpan (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Episode"
    Right (Just episode) -> pure episode

-- | Fetch show by ID
fetchShowOrNotFound ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    Has Tracer env
  ) =>
  Shows.Id ->
  m Shows.Model
fetchShowOrNotFound showId =
  execQuerySpan (Shows.getShowById showId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just showModel) -> pure showModel

-- | Verify user has access to the show (admin or assigned host)
requireShowAccess ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    Has Tracer env
  ) =>
  User.Model ->
  UserMetadata.Model ->
  Shows.Model ->
  m ()
requireShowAccess user userMetadata showModel =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then pure ()
    else do
      userShows <- fromRight [] <$> execQuerySpan (Shows.getShowsForUser (User.mId user))
      if any (\s -> s.id == showModel.id) userShows
        then pure ()
        else throwNotAuthorized "You don't have access to this show."

-- | Fetch shows for user based on role
fetchShowsForUser ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    Has Tracer env
  ) =>
  User.Model ->
  UserMetadata.Model ->
  m [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then fromRight [] <$> execQuerySpan Shows.getAllActiveShows
    else fromRight [] <$> execQuerySpan (Shows.getShowsForUser (User.mId user))
