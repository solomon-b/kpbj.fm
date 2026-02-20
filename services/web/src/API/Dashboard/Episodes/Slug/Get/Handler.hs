{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Slug.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Has (getter)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EpisodeTags qualified as EpisodeTags
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the episode detail page.
data EpisodeDetailViewData = EpisodeDetailViewData
  { edvUserMetadata :: UserMetadata.Model,
    edvAllShows :: [Shows.Model],
    edvShowModel :: Shows.Model,
    edvEpisode :: Episodes.Model,
    edvTracks :: [EpisodeTrack.Model],
    edvTags :: [EpisodeTags.Model],
    edvBackend :: StorageBackend
  }

-- | Business logic: fetch episode + show, authorization, fetch tracks/tags.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  Episodes.EpisodeNumber ->
  ExceptT HandlerError AppM EpisodeDetailViewData
action user userMetadata showSlug episodeNumber = do
  -- 1. Get storage backend
  backend <- asks getter

  -- 2. Fetch the episode
  episode <- fetchEpisodeOrNotFound showSlug episodeNumber

  -- 3. Fetch the show
  showModel <- fetchShowOrNotFound episode.showId

  -- 4. Verify user has access to this show
  requireShowAccess user userMetadata showModel

  -- 5. Fetch tracks and tags for the episode
  tracks <- fromRight [] <$> execQuery (EpisodeTrack.getTracksForEpisode episode.id)
  tags <- fromRight [] <$> execQuery (Episodes.getTagsForEpisode episode.id)

  -- 6. Get user's shows for sidebar
  userShows <- lift $ fetchShowsForUser user userMetadata

  pure
    EpisodeDetailViewData
      { edvUserMetadata = userMetadata,
        edvAllShows = userShows,
        edvShowModel = showModel,
        edvEpisode = episode,
        edvTracks = tracks,
        edvTags = tags,
        edvBackend = backend
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Slug ->
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showSlug episodeNumber cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Episode detail" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata showSlug episodeNumber
    let content = template vd.edvBackend vd.edvUserMetadata vd.edvShowModel vd.edvEpisode vd.edvTracks vd.edvTags
    lift $
      renderDashboardTemplate
        hxRequest
        vd.edvUserMetadata
        vd.edvAllShows
        (Just vd.edvShowModel)
        NavEpisodes
        Nothing
        Nothing
        content

-- | Fetch episode by show slug and episode number
fetchEpisodeOrNotFound ::
  Slug ->
  Episodes.EpisodeNumber ->
  ExceptT HandlerError AppM Episodes.Model
fetchEpisodeOrNotFound showSlug episodeNumber =
  fromMaybeM (throwNotFound "Episode") $
    fromRightM throwDatabaseError $
      execQuery (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber)

-- | Fetch show by ID
fetchShowOrNotFound ::
  Shows.Id ->
  ExceptT HandlerError AppM Shows.Model
fetchShowOrNotFound showId =
  fromMaybeM (throwNotFound "Show") $
    fromRightM throwDatabaseError $
      execQuery (Shows.getShowById showId)

-- | Verify user has access to the show (admin or assigned host)
requireShowAccess ::
  User.Model ->
  UserMetadata.Model ->
  Shows.Model ->
  ExceptT HandlerError AppM ()
requireShowAccess user userMetadata showModel =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then pure ()
    else do
      userShows <- fromRight [] <$> execQuery (Shows.getShowsForUser (User.mId user))
      if any (\s -> s.id == showModel.id) userShows
        then pure ()
        else throwNotAuthorized "You don't have access to this show." (Just userMetadata.mUserRole)

-- | Fetch shows for user based on role
fetchShowsForUser ::
  User.Model ->
  UserMetadata.Model ->
  AppM [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then fromRight [] <$> execQuery Shows.getAllActiveShows
    else fromRight [] <$> execQuery (Shows.getShowsForUser (User.mId user))
