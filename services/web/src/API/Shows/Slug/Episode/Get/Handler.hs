{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Episode.Get.Handler where

--------------------------------------------------------------------------------

import API.Shows.Slug.Episode.Get.Templates.Page (template)
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError, handlePublicErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Has (getter)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EpisodeTags qualified as EpisodeTags
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

data EpisodeViewData = EpisodeViewData
  { evdStorageBackend :: StorageBackend,
    evdShowModel :: Shows.Model,
    evdEpisode :: Episodes.Model,
    evdTracks :: [EpisodeTrack.Model],
    evdTags :: [EpisodeTags.Model]
  }

--------------------------------------------------------------------------------

-- | Handler for episode detail page by show slug and episode number.
handler ::
  Slug ->
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showSlug episodeNumber cookie (foldHxReq -> hxRequest) =
  handlePublicErrors "Episode detail" renderError $ do
    mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
    vd <- action showSlug episodeNumber
    lift $
      renderTemplate hxRequest mUserInfo $
        template
          vd.evdStorageBackend
          vd.evdShowModel
          vd.evdEpisode
          vd.evdTracks
          vd.evdTags
  where
    renderError content = do
      mUserInfo <- getUserInfo cookie <&> fmap snd
      renderTemplate hxRequest mUserInfo content

--------------------------------------------------------------------------------

-- | Business logic: fetch episode and all related details.
action ::
  Slug ->
  Episodes.EpisodeNumber ->
  ExceptT HandlerError AppM EpisodeViewData
action showSlug episodeNumber = do
  backend <- asks getter
  episode <- fetchEpisode showSlug episodeNumber
  showModel <- fetchShow episode.showId
  tracks <- fromRight [] <$> execQuery (EpisodeTrack.getTracksForEpisode episode.id)
  tags <- fromRight [] <$> execQuery (Episodes.getTagsForEpisode episode.id)
  pure
    EpisodeViewData
      { evdStorageBackend = backend,
        evdShowModel = showModel,
        evdEpisode = episode,
        evdTracks = tracks,
        evdTags = tags
      }

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

fetchShow ::
  Shows.Id ->
  ExceptT HandlerError AppM Shows.Model
fetchShow showId =
  fromMaybeM (throwNotFound "Show") $
    fromRightM throwDatabaseError $
      execQuery (Shows.getShowById showId)
