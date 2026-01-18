{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Episode.Get.Handler where

--------------------------------------------------------------------------------

import API.Shows.Slug.Episode.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError (..), catchHandlerError, logHandlerError, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Control.Monad.Reader (asks)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Has (getter)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

-- | Handler for episode detail page by show slug and episode number.
handler ::
  Tracer ->
  Slug ->
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer showSlug episodeNumber cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie <&> fmap snd
  backend <- asks getter

  handleEpisodePageErrors hxRequest mUserInfo showSlug episodeNumber $ do
    -- 1. Fetch episode
    episode <- fetchEpisode showSlug episodeNumber

    -- 2. Fetch show and related data
    showModel <- fetchShow episode.showId
    tracks <- fromRight [] <$> execQuerySpan (EpisodeTrack.getTracksForEpisode episode.id)
    tags <- fromRight [] <$> execQuerySpan (Episodes.getTagsForEpisode episode.id)

    -- 3. Render page
    let episodeTemplate = template backend showModel episode tracks tags
    renderTemplate hxRequest mUserInfo episodeTemplate

--------------------------------------------------------------------------------
-- Data Fetching

fetchEpisode ::
  Slug ->
  Episodes.EpisodeNumber ->
  AppM Episodes.Model
fetchEpisode showSlug episodeNumber =
  execQuerySpan (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Episode"
    Right (Just episode) -> pure episode

fetchShow ::
  Shows.Id ->
  AppM Shows.Model
fetchShow showId =
  execQuerySpan (Shows.getShowById showId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just showModel) -> pure showModel

--------------------------------------------------------------------------------
-- Error Handling

handleEpisodePageErrors ::
  HxRequest ->
  Maybe UserMetadata.Model ->
  Slug ->
  Episodes.EpisodeNumber ->
  AppM (Lucid.Html ()) ->
  AppM (Lucid.Html ())
handleEpisodePageErrors hxRequest mUserInfo showSlug episodeNumber action =
  action `catchHandlerError` \err -> do
    logHandlerError "Episode page" err
    renderTemplate hxRequest mUserInfo $ case err of
      NotFound _ -> notFoundTemplate showSlug episodeNumber
      _ -> errorTemplate "Failed to load episode. Please try again."
