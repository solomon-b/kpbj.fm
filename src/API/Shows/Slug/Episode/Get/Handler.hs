{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Episode.Get.Handler where

--------------------------------------------------------------------------------

import API.Shows.Slug.Episode.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError (..), catchHandlerError, logHandlerError, throwDatabaseError, throwNotFound)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

-- | Handler for episode detail page by show slug and episode number.
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
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug episodeNumber cookie (foldHxReq -> hxRequest) = do
  userInfoResult <- getUserInfo cookie
  let mUserInfo = fmap snd userInfoResult
      mUserId = fmap (User.mId . fst) userInfoResult

  handleEpisodePageErrors hxRequest mUserInfo showSlug episodeNumber $ do
    -- 1. Fetch episode
    episode <- fetchEpisode showSlug episodeNumber

    -- 2. Fetch show and related data
    showModel <- fetchShow episode.showId
    tracks <- fromRight [] <$> execQuerySpan (EpisodeTrack.getTracksForEpisode episode.id)
    tags <- fromRight [] <$> execQuerySpan (Episodes.getTagsForEpisode episode.id)

    -- 3. Determine if user can view drafts
    canViewDrafts <- checkCanViewDrafts mUserInfo mUserId showModel.id

    -- 4. Render page
    let episodeTemplate = template showModel episode tracks tags canViewDrafts
    renderTemplate hxRequest mUserInfo episodeTemplate

--------------------------------------------------------------------------------
-- Data Fetching

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

fetchShow ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadThrow m,
    Has Tracer env
  ) =>
  Shows.Id ->
  m Shows.Model
fetchShow showId =
  execQuerySpan (Shows.getShowById showId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just showModel) -> pure showModel

checkCanViewDrafts ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    Has Tracer env
  ) =>
  Maybe UserMetadata.Model ->
  Maybe User.Id ->
  Shows.Id ->
  m Bool
checkCanViewDrafts mUserInfo mUserId showId = do
  let isStaffOrAdmin = case mUserInfo of
        Just metadata -> UserMetadata.isStaffOrHigher metadata.mUserRole
        Nothing -> False
  isHost <- case mUserId of
    Nothing -> pure False
    Just userId -> do
      result <- execQuerySpan (ShowHost.isUserHostOfShow userId showId)
      pure $ fromRight False result
  pure $ isHost || isStaffOrAdmin

--------------------------------------------------------------------------------
-- Error Handling

handleEpisodePageErrors ::
  (MonadCatch m, Log.MonadLog m) =>
  HxRequest ->
  Maybe UserMetadata.Model ->
  Slug ->
  Episodes.EpisodeNumber ->
  m (Lucid.Html ()) ->
  m (Lucid.Html ())
handleEpisodePageErrors hxRequest mUserInfo showSlug episodeNumber action =
  action `catchHandlerError` \err -> do
    logHandlerError "Episode page" err
    renderTemplate hxRequest mUserInfo $ case err of
      NotFound _ -> notFoundTemplate showSlug episodeNumber
      _ -> errorTemplate "Failed to load episode. Please try again."
