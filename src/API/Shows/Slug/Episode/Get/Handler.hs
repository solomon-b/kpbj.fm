{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Episode.Get.Handler where

--------------------------------------------------------------------------------

import API.Shows.Slug.Episode.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
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

  -- Fetch the episode by show slug and episode number
  execQuerySpan (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber) >>= \case
    Left _err -> do
      Log.logInfo "Failed to fetch episode from database" (showSlug, episodeNumber)
      renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load episode. Please try again.")
    Right Nothing -> do
      Log.logInfo "Episode not found" (showSlug, episodeNumber)
      renderTemplate hxRequest mUserInfo (notFoundTemplate showSlug episodeNumber)
    Right (Just episode) -> do
      renderEpisode hxRequest mUserInfo mUserId episode

renderEpisode ::
  ( Has Tracer env,
    MonadReader env m,
    Log.MonadLog m,
    MonadDB m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    Has HSQL.Pool.Pool env
  ) =>
  HxRequest ->
  Maybe UserMetadata.Model ->
  Maybe User.Id ->
  Episodes.Model ->
  m (Lucid.Html ())
renderEpisode hxRequest mUserInfo mUserId episode = do
  showResult <- execQuerySpan (Shows.getShowById episode.showId)
  tracks <- fromRight [] <$> execQuerySpan (EpisodeTrack.getTracksForEpisode episode.id)

  case showResult of
    Left err -> do
      Log.logInfo "Failed to fetch show from database" (Aeson.object ["showId" .= episode.showId, "error" .= show err])
      renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load show data. Please try again.")
    Right Nothing -> do
      Log.logInfo "Show not found for episode" (Aeson.object ["showId" .= episode.showId])
      renderTemplate hxRequest mUserInfo (errorTemplate "Show not found for this episode.")
    Right (Just showModel) -> do
      -- Check if user can view drafts (is host of the show or is staff/admin)
      let isStaffOrAdmin = case mUserInfo of
            Just metadata -> UserMetadata.isStaffOrHigher (UserMetadata.mUserRole metadata)
            Nothing -> False
      isHost <- case mUserId of
        Nothing -> pure False
        Just userId -> do
          result <- execQuerySpan (ShowHost.isUserHostOfShow userId showModel.id)
          pure $ fromRight False result
      let canViewDrafts = isHost || isStaffOrAdmin
          episodeTemplate = template showModel episode tracks canViewDrafts
      renderTemplate hxRequest mUserInfo episodeTemplate
