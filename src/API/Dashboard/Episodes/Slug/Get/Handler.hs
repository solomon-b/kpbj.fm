{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Slug.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import App.Common (getUserInfo, renderDashboardTemplate, renderTemplate)
import Component.Dashboard.Auth (notAuthorizedTemplate, notLoggedInTemplate)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
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
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Slug ->
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug episodeNumber cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to dashboard episode" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (_, userMetadata)
      | not (UserMetadata.isHostOrHigher userMetadata.mUserRole) -> do
          Log.logInfo "User without Host role tried to access dashboard episode" userMetadata.mDisplayName
          renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate
    Just (user, userMetadata) -> do
      -- Fetch the show and episode together
      execQuerySpan (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber) >>= \case
        Left _err -> do
          Log.logInfo "Failed to fetch episode from database" (showSlug, episodeNumber)
          let content = errorTemplate "Failed to load episode. Please try again."
          renderDashboardTemplate hxRequest userMetadata [] Nothing NavEpisodes Nothing Nothing content
        Right Nothing -> do
          Log.logInfo "Episode not found" (showSlug, episodeNumber)
          let content = notFoundTemplate showSlug
          renderDashboardTemplate hxRequest userMetadata [] Nothing NavEpisodes Nothing Nothing content
        Right (Just episode) -> do
          -- Fetch the show
          execQuerySpan (Shows.getShowById episode.showId) >>= \case
            Left _err -> do
              Log.logInfo "Failed to fetch show from database" showSlug
              let content = errorTemplate "Failed to load show. Please try again."
              renderDashboardTemplate hxRequest userMetadata [] Nothing NavEpisodes Nothing Nothing content
            Right Nothing -> do
              Log.logInfo "Show not found" showSlug
              let content = notFoundTemplate showSlug
              renderDashboardTemplate hxRequest userMetadata [] Nothing NavEpisodes Nothing Nothing content
            Right (Just showModel) -> do
              -- Verify user has access to this show (unless admin)
              hasAccess <-
                if UserMetadata.isAdmin userMetadata.mUserRole
                  then pure True
                  else do
                    execQuerySpan (Shows.getShowsForUser (User.mId user)) >>= \case
                      Left _ -> pure False
                      Right userShows -> pure $ any (\s -> s.id == showModel.id) userShows

              if not hasAccess
                then do
                  Log.logInfo "User tried to access episode for show they don't have access to" (userMetadata.mDisplayName, showSlug)
                  renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate
                else do
                  -- Fetch tracks for the episode
                  tracks <- fromRight [] <$> execQuerySpan (EpisodeTrack.getTracksForEpisode episode.id)

                  -- Fetch tags for the episode
                  tags <- fromRight [] <$> execQuerySpan (Episodes.getTagsForEpisode episode.id)

                  -- Get user's shows for sidebar
                  userShows <-
                    if UserMetadata.isAdmin userMetadata.mUserRole
                      then fromRight [] <$> execQuerySpan Shows.getAllActiveShows
                      else fromRight [] <$> execQuerySpan (Shows.getShowsForUser (User.mId user))

                  let content = template userMetadata showModel episode tracks tags
                  renderDashboardTemplate hxRequest userMetadata userShows (Just showModel) NavEpisodes Nothing Nothing content
