{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Slug.Get where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import API.Dashboard.Get.Templates.Auth (notAuthorizedTemplate, notLoggedInTemplate)
import App.Common (getUserInfo, renderDashboardTemplate, renderTemplate)
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
  Episodes.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug episodeId _episodeSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to dashboard episode" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (_, userMetadata)
      | not (UserMetadata.isHostOrHigher userMetadata.mUserRole) -> do
          Log.logInfo "User without Host role tried to access dashboard episode" userMetadata.mDisplayName
          renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate
    Just (user, userMetadata) -> do
      -- Fetch the show
      execQuerySpan (Shows.getShowBySlug showSlug) >>= \case
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
              -- Fetch the episode
              execQuerySpan (Episodes.getEpisodeById episodeId) >>= \case
                Left _err -> do
                  Log.logInfo "Failed to fetch episode from database" episodeId
                  let content = errorTemplate "Failed to load episode. Please try again."
                  renderDashboardTemplate hxRequest userMetadata [showModel] (Just showModel) NavEpisodes Nothing Nothing content
                Right Nothing -> do
                  Log.logInfo "Episode not found" episodeId
                  let content = notFoundTemplate showSlug
                  renderDashboardTemplate hxRequest userMetadata [showModel] (Just showModel) NavEpisodes Nothing Nothing content
                Right (Just episode) -> do
                  -- Verify episode belongs to the show
                  if episode.showId /= showModel.id
                    then do
                      Log.logInfo "Episode does not belong to show" (showSlug, episodeId)
                      let content = errorTemplate "Episode not found in this show."
                      renderDashboardTemplate hxRequest userMetadata [showModel] (Just showModel) NavEpisodes Nothing Nothing content
                    else do
                      -- Fetch tracks for the episode
                      tracks <- fromRight [] <$> execQuerySpan (Episodes.getTracksForEpisode episode.id)

                      -- Get user's shows for sidebar
                      userShows <-
                        if UserMetadata.isAdmin userMetadata.mUserRole
                          then fromRight [] <$> execQuerySpan Shows.getAllActiveShows
                          else fromRight [] <$> execQuerySpan (Shows.getShowsForUser (User.mId user))

                      let content = template userMetadata showModel episode tracks
                      renderDashboardTemplate hxRequest userMetadata userShows (Just showModel) NavEpisodes Nothing Nothing content
