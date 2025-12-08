{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Slug.Get where

--------------------------------------------------------------------------------

import API.Dashboard.Get.Templates.Auth (notAuthorizedTemplate, notLoggedInTemplate)
import API.Dashboard.Shows.Slug.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import App.Common (getUserInfo, renderDashboardTemplate, renderTemplate)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Slug (Slug)
import Effects.Clock (MonadClock, currentSystemTime)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

-- | Number of episodes to show per page
episodesPerPage :: Int64
episodesPerPage = 10

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env,
    MonadClock m
  ) =>
  Tracer ->
  Shows.Id ->
  Slug ->
  Maybe Int ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showId showSlug mPage cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to dashboard show details" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (_, userMetadata)
      | not (UserMetadata.isHostOrHigher userMetadata.mUserRole) -> do
          Log.logInfo "User without Host role tried to access dashboard show details" userMetadata.mDisplayName
          renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate
    Just (_, userMetadata)
      | UserMetadata.isAdmin userMetadata.mUserRole -> do
          Log.logInfo "Admin accessing dashboard show details" userMetadata.mDisplayName
          execQuerySpan Shows.getAllActiveShows >>= \case
            Left _err -> do
              let content = errorTemplate "Failed to load shows."
              renderDashboardTemplate hxRequest userMetadata [] Nothing NavShows Nothing Nothing content
            Right allShows -> do
              let selectedShow = find (\s -> s.id == showId) allShows
              case selectedShow of
                Nothing -> do
                  Log.logInfo ("Show not found: " <> display showId) ()
                  let content = notFoundTemplate showSlug
                  renderDashboardTemplate hxRequest userMetadata allShows Nothing NavShows Nothing Nothing content
                Just showModel -> do
                  renderShowDetails hxRequest userMetadata allShows showModel mPage
    Just (user, userMetadata) -> do
      Log.logInfo "Host accessing dashboard show details" userMetadata.mDisplayName
      execQuerySpan (Shows.getShowsForUser (User.mId user)) >>= \case
        Left _err -> do
          let content = errorTemplate "Failed to load shows."
          renderDashboardTemplate hxRequest userMetadata [] Nothing NavShows Nothing Nothing content
        Right userShows -> do
          let selectedShow = find (\s -> s.id == showId) userShows
          case selectedShow of
            Nothing -> do
              Log.logInfo ("Show not found or not authorized: " <> display showId) ()
              let content = notFoundTemplate showSlug
              renderDashboardTemplate hxRequest userMetadata userShows Nothing NavShows Nothing Nothing content
            Just showModel -> do
              renderShowDetails hxRequest userMetadata userShows showModel mPage

-- | Render show details page within dashboard frame
renderShowDetails ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadClock m,
    MonadCatch m
  ) =>
  HxRequest ->
  UserMetadata.Model ->
  [Shows.Model] ->
  Shows.Model ->
  Maybe Int ->
  m (Lucid.Html ())
renderShowDetails hxRequest userMetadata allShows showModel mPage = do
  let page = fromMaybe 1 mPage
      limit = fromIntegral episodesPerPage
      offset = fromIntegral (page - 1) * fromIntegral episodesPerPage
  now <- currentSystemTime
  fetchShowDetails now showModel limit offset >>= \case
    Left err -> do
      Log.logAttention "Failed to fetch show details from database" (show err)
      let content = template showModel [] [] [] [] page
      renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavShows Nothing Nothing content
    Right (hosts, schedule, episodes, blogPosts) -> do
      let content = template showModel episodes hosts schedule blogPosts page
      renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavShows Nothing Nothing content

fetchShowDetails ::
  (MonadDB m) =>
  UTCTime ->
  Shows.Model ->
  Limit ->
  Offset ->
  m (Either HSQL.Pool.UsageError ([ShowHost.ShowHostWithUser], [ShowSchedule.ScheduleTemplate], [Episodes.Model], [ShowBlogPosts.Model]))
fetchShowDetails now showModel limit offset = runDBTransaction $ do
  episodes <- TRX.statement () $ Episodes.getPublishedEpisodesForShow now showModel.id limit offset
  hosts <- TRX.statement () $ ShowHost.getShowHostsWithUsers showModel.id
  blogPosts <- TRX.statement () $ ShowBlogPosts.getPublishedShowBlogPosts showModel.id 3 0
  schedule <- TRX.statement () $ ShowSchedule.getActiveScheduleTemplatesForShow showModel.id
  pure (hosts, schedule, episodes, blogPosts)
