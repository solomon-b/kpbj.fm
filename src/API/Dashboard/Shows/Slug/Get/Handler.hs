{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Slug.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (handleHtmlErrors, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Data.Either (fromRight)
import Data.Has (getter)
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Clock (currentSystemTime)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Rel8 (Result)

--------------------------------------------------------------------------------

-- | Number of episodes to show per page
episodesPerPage :: Int64
episodesPerPage = 10

handler ::
  Tracer ->
  Shows.Id ->
  Slug ->
  Maybe Int ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer showId showSlug mPage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Show details" apiLinks.rootGet $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireShowHostOrStaff user.mId showSlug userMetadata

    -- 2. Get storage backend
    backend <- asks getter

    -- 3. Fetch shows for sidebar (admins see all, hosts see their own)
    allShows <- fetchShowsForUser user userMetadata

    -- 4. Find the selected show
    let selectedShow = find (\s -> s.id == showId) allShows
    case selectedShow of
      Nothing -> throwNotFound "Show"
      Just showModel -> do
        -- 5. Render show details
        renderShowDetails backend hxRequest userMetadata allShows showModel mPage

-- | Fetch shows based on user role (admins see all, hosts see their own)
fetchShowsForUser ::
  User.Model ->
  UserMetadata.Model ->
  AppM [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then fromRight [] <$> execQuerySpan Shows.getAllActiveShows
    else fromRight [] <$> execQuerySpan (Shows.getShowsForUser (User.mId user))

-- | Render show details page within dashboard frame
renderShowDetails ::
  StorageBackend ->
  HxRequest ->
  UserMetadata.Model ->
  [Shows.Model] ->
  Shows.Model ->
  Maybe Int ->
  AppM (Lucid.Html ())
renderShowDetails backend hxRequest userMetadata allShows showModel mPage = do
  let page = fromMaybe 1 mPage
      limit = fromIntegral episodesPerPage
      offset = fromIntegral (page - 1) * fromIntegral episodesPerPage
  now <- currentSystemTime
  fetchShowDetails now showModel limit offset >>= \case
    Left err -> do
      Log.logAttention "Failed to fetch show details from database" (show err)
      let content = template backend showModel [] [] [] [] [] page
      renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavShows Nothing Nothing content
    Right (hosts, schedule, episodes, blogPosts, tags) -> do
      let content = template backend showModel episodes hosts schedule blogPosts tags page
      renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavShows Nothing Nothing content

fetchShowDetails ::
  UTCTime ->
  Shows.Model ->
  Limit ->
  Offset ->
  AppM (Either HSQL.Pool.UsageError ([ShowHost.ShowHostWithUser], [ShowSchedule.ScheduleTemplate Result], [Episodes.Model], [ShowBlogPosts.Model], [ShowTags.Model]))
fetchShowDetails now showModel limit offset = runDBTransaction $ do
  episodes <- TRX.statement () $ Episodes.getPublishedEpisodesForShow now showModel.id limit offset
  hosts <- TRX.statement () $ ShowHost.getShowHostsWithUsers showModel.id
  blogPosts <- TRX.statement () $ ShowBlogPosts.getPublishedShowBlogPosts showModel.id 3 0
  schedule <- TRX.statement () $ ShowSchedule.getActiveScheduleTemplatesForShow showModel.id
  tags <- TRX.statement () $ Shows.getTagsForShow showModel.id
  pure (hosts, schedule, episodes, blogPosts, tags)
