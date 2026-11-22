{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Slug.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Combinators (authorizeShow)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Flash (FlashMessage)
import Control.Monad (unless)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has (getter)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Clock (currentSystemTime)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified
import Rel8 (Result)

--------------------------------------------------------------------------------

-- | Number of episodes to show per page
episodesPerPage :: Int64
episodesPerPage = 10

-- | All data needed to render the show detail page.
data ShowDetailViewData = ShowDetailViewData
  { sdvUserMetadata :: UserMetadata.Model,
    sdvShowModel :: Shows.Model,
    sdvBackend :: StorageBackend,
    sdvPage :: Int
  }

-- | Business logic: check that the show ID in the path names the show.
--
-- The handler checks access with 'authorizeShow', which loads the show by
-- slug. A show ID that names a different show is not found.
action ::
  UserMetadata.Model ->
  -- | The show ID in the path
  Shows.Id ->
  -- | The show of the slug in the path
  Shows.Model ->
  Maybe Int ->
  ExceptT HandlerError AppM ShowDetailViewData
action userMetadata showId showModel mPage = do
  unless (showModel.id == showId) $ throwNotFound "Show"
  backend <- asks getter
  pure
    ShowDetailViewData
      { sdvUserMetadata = userMetadata,
        sdvShowModel = showModel,
        sdvBackend = backend,
        sdvPage = fromMaybe 1 mPage
      }

handler ::
  Shows.Id ->
  Slug ->
  Maybe Int ->
  Satisfies 'UserMetadata.Host RoleAuth ->
  Maybe FlashMessage ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showId showSlug mPage host@(Satisfies _ auth) mFlash (foldHxReq -> hxRequest) =
  handleHtmlErrors "Show details" apiLinks.rootGet $ do
    showModel <- authorizeShow host showSlug
    vd <- action auth.raMetadata showId showModel mPage
    lift $ renderShowDetails vd.sdvBackend hxRequest mFlash vd.sdvUserMetadata vd.sdvShowModel (Just vd.sdvPage)

-- | Render show details page within dashboard frame
renderShowDetails ::
  StorageBackend ->
  HxRequest ->
  Maybe FlashMessage ->
  UserMetadata.Model ->
  Shows.Model ->
  Maybe Int ->
  AppM (Lucid.Html ())
renderShowDetails backend hxRequest mFlash userMetadata showModel mPage = do
  let page = fromMaybe 1 mPage
      limit = fromIntegral episodesPerPage
      offset = fromIntegral (page - 1) * fromIntegral episodesPerPage
  now <- currentSystemTime
  fetchShowDetails now showModel limit offset >>= \case
    Left err -> do
      Log.logAttention "Failed to fetch show details from database" (show err)
      let content = template backend showModel [] [] [] [] [] page
      renderDashboardTemplate hxRequest mFlash userMetadata (ForShow showModel) NavShows Nothing Nothing content
    Right (hosts, schedule, episodes, blogPosts, tags) -> do
      let content = template backend showModel episodes hosts schedule blogPosts tags page
      renderDashboardTemplate hxRequest mFlash userMetadata (ForShow showModel) NavShows Nothing Nothing content

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
