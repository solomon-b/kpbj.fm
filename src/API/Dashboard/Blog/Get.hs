{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blog.Get where

--------------------------------------------------------------------------------

import API.Dashboard.Blog.Get.Templates.Page (template)
import API.Dashboard.Get.Templates.Auth (notAuthorizedTemplate, notLoggedInTemplate)
import App.Common (getUserInfo, renderDashboardTemplate, renderTemplate)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as Txn
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /dashboard/blog"
    ( "dashboard"
        :> "blog"
        :> Servant.QueryParam "show" Slug
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

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
  Maybe Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer maybeShowSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to dashboard blog" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (_, userMetadata)
      | not (UserMetadata.isHostOrHigher userMetadata.mUserRole) -> do
          Log.logInfo "User without Host role tried to access dashboard blog" userMetadata.mDisplayName
          renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate
    Just (_, userMetadata)
      -- Admins see all shows, hosts see their assigned shows
      | UserMetadata.isAdmin userMetadata.mUserRole -> do
          Log.logInfo "Admin accessing dashboard blog" userMetadata.mDisplayName
          execQuerySpan Shows.getAllActiveShows >>= \case
            Left _err -> do
              let content = template userMetadata Nothing []
              renderDashboardTemplate hxRequest userMetadata [] Nothing NavBlog content
            Right [] -> do
              let content = template userMetadata Nothing []
              renderDashboardTemplate hxRequest userMetadata [] Nothing NavBlog content
            Right allShows@(firstShow : _) -> do
              let showToFetch = findShow firstShow allShows maybeShowSlug
              execTransactionSpan (fetchBlogData showToFetch) >>= \case
                Left _err -> do
                  let content = template userMetadata (Just showToFetch) []
                  renderDashboardTemplate hxRequest userMetadata allShows (Just showToFetch) NavBlog content
                Right blogPosts -> do
                  let content = template userMetadata (Just showToFetch) blogPosts
                  renderDashboardTemplate hxRequest userMetadata allShows (Just showToFetch) NavBlog content
    Just (user, userMetadata) -> do
      Log.logInfo "Host accessing dashboard blog" userMetadata.mDisplayName
      execQuerySpan (Shows.getShowsForUser (User.mId user)) >>= \case
        Left _err -> do
          let content = template userMetadata Nothing []
          renderDashboardTemplate hxRequest userMetadata [] Nothing NavBlog content
        Right [] -> do
          let content = template userMetadata Nothing []
          renderDashboardTemplate hxRequest userMetadata [] Nothing NavBlog content
        Right userShows@(firstShow : _) -> do
          let showToFetch = findShow firstShow userShows maybeShowSlug
          execTransactionSpan (fetchBlogData showToFetch) >>= \case
            Left _err -> do
              let content = template userMetadata (Just showToFetch) []
              renderDashboardTemplate hxRequest userMetadata userShows (Just showToFetch) NavBlog content
            Right blogPosts -> do
              let content = template userMetadata (Just showToFetch) blogPosts
              renderDashboardTemplate hxRequest userMetadata userShows (Just showToFetch) NavBlog content

-- | Fetch blog data for dashboard
fetchBlogData :: Shows.Model -> Txn.Transaction [ShowBlogPosts.Model]
fetchBlogData showModel =
  Txn.statement () (ShowBlogPosts.getShowBlogPosts showModel.id 50 0)

-- | Select show based on 'Slug' query parameter or default to first show
findShow :: Shows.Model -> [Shows.Model] -> Maybe Slug -> Shows.Model
findShow firstShow userShows = \case
  Just slug -> fromMaybe firstShow $ find (\s -> s.slug == slug) userShows
  Nothing -> firstShow
