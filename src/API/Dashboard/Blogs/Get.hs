{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blogs.Get where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Get.Templates.Page (template)
import API.Dashboard.Get.Templates.Auth (notAuthorizedTemplate, notLoggedInTemplate)
import API.Links (showBlogLinks)
import API.Types
import App.Common (getUserInfo, renderDashboardTemplate, renderTemplate)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as Txn
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

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
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug cookie (foldHxReq -> hxRequest) = do
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
              renderDashboardTemplate hxRequest userMetadata [] Nothing NavBlog (statsContent []) Nothing (template Nothing [])
            Right [] -> do
              renderDashboardTemplate hxRequest userMetadata [] Nothing NavBlog (statsContent []) Nothing (template Nothing [])
            Right allShows@(firstShow : _) -> do
              let showToFetch = findShow firstShow allShows (Just showSlug)
              execTransactionSpan (fetchBlogData showToFetch) >>= \case
                Left _err -> do
                  renderDashboardTemplate hxRequest userMetadata allShows (Just showToFetch) NavBlog (statsContent []) (actionButton showToFetch) (template (Just showToFetch) [])
                Right blogPosts -> do
                  renderDashboardTemplate hxRequest userMetadata allShows (Just showToFetch) NavBlog (statsContent blogPosts) (actionButton showToFetch) (template (Just showToFetch) blogPosts)
    Just (user, userMetadata) -> do
      Log.logInfo "Host accessing dashboard blog" userMetadata.mDisplayName
      execQuerySpan (Shows.getShowsForUser (User.mId user)) >>= \case
        Left _err -> do
          renderDashboardTemplate hxRequest userMetadata [] Nothing NavBlog (statsContent []) Nothing (template Nothing [])
        Right [] -> do
          renderDashboardTemplate hxRequest userMetadata [] Nothing NavBlog (statsContent []) Nothing (template Nothing [])
        Right userShows@(firstShow : _) -> do
          let showToFetch = findShow firstShow userShows (Just showSlug)
          execTransactionSpan (fetchBlogData showToFetch) >>= \case
            Left _err -> do
              renderDashboardTemplate hxRequest userMetadata userShows (Just showToFetch) NavBlog (statsContent []) (actionButton showToFetch) (template (Just showToFetch) [])
            Right blogPosts -> do
              renderDashboardTemplate hxRequest userMetadata userShows (Just showToFetch) NavBlog (statsContent blogPosts) (actionButton showToFetch) (template (Just showToFetch) blogPosts)
  where
    -- \| Build stats content for top bar
    statsContent :: [ShowBlogPosts.Model] -> Maybe (Lucid.Html ())
    statsContent blogPosts =
      Just $
        Lucid.span_ [] $
          Lucid.toHtml $
            show (length blogPosts) <> " posts"

    actionButton :: Shows.Model -> Maybe (Lucid.Html ())
    actionButton showModel =
      let newBlogUrl = Links.linkURI $ showBlogLinks.newGet showModel.slug
       in Just $
            Lucid.a_
              [ Lucid.href_ [i|/#{newBlogUrl}|],
                hxGet_ [i|/#{newBlogUrl}|],
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700"
              ]
              "New Post"

-- | Fetch blog data for dashboard
fetchBlogData :: Shows.Model -> Txn.Transaction [ShowBlogPosts.Model]
fetchBlogData showModel =
  Txn.statement () (ShowBlogPosts.getShowBlogPosts showModel.id 50 0)

-- | Select show based on 'Slug' query parameter or default to first show
findShow :: Shows.Model -> [Shows.Model] -> Maybe Slug -> Shows.Model
findShow firstShow userShows = \case
  Just slug -> fromMaybe firstShow $ find (\s -> s.slug == slug) userShows
  Nothing -> firstShow
