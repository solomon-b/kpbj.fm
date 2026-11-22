{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blogs.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.Blogs.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardBlogsLinks)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.CurrentShow (fetchDashboardShows)
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execTransaction)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as Txn
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Slug ->
  Maybe Int64 ->
  Satisfies 'UserMetadata.Host RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showSlug maybePage (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Show blog list" apiLinks.rootGet $ do
    vd <- action auth.raMetadata showSlug maybePage
    let isAppendRequest = hxRequest == IsHxRequest && vd.blvPage > 1
    if isAppendRequest
      then case vd.blvCurrentShow of
        Nothing -> throwNotFound "Show"
        Just showModel ->
          -- Infinite scroll: return only new rows + sentinel
          pure $ renderItemsFragment showModel vd.blvPosts vd.blvPage vd.blvHasMore
      else case vd.blvCurrentShow of
        Nothing ->
          lift $ renderDashboardTemplate hxRequest vd.blvUserMetadata NoShow NavBlog (statsContent []) Nothing (template Nothing [] vd.blvPage False)
        Just showModel ->
          lift $ renderDashboardTemplate hxRequest vd.blvUserMetadata (ForShow showModel) NavBlog (statsContent vd.blvPosts) (actionButton showModel) (template (Just showModel) vd.blvPosts vd.blvPage vd.blvHasMore)
  where
    statsContent :: [ShowBlogPosts.Model] -> Maybe (Lucid.Html ())
    statsContent blogPosts =
      Just $
        Lucid.span_ [] $
          Lucid.toHtml $
            show (length blogPosts) <> " posts"

    actionButton :: Shows.Model -> Maybe (Lucid.Html ())
    actionButton showModel =
      let newBlogUrl = Links.linkURI $ dashboardBlogsLinks.newGet showModel.slug
       in Just $
            Lucid.a_
              [ Lucid.href_ [i|/#{newBlogUrl}|],
                hxGet_ [i|/#{newBlogUrl}|],
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                class_ $ base [Tokens.bgInverse, Tokens.fgInverse, Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, Tokens.hoverBg]
              ]
              "New Post"

--------------------------------------------------------------------------------

-- | All data needed to render the blog list page.
data BlogListViewData = BlogListViewData
  { blvUserMetadata :: UserMetadata.Model,
    blvCurrentShow :: Maybe Shows.Model,
    blvPosts :: [ShowBlogPosts.Model],
    blvPage :: Int64,
    blvHasMore :: Bool
  }

-- | Business logic: pagination, show selection, blog post fetching.
action ::
  UserMetadata.Model ->
  Slug ->
  Maybe Int64 ->
  ExceptT HandlerError AppM BlogListViewData
action userMetadata showSlug maybePage = do
  -- 1. Set up pagination
  let page = fromMaybe 1 maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset

  -- 2. Fetch the shows this user may open. This list is the access check:
  -- the page shows a show from it, and never a show outside it.
  accessibleShows <- lift $ fetchDashboardShows userMetadata

  -- 3. Determine which show to display and fetch posts
  case listToMaybe accessibleShows of
    Nothing ->
      pure
        BlogListViewData
          { blvUserMetadata = userMetadata,
            blvCurrentShow = Nothing,
            blvPosts = [],
            blvPage = page,
            blvHasMore = False
          }
    Just firstShow -> do
      let showToFetch = findShow firstShow accessibleShows (Just showSlug)
      -- 4. Fetch blog posts for the show
      allPosts <- fetchBlogPosts showToFetch limit offset
      let posts = take (fromIntegral limit) allPosts
          hasMore = length allPosts > fromIntegral limit
      pure
        BlogListViewData
          { blvUserMetadata = userMetadata,
            blvCurrentShow = Just showToFetch,
            blvPosts = posts,
            blvPage = page,
            blvHasMore = hasMore
          }

-- | Fetch blog posts for a show with pagination
fetchBlogPosts ::
  Shows.Model ->
  Limit ->
  Offset ->
  ExceptT HandlerError AppM [ShowBlogPosts.Model]
fetchBlogPosts showModel limit offset =
  fromRightM throwDatabaseError $
    execTransaction (fetchBlogData showModel limit offset)

-- | Fetch blog data for dashboard with pagination
fetchBlogData :: Shows.Model -> Limit -> Offset -> Txn.Transaction [ShowBlogPosts.Model]
fetchBlogData showModel lim off =
  -- Fetch limit + 1 to check if there are more results. Read the id through
  -- the field selector. hlint reads showModel.id as a composition with
  -- Data.Function.id, and reports a redundant id.
  Txn.statement () (ShowBlogPosts.getShowBlogPosts (Shows.id showModel) (lim + 1) off)

-- | Select show based on 'Slug' query parameter or default to first show
findShow :: Shows.Model -> [Shows.Model] -> Maybe Slug -> Shows.Model
findShow firstShow userShows = \case
  Just slug -> fromMaybe firstShow $ find (\s -> s.slug == slug) userShows
  Nothing -> firstShow
