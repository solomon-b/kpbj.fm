{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Blogs.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.Blogs.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardBlogsLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
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
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showSlug maybePage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Show blog list" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata showSlug maybePage
    let isAppendRequest = hxRequest == IsHxRequest && vd.blvPage > 1
    if isAppendRequest
      then case vd.blvCurrentShow of
        Nothing -> throwNotFound "Show"
        Just showModel ->
          -- Infinite scroll: return only new rows + sentinel
          pure $ renderItemsFragment showModel vd.blvPosts vd.blvPage vd.blvHasMore
      else case vd.blvCurrentShow of
        Nothing ->
          lift $ renderDashboardTemplate hxRequest vd.blvUserMetadata [] Nothing NavBlog (statsContent []) Nothing (template Nothing [] vd.blvPage False)
        Just showModel ->
          lift $ renderDashboardTemplate hxRequest vd.blvUserMetadata vd.blvAllShows (Just showModel) NavBlog (statsContent vd.blvPosts) (actionButton showModel) (template (Just showModel) vd.blvPosts vd.blvPage vd.blvHasMore)
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
    blvAllShows :: [Shows.Model],
    blvCurrentShow :: Maybe Shows.Model,
    blvPosts :: [ShowBlogPosts.Model],
    blvPage :: Int64,
    blvHasMore :: Bool
  }

-- | Business logic: pagination, show selection, blog post fetching.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  Maybe Int64 ->
  ExceptT HandlerError AppM BlogListViewData
action user userMetadata showSlug maybePage = do
  -- 1. Set up pagination
  let page = fromMaybe 1 maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset

  -- 2. Fetch shows for sidebar (admins see all, hosts see their own)
  allShows <- lift $ fetchShowsForUser user userMetadata

  -- 3. Determine which show to display and fetch posts
  case listToMaybe allShows of
    Nothing ->
      pure
        BlogListViewData
          { blvUserMetadata = userMetadata,
            blvAllShows = [],
            blvCurrentShow = Nothing,
            blvPosts = [],
            blvPage = page,
            blvHasMore = False
          }
    Just firstShow -> do
      let showToFetch = findShow firstShow allShows (Just showSlug)
      -- 4. Fetch blog posts for the show
      allPosts <- fetchBlogPosts showToFetch limit offset
      let posts = take (fromIntegral limit) allPosts
          hasMore = length allPosts > fromIntegral limit
      pure
        BlogListViewData
          { blvUserMetadata = userMetadata,
            blvAllShows = allShows,
            blvCurrentShow = Just showToFetch,
            blvPosts = posts,
            blvPage = page,
            blvHasMore = hasMore
          }

-- | Fetch shows based on user role (admins see all, hosts see their own)
fetchShowsForUser ::
  User.Model ->
  UserMetadata.Model ->
  AppM [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then fromRight [] <$> execQuery Shows.getAllActiveShows
    else fromRight [] <$> execQuery (Shows.getShowsForUser (User.mId user))

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
  -- Fetch limit + 1 to check if there are more results
  Txn.statement () (ShowBlogPosts.getShowBlogPosts showModel.id (lim + 1) off)

-- | Select show based on 'Slug' query parameter or default to first show
findShow :: Shows.Model -> [Shows.Model] -> Maybe Slug -> Shows.Model
findShow firstShow userShows = \case
  Just slug -> fromMaybe firstShow $ find (\s -> s.slug == slug) userShows
  Nothing -> firstShow
