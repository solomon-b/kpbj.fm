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
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
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

--------------------------------------------------------------------------------

handler ::
  Slug ->
  Maybe Int64 ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler showSlug maybePage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Show blog list" apiLinks.rootGet $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Set up pagination
    let page = fromMaybe 1 maybePage
        limit = 20 :: Limit
        offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
        isAppendRequest = hxRequest == IsHxRequest && page > 1

    -- 3. Fetch shows for sidebar (admins see all, hosts see their own)
    allShows <- fetchShowsForUser user userMetadata

    -- 4. Determine which show to display
    case listToMaybe allShows of
      Nothing -> do
        -- No shows available
        renderDashboardTemplate hxRequest userMetadata [] Nothing NavBlog (statsContent []) Nothing (template Nothing [] page False)
      Just firstShow -> do
        let showToFetch = findShow firstShow allShows (Just showSlug)
        -- 5. Fetch blog posts for the show
        allPosts <- fetchBlogPosts showToFetch limit offset

        let posts = take (fromIntegral limit) allPosts
            hasMore = length allPosts > fromIntegral limit

        if isAppendRequest
          then
            -- Infinite scroll: return only new rows + sentinel
            pure $ renderItemsFragment showToFetch posts page hasMore
          else do
            -- Full page: render with table, sentinel, and noscript pagination
            renderDashboardTemplate hxRequest userMetadata allShows (Just showToFetch) NavBlog (statsContent posts) (actionButton showToFetch) (template (Just showToFetch) posts page hasMore)
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
  AppM [ShowBlogPosts.Model]
fetchBlogPosts showModel limit offset =
  execTransaction (fetchBlogData showModel limit offset) >>= \case
    Left err -> throwDatabaseError err
    Right posts -> pure posts

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
