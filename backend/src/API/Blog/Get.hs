{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogGetLink, blogPostGetLink)
import App.Auth qualified as Auth
import Component.Frame (UserInfo (..), loadContentOnly, loadFrame, loadFrameWithUser)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Blog qualified as Blog
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- URL helpers
blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing Nothing

blogPostGetUrl :: Text -> Links.URI
blogPostGetUrl slug = Links.linkURI $ blogPostGetLink slug

blogGetPageUrl :: Int64 -> Links.URI
blogGetPageUrl page = Links.linkURI $ blogGetLink (Just page) Nothing Nothing

blogGetCategoryUrl :: Text -> Links.URI
blogGetCategoryUrl category = Links.linkURI $ blogGetLink Nothing (Just category) Nothing

blogGetTagUrl :: Text -> Links.URI
blogGetTagUrl tag = Links.linkURI $ blogGetLink Nothing Nothing (Just tag)

--------------------------------------------------------------------------------

type Route =
  "blog"
    :> Servant.QueryParam "page" Int64
    :> Servant.QueryParam "category" Text
    :> Servant.QueryParam "tag" Text
    :> Servant.Header "Cookie" Text
    :> Servant.Header "HX-Request" Text
    :> Servant.Get '[HTML] (Lucid.Html ())

--------------------------------------------------------------------------------

-- | Render a blog post card for the list view
renderBlogPostCard :: Blog.BlogPostModel -> Lucid.Html ()
renderBlogPostCard post = do
  Lucid.article_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-6"] $ do
    -- Category badge
    Lucid.div_ [Lucid.class_ "mb-3"] $ do
      Lucid.span_ [Lucid.class_ "bg-blue-200 text-blue-800 px-2 py-1 text-xs font-bold"] $
        Lucid.toHtml (Blog.bpmCategory post)

    -- Title
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-3"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{blogPostGetUrl (Blog.bpmSlug post)}|],
          hxGet_ [i|/#{blogPostGetUrl (Blog.bpmSlug post)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "hover:underline"
        ]
        $ Lucid.toHtml (Blog.bpmTitle post)

    -- Metadata
    Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-4"] $ do
      case Blog.bpmPublishedAt post of
        Just publishedAt -> do
          let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
          Lucid.toHtml $ "Published " <> dateStr
        Nothing -> "Draft"

    -- Excerpt
    case Blog.bpmExcerpt post of
      Just excerpt ->
        Lucid.p_ [Lucid.class_ "text-gray-700 mb-4 leading-relaxed"] $
          Lucid.toHtml excerpt
      Nothing -> do
        let truncatedContent = Text.take 200 (Blog.bpmContent post)
        Lucid.p_ [Lucid.class_ "text-gray-700 mb-4 leading-relaxed"] $
          Lucid.toHtml $
            truncatedContent <> if Text.length (Blog.bpmContent post) > 200 then "..." else ""

    -- Read more link
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ [Lucid.class_ "flex items-center gap-4 text-sm text-gray-600"] $ do
        Lucid.span_ ""
      Lucid.a_
        [ Lucid.href_ [i|/#{blogPostGetUrl (Blog.bpmSlug post)}|],
          hxGet_ [i|/#{blogPostGetUrl (Blog.bpmSlug post)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-4 py-2 font-bold hover:bg-gray-700"
        ]
        "READ MORE"

-- | Render pagination controls
renderPagination :: Int64 -> Bool -> Lucid.Html ()
renderPagination currentPage hasMore = do
  Lucid.div_ [Lucid.class_ "flex justify-center mt-8"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center space-x-2"] $ do
      -- Previous button
      if currentPage > 1
        then
          Lucid.a_
            [ Lucid.href_ [i|/#{blogGetPageUrl (currentPage - 1)}|],
              hxGet_ [i|/#{blogGetPageUrl (currentPage - 1)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "px-3 py-1 text-gray-800 hover:bg-gray-200"
            ]
            "‹ Previous"
        else Lucid.span_ [Lucid.class_ "px-3 py-1 text-gray-400"] "‹ Previous"

      -- Current page
      Lucid.span_ [Lucid.class_ "px-3 py-1 bg-gray-800 text-white font-bold"] $
        Lucid.toHtml $
          show currentPage

      -- Next button
      if hasMore
        then
          Lucid.a_
            [ Lucid.href_ [i|/#{blogGetPageUrl (currentPage + 1)}|],
              hxGet_ [i|/#{blogGetPageUrl (currentPage + 1)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "px-3 py-1 text-gray-800 hover:bg-gray-200"
            ]
            "Next ›"
        else Lucid.span_ [Lucid.class_ "px-3 py-1 text-gray-400"] "Next ›"

-- | Template for general error
errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Error"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] $ Lucid.toHtml errorMsg
    Lucid.a_
      [ Lucid.href_ [i|/#{blogGetUrl}|],
        hxGet_ [i|/#{blogGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
      ]
      "REFRESH"

-- | Main blog template
template :: [Blog.BlogPostModel] -> Int64 -> Bool -> [Blog.BlogTagWithCount] -> [Blog.CategoryWithCount] -> Lucid.Html ()
template blogPosts currentPage hasMore tagsWithCounts categoriesWithCounts = do
  -- Blog Header
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 text-center w-full"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4"] "KPBJ STATION BLOG"
    Lucid.p_ [Lucid.class_ "text-lg text-gray-600 mb-6"] "News, stories, and insights from the KPBJ community"

  -- Blog Content Grid (Main + Sidebar)
  Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-4 gap-8"] $ do
    -- Blog Posts - Main Content (3 columns)
    Lucid.div_ [Lucid.class_ "lg:col-span-3 space-y-6"] $ do
      if null blogPosts
        then Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "No Blog Posts Yet"
          Lucid.p_ [Lucid.class_ "text-gray-600"] "Check back soon for updates from the KPBJ community!"
        else mapM_ renderBlogPostCard blogPosts

      -- Pagination
      unless (null blogPosts) $
        renderPagination currentPage hasMore

    -- Sidebar (1 column)
    renderSidebar tagsWithCounts categoriesWithCounts

-- | Render the blog sidebar
renderSidebar :: [Blog.BlogTagWithCount] -> [Blog.CategoryWithCount] -> Lucid.Html ()
renderSidebar tagsWithCounts categoriesWithCounts = do
  Lucid.div_ [Lucid.class_ "lg:col-span-1 space-y-6"] $ do
    -- Search
    Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-4"] $ do
      Lucid.h3_ [Lucid.class_ "font-bold mb-4 text-center"] "SEARCH BLOG"
      Lucid.input_ [Lucid.type_ "search", Lucid.placeholder_ "Search posts...", Lucid.class_ "w-full border-2 border-gray-600 p-2 font-mono text-sm"]
      Lucid.button_ [Lucid.class_ "w-full bg-gray-800 text-white py-2 mt-2 font-bold hover:bg-gray-700"] "SEARCH"

    -- Categories
    Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-4"] $ do
      Lucid.h3_ [Lucid.class_ "font-bold mb-4 text-center"] "CATEGORIES"
      Lucid.div_ [Lucid.class_ "space-y-2 text-sm"] $ do
        if null categoriesWithCounts
          then Lucid.p_ [Lucid.class_ "text-gray-500 text-center"] "No categories yet"
          else mapM_ renderCategoryWithCount categoriesWithCounts

    -- Popular Tags
    Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-4"] $ do
      Lucid.h3_ [Lucid.class_ "font-bold mb-4 text-center"] "POPULAR TAGS"
      Lucid.div_ [Lucid.class_ "flex flex-wrap gap-2 text-xs"] $ do
        if null tagsWithCounts
          then Lucid.p_ [Lucid.class_ "text-gray-500 text-center"] "No tags yet"
          else mapM_ renderTagWithCount tagsWithCounts

    -- Station Info
    Lucid.div_ [Lucid.class_ "bg-gray-800 text-white p-4"] $ do
      Lucid.h3_ [Lucid.class_ "font-bold mb-4 text-center"] "ABOUT KPBJ"
      Lucid.p_ [Lucid.class_ "text-sm leading-relaxed mb-4"] $
        "Community-powered radio serving Shadow Hills and beyond with underground music, "
          <> "local voices, and authentic programming since 2018."
      Lucid.div_ [Lucid.class_ "text-xs space-y-1"] $ do
        Lucid.div_ "📻 95.9 FM"
        Lucid.div_ "🌐 kpbj.fm"
        Lucid.div_ "📧 hello@kpbj.fm"
        Lucid.div_ "📞 (555) 959-KPBJ"
  where
    renderCategoryWithCount :: Blog.CategoryWithCount -> Lucid.Html ()
    renderCategoryWithCount categoryWithCount =
      Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{blogGetCategoryUrl (Blog.cwcCategory categoryWithCount)}|],
            hxGet_ [i|/#{blogGetCategoryUrl (Blog.cwcCategory categoryWithCount)}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "hover:underline"
          ]
          $ Lucid.toHtml (Blog.cwcCategory categoryWithCount)
        Lucid.span_ [Lucid.class_ "text-gray-600"] $
          Lucid.toHtml $
            "(" <> show (Blog.cwcCount categoryWithCount) <> ")"

    renderTagWithCount :: Blog.BlogTagWithCount -> Lucid.Html ()
    renderTagWithCount tagWithCount =
      let tagName = Blog.btwcName tagWithCount
          count = Blog.btwcCount tagWithCount
       in Lucid.a_
            [ Lucid.href_ [i|/#{blogGetTagUrl tagName}|],
              hxGet_ [i|/#{blogGetTagUrl tagName}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "bg-gray-200 text-gray-800 px-2 py-1 font-mono hover:bg-gray-300",
              Lucid.title_ (tagName <> " (" <> Text.pack (show count) <> " posts)")
            ]
            $ Lucid.toHtml
            $ "#" <> tagName

-- | Render template with proper HTMX handling
renderTemplate :: (Log.MonadLog m, MonadCatch m) => Bool -> Maybe UserInfo -> Lucid.Html () -> m (Lucid.Html ())
renderTemplate isHtmxRequest mUserInfo templateContent =
  case mUserInfo of
    Just userInfo ->
      if isHtmxRequest
        then loadContentOnly templateContent
        else loadFrameWithUser userInfo templateContent
    Nothing ->
      if isHtmxRequest
        then loadContentOnly templateContent
        else loadFrame templateContent

checkHtmxRequest :: Maybe Text -> Bool
checkHtmxRequest = \case
  Just "true" -> True
  _ -> False

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
  Maybe Int64 ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  m (Lucid.Html ())
handler maybePage maybeCategory maybeTag cookie hxRequest =
  Observability.handlerSpan "GET /blog" $ do
    let page = fromMaybe 1 maybePage
        limit = 10
        offset = (page - 1) * limit
        isHtmxRequest = checkHtmxRequest hxRequest

    -- Get user info once upfront
    loginState <- Auth.userLoginState cookie
    mUserInfo <- case loginState of
      Auth.IsNotLoggedIn -> pure Nothing
      Auth.IsLoggedIn user -> do
        execQuerySpan (UserMetadata.getUserMetadata (User.mId user)) >>= \case
          Right (Just userMetadata) ->
            pure $ Just $ UserInfo {userDisplayName = UserMetadata.mDisplayName userMetadata}
          _ -> pure Nothing

    -- Get sidebar data
    tagsResult <- execQuerySpan Blog.getTagsWithCounts
    categoriesResult <- execQuerySpan Blog.getCategoriesWithCounts

    let tagsWithCounts = fromRight [] tagsResult
        categoriesWithCounts = fromRight [] categoriesResult

    -- Get blog posts based on filters (category or tag)
    blogPostsResult <- case (maybeCategory, maybeTag) of
      (Just category, _) ->
        execQuerySpan (Blog.getBlogPostsByCategory category limit offset)
      (_, Just tagName) ->
        execQuerySpan (Blog.getTagByName tagName) >>= \case
          Left err ->
            pure (Left err)
          Right Nothing ->
            pure (Right [])
          Right (Just tag) ->
            execQuerySpan (Blog.getPostsByTag (Blog.btmId tag) limit offset)
      (Nothing, Nothing) ->
        execQuerySpan (Blog.getPublishedBlogPosts limit offset)
    case blogPostsResult of
      Left _err -> do
        Log.logInfo "Failed to fetch blog posts from database" ()
        renderTemplate isHtmxRequest mUserInfo (errorTemplate "Failed to load blog posts. Please try again.")
      Right allPosts -> do
        let posts = take (fromIntegral limit) allPosts
            hasMore = length allPosts > fromIntegral limit
            blogTemplate = template posts page hasMore tagsWithCounts categoriesWithCounts
        renderTemplate isHtmxRequest mUserInfo blogTemplate
