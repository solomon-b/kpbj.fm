{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Get.Templates.Page
  ( template,
    renderSidebar,
  )
where

import {-# SOURCE #-} API (blogGetLink)
import API.Blog.Get.Templates.Pagination (renderPagination)
import API.Blog.Get.Templates.PostCard (renderBlogPostCard)
import Control.Monad (unless)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Tables.Blog qualified as Blog
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

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
    blogGetCategoryUrl :: Text -> Links.URI
    blogGetCategoryUrl category = Links.linkURI $ blogGetLink Nothing (Just category) Nothing

    blogGetTagUrl :: Text -> Links.URI
    blogGetTagUrl tag = Links.linkURI $ blogGetLink Nothing Nothing (Just tag)

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
