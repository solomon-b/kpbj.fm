module API.Blog.Get.Templates.Page
  ( template,
  )
where

import API.Blog.Get.Templates.Pagination (renderPagination)
import API.Blog.Get.Templates.PostCard (renderBlogPostCard)
import Control.Monad (unless)
import Data.Int (Int64)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Lucid qualified

-- | Main blog template
template :: [(BlogPosts.Model, [BlogTags.Model])] -> Int64 -> Bool -> Lucid.Html ()
template blogPosts currentPage hasMore = do
  -- Blog Header
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 text-center w-full"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4"] "KPBJ STATION BLOG"
    Lucid.p_ [Lucid.class_ "text-lg text-gray-600 mb-6"] "News, stories, and insights from the KPBJ community"

  -- Blog Content Grid (Main + Sidebar)
  Lucid.div_ [Lucid.class_ "gap-8 w-full"] $ do
    -- Blog Posts - Main Content (3 columns)
    Lucid.div_ [Lucid.class_ "space-y-6"] $ do
      if null blogPosts
        then Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "No Blog Posts Yet"
          Lucid.p_ [Lucid.class_ "text-gray-600"] "Check back soon for updates from the KPBJ community!"
        else mapM_ renderBlogPostCard blogPosts

      -- Pagination
      unless (null blogPosts) $
        renderPagination currentPage hasMore
