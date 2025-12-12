module API.Blog.Get.Templates.Page
  ( template,
  )
where

import API.Blog.Get.Templates.Pagination (renderPagination)
import API.Blog.Get.Templates.PostCard (renderBlogPostCard)
import Component.Layout qualified as Layout
import Control.Monad (unless)
import Data.Int (Int64)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Lucid qualified
import Lucid.Responsive (cls, md)

-- | Main blog template
template :: [(BlogPosts.Model, [BlogTags.Model])] -> Int64 -> Bool -> Lucid.Html ()
template blogPosts currentPage hasMore = do
  -- Blog Header
  Layout.heroSection $ do
    Lucid.h1_ [Lucid.class_ $ cls [Tokens.heading2xl, Tokens.mb4]] "KPBJ STATION BLOG"
    Lucid.p_ [Lucid.class_ $ cls [Tokens.textLg, md "text-xl", Tokens.textGray600, Tokens.mb6]] $
      "News, stories, and insights from the KPBJ community"

  -- Blog Posts
  Lucid.div_ [Lucid.class_ $ cls [Tokens.gap8, Tokens.fullWidth]] $ do
    Lucid.div_ [Lucid.class_ "space-y-6"] $ do
      if null blogPosts
        then Layout.cardSection $ do
          Lucid.div_ [Lucid.class_ "text-center"] $ do
            Lucid.h2_ [Lucid.class_ $ cls [Tokens.headingLg, Tokens.mb4]] "No Blog Posts Yet"
            Lucid.p_ [Lucid.class_ Tokens.textGray600] "Check back soon for updates from the KPBJ community!"
        else mapM_ renderBlogPostCard blogPosts

      -- Pagination
      unless (null blogPosts) $
        renderPagination currentPage hasMore
