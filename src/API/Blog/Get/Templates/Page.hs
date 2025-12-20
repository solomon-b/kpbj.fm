module API.Blog.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Blog.Get.Templates.Pagination (renderPagination)
import Component.Card.BlogPost (renderStationBlogPostCard)
import Component.PageHeader (pageHeader)
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Time (UTCTime)
import Design (base, class_)
import Design.Lucid qualified as Layout
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

--------------------------------------------------------------------------------

-- | Main blog posts template
template :: UTCTime -> [(BlogPosts.Model, UserMetadata.Model, [BlogTags.Model])] -> Int64 -> Bool -> Lucid.Html ()
template currentTime blogPosts currentPage hasMore = do
  -- Blog Header
  pageHeader "BLOG"

  Lucid.section_ [Lucid.id_ "blog-posts-content-container", Lucid.class_ "w-full"] $ do
    Lucid.div_ [class_ $ base [Tokens.fullWidth, "space-y-8"]] $ do
      if null blogPosts
        then Layout.cardSection $ do
          Lucid.div_ [Lucid.class_ "text-center"] $ do
            Lucid.h2_ [class_ $ base [Tokens.headingLg, Tokens.mb4]] "No Blog Posts Yet"
            Lucid.p_ [Lucid.class_ Tokens.textGray600] "Check back soon for updates from the KPBJ community!"
        else traverse_ (renderStationBlogPostCard currentTime) blogPosts

      -- Pagination
      unless (null blogPosts) $
        renderPagination currentPage hasMore
