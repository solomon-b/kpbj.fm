{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Fragment template for infinite scroll append responses.
--
-- Renders only the new blog post table rows and the sentinel/end indicator,
-- without the page wrapper. Used when appending content via HTMX.
module API.Dashboard.Blogs.Get.Templates.ItemsFragment
  ( renderItemsFragment,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Get.Templates.BlogPostRow (renderBlogPostTableRow)
import API.Links (dashboardBlogsLinks)
import API.Types
import Component.InfiniteScroll (renderEndOfContent, renderSentinel)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render just the blog post rows and sentinel for infinite scroll append.
--
-- This is returned for HTMX requests when page > 1, and gets appended
-- to the existing table body.
renderItemsFragment ::
  Shows.Model ->
  [ShowBlogPosts.Model] ->
  Int64 ->
  Bool ->
  Lucid.Html ()
renderItemsFragment showModel posts currentPage hasMore = do
  -- Render each new blog post row
  mapM_ (renderBlogPostTableRow showModel) posts

  -- Render sentinel for next page or end indicator
  -- For tables, we use a tr element as sentinel
  if hasMore
    then
      Lucid.tr_ [Lucid.id_ "load-more-sentinel-row"] $
        Lucid.td_ [Lucid.colspan_ "4"] $
          renderSentinel [i|/#{nextPageUrl}|] "#blog-posts-table-body"
    else
      Lucid.tr_ [] $
        Lucid.td_ [Lucid.colspan_ "4"] $
          renderEndOfContent
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardBlogsLinks.list showModel.slug (Just (currentPage + 1))
