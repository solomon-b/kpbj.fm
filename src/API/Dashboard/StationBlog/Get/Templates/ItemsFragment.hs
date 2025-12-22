{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Fragment template for infinite scroll append responses.
--
-- Renders only the new blog post table rows and the sentinel/end indicator,
-- without the page wrapper. Used when appending content via HTMX.
module API.Dashboard.StationBlog.Get.Templates.ItemsFragment
  ( renderItemsFragment,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Get.Templates.Page (renderPostRow)
import API.Links (dashboardStationBlogLinks)
import API.Types
import Component.InfiniteScroll (renderEndOfContent, renderSentinel)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render just the blog post rows and sentinel for infinite scroll append.
--
-- This is returned for HTMX requests when page > 1, and gets appended
-- to the existing table body.
renderItemsFragment ::
  [BlogPosts.Model] ->
  Int64 ->
  Bool ->
  Lucid.Html ()
renderItemsFragment posts currentPage hasMore = do
  -- Render each new post row
  mapM_ renderPostRow posts

  -- Render sentinel for next page or end indicator
  -- For tables, we use a tr element as sentinel
  if hasMore
    then
      Lucid.tr_ [Lucid.id_ "load-more-sentinel-row"] $
        Lucid.td_ [Lucid.colspan_ "5"] $
          renderSentinel [i|/#{nextPageUrl}|] "#station-blog-table-body"
    else
      Lucid.tr_ [] $
        Lucid.td_ [Lucid.colspan_ "5"] $
          renderEndOfContent
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardStationBlogLinks.list (Just (currentPage + 1))
