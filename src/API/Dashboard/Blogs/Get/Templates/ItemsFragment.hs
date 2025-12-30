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
import Component.Table (renderTableFragment)
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
renderItemsFragment showModel posts currentPage hasMore =
  renderTableFragment
    4 -- Number of columns
    "#blog-posts-table-body"
    (if hasMore then Just [i|/#{nextPageUrl}|] else Nothing)
    (mapM_ (renderBlogPostTableRow showModel) posts)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardBlogsLinks.list showModel.slug (Just (currentPage + 1))
