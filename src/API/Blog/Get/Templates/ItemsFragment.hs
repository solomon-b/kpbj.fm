{-# LANGUAGE QuasiQuotes #-}

-- | Fragment template for infinite scroll append responses.
--
-- Renders only the new blog post items and the sentinel/end indicator,
-- without the page wrapper. Used when appending content via HTMX.
module API.Blog.Get.Templates.ItemsFragment
  ( renderItemsFragment,
  )
where

--------------------------------------------------------------------------------

import API.Links (blogLinks)
import API.Types
import Component.Card.BlogPost (renderStationBlogPostCard)
import Component.InfiniteScroll (renderEndOfContent, renderSentinel)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (UTCTime)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render just the blog post items and sentinel for infinite scroll append.
--
-- This is returned for HTMX requests when page > 1, and gets appended
-- to the existing items container.
renderItemsFragment ::
  UTCTime ->
  [(BlogPosts.Model, UserMetadata.Model, [BlogTags.Model])] ->
  Int64 ->
  Bool ->
  Maybe Text ->
  Lucid.Html ()
renderItemsFragment currentTime posts currentPage hasMore maybeTag = do
  -- Render each new post
  traverse_ (renderStationBlogPostCard currentTime) posts

  -- Render sentinel for next page or end indicator
  if hasMore
    then renderSentinel [i|/#{nextPageUrl}|] "#blog-posts-list"
    else renderEndOfContent
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ blogLinks.list (Just (currentPage + 1)) maybeTag
