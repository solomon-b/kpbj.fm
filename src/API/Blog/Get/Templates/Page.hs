{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Blog.Get.Templates.Pagination (renderPagination)
import API.Links (blogLinks)
import API.Types
import Component.Card.BlogPost (renderStationBlogPostCard)
import Component.InfiniteScroll (renderEndOfContent, renderLoadingIndicator, renderSentinel)
import Component.PageHeader (pageHeader)
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (UTCTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Main blog posts template with infinite scroll support.
--
-- Renders the full blog page with items container, loading indicator,
-- sentinel element, and noscript fallback pagination.
template ::
  StorageBackend ->
  UTCTime ->
  [(BlogPosts.Model, UserMetadata.Model, [BlogTags.Model])] ->
  Int64 ->
  Bool ->
  Maybe Text ->
  Lucid.Html ()
template backend currentTime blogPosts currentPage hasMore maybeTag = do
  -- Blog Header
  pageHeader "BLOG"

  Lucid.section_ [Lucid.id_ "blog-posts-content-container", Lucid.class_ "w-full"] $ do
    -- Items container with stable ID for HTMX appending
    Lucid.div_ [Lucid.id_ "blog-posts-list", class_ $ base [Tokens.fullWidth, "space-y-8"]] $ do
      if null blogPosts
        then Lucid.div_ [Lucid.class_ Tokens.cardBase] $ do
          Lucid.div_ [Lucid.class_ "text-center"] $ do
            Lucid.h2_ [class_ $ base [Tokens.headingLg, Tokens.mb4]] "No Blog Posts Yet"
            Lucid.p_ [Lucid.class_ Tokens.textGray600] "Check back soon for updates from the KPBJ community!"
        else traverse_ (renderStationBlogPostCard backend currentTime) blogPosts

    -- Loading indicator (hidden by default, shown during HTMX requests)
    renderLoadingIndicator

    -- Sentinel for infinite scroll or end indicator
    unless (null blogPosts) $
      if hasMore
        then renderSentinel [i|/#{nextPageUrl}|] "#blog-posts-list"
        else renderEndOfContent

    -- Fallback pagination for browsers without JavaScript
    Lucid.noscript_ $
      unless (null blogPosts) $
        renderPagination currentPage hasMore maybeTag
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ blogLinks.list (Just (currentPage + 1)) maybeTag
