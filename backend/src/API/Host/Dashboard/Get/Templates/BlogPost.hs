{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Host.Dashboard.Get.Templates.BlogPost
  ( renderBlogPostTableRow,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogEditGetLink)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

blogEditGetUrl :: Slug -> Links.URI
blogEditGetUrl slug = Links.linkURI $ blogEditGetLink slug

--------------------------------------------------------------------------------

-- | Render individual blog post as table row
renderBlogPostTableRow :: ShowBlogPosts.Model -> Lucid.Html ()
renderBlogPostTableRow post = do
  let postId = post.id
      postRowId = [i|blog-post-row-#{postId}|]
  Lucid.tr_ [Lucid.class_ "border-b border-gray-300 hover:bg-gray-50", Lucid.id_ postRowId] $ do
    -- Title
    Lucid.td_ [Lucid.class_ "px-4 py-3"] $ do
      Lucid.div_ [Lucid.class_ "font-bold text-gray-900"] $ Lucid.toHtml post.title
      case post.excerpt of
        Just excerpt ->
          Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mt-1"] $ do
            Lucid.toHtml $ Text.take 100 excerpt
            if Text.length excerpt > 100 then "..." else ""
        Nothing ->
          Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mt-1"] $ do
            Lucid.toHtml $ Text.take 100 post.content
            if Text.length post.content > 100 then "..." else ""

    -- Published date
    Lucid.td_ [Lucid.class_ "px-4 py-3 text-sm"] $ do
      case post.publishedAt of
        Just publishedAt -> Lucid.toHtml $ Text.pack $ formatTime defaultTimeLocale "%b %d, %Y" publishedAt
        Nothing -> Lucid.span_ [Lucid.class_ "text-gray-500 italic"] "Draft"

    -- Status
    Lucid.td_ [Lucid.class_ "px-4 py-3 text-sm"] $
      renderStatusBadge post.status

    -- Actions
    Lucid.td_ [Lucid.class_ "px-4 py-3 text-right"] $ do
      let editUrl = blogEditGetUrl post.slug
      Lucid.div_ [Lucid.class_ "flex gap-2 justify-end"] $
        Lucid.a_
          [ Lucid.href_ [i|/#{editUrl}|],
            hxGet_ [i|/#{editUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-blue-600 text-white px-3 py-1 text-xs font-bold hover:bg-blue-700 no-underline"
          ]
          "EDIT"

-- | Render status badge with appropriate styling
renderStatusBadge :: BlogPostStatus -> Lucid.Html ()
renderStatusBadge Draft =
  Lucid.span_ [Lucid.class_ "inline-block bg-gray-100 text-gray-800 px-2 py-1 rounded text-xs font-bold"] "DRAFT"
renderStatusBadge Published =
  Lucid.span_ [Lucid.class_ "inline-block bg-green-100 text-green-800 px-2 py-1 rounded text-xs font-bold"] "PUBLISHED"
renderStatusBadge Archived =
  Lucid.span_ [Lucid.class_ "inline-block bg-red-100 text-red-800 px-2 py-1 rounded text-xs font-bold"] "ARCHIVED"
