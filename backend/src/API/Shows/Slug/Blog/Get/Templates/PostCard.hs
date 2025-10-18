{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Blog.Get.Templates.PostCard (renderPostCard) where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showBlogPostGetLink)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helper
showBlogPostGetUrl :: Shows.Model -> ShowBlogPosts.Model -> Links.URI
showBlogPostGetUrl showModel post =
  Links.linkURI $ showBlogPostGetLink (Shows.slug showModel) (ShowBlogPosts.slug post)

--------------------------------------------------------------------------------

renderPostCard :: Shows.Model -> ShowBlogPosts.Model -> Lucid.Html ()
renderPostCard showModel post = do
  Lucid.article_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 hover:shadow-lg transition-shadow"] $ do
    -- Date
    case ShowBlogPosts.publishedAt post of
      Just publishedTime ->
        Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-2"] $ do
          Lucid.toHtml $ formatTime defaultTimeLocale "%B %e, %Y" publishedTime
      Nothing ->
        Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-2"] $ do
          Lucid.toHtml $ formatTime defaultTimeLocale "%B %e, %Y" (ShowBlogPosts.createdAt post)

    -- Title
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-3"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogPostGetUrl showModel post}|],
          hxGet_ [i|/#{showBlogPostGetUrl showModel post}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "hover:underline"
        ]
        $ Lucid.toHtml (ShowBlogPosts.title post)

    -- Excerpt
    case ShowBlogPosts.excerpt post of
      Just exc -> do
        Lucid.p_ [Lucid.class_ "text-gray-700 mb-4"] $ Lucid.toHtml exc
      Nothing -> do
        -- Generate excerpt from content (first 150 characters)
        let contentPreview = Text.take 150 (ShowBlogPosts.content post)
        let excerpt = if Text.length (ShowBlogPosts.content post) > 150 then contentPreview <> "..." else contentPreview
        Lucid.p_ [Lucid.class_ "text-gray-700 mb-4"] $ Lucid.toHtml excerpt

    -- Read more link
    Lucid.a_
      [ Lucid.href_ [i|/#{showBlogPostGetUrl showModel post}|],
        hxGet_ [i|/#{showBlogPostGetUrl showModel post}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "text-blue-600 font-bold hover:underline"
      ]
      "Read more →"
