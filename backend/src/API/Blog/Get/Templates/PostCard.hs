{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Get.Templates.PostCard
  ( renderBlogPostCard,
  )
where

import {-# SOURCE #-} API (blogPostGetLink)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effects.Database.Tables.Blog qualified as Blog
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

-- | Render a blog post card for the list view
renderBlogPostCard :: Blog.BlogPostModel -> Lucid.Html ()
renderBlogPostCard post = do
  Lucid.article_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-6"] $ do
    -- Category badge
    Lucid.div_ [Lucid.class_ "mb-3"] $ do
      Lucid.span_ [Lucid.class_ "bg-blue-200 text-blue-800 px-2 py-1 text-xs font-bold"] $
        Lucid.toHtml (Blog.bpmCategory post)

    -- Title
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-3"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{blogPostGetUrl (Blog.bpmSlug post)}|],
          hxGet_ [i|/#{blogPostGetUrl (Blog.bpmSlug post)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "hover:underline"
        ]
        $ Lucid.toHtml (Blog.bpmTitle post)

    -- Metadata
    Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-4"] $ do
      case Blog.bpmPublishedAt post of
        Just publishedAt -> do
          let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
          Lucid.toHtml $ "Published " <> dateStr
        Nothing -> "Draft"

    -- Excerpt
    case Blog.bpmExcerpt post of
      Just excerpt ->
        Lucid.p_ [Lucid.class_ "text-gray-700 mb-4 leading-relaxed"] $
          Lucid.toHtml excerpt
      Nothing -> do
        let truncatedContent = Text.take 200 (Blog.bpmContent post)
        Lucid.p_ [Lucid.class_ "text-gray-700 mb-4 leading-relaxed"] $
          Lucid.toHtml $
            truncatedContent <> if Text.length (Blog.bpmContent post) > 200 then "..." else ""

    -- Read more link
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ [Lucid.class_ "flex items-center gap-4 text-sm text-gray-600"] $ do
        Lucid.span_ ""
      Lucid.a_
        [ Lucid.href_ [i|/#{blogPostGetUrl (Blog.bpmSlug post)}|],
          hxGet_ [i|/#{blogPostGetUrl (Blog.bpmSlug post)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-4 py-2 font-bold hover:bg-gray-700"
        ]
        "READ MORE"
  where
    blogPostGetUrl :: Text -> Links.URI
    blogPostGetUrl slug = Links.linkURI $ blogPostGetLink slug
