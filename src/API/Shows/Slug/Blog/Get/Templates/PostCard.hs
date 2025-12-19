{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Blog.Get.Templates.PostCard (renderPostCard) where

--------------------------------------------------------------------------------

import API.Links (showBlogLinks)
import API.Types
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design.StyleBuilder.Internal (cls)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
showBlogPostGetUrl :: Shows.Model -> ShowBlogPosts.Model -> Links.URI
showBlogPostGetUrl showModel post =
  Links.linkURI $ showBlogLinks.postWithSlug (Shows.id showModel) (ShowBlogPosts.id post) (ShowBlogPosts.slug post)

--------------------------------------------------------------------------------

renderPostCard :: Shows.Model -> ShowBlogPosts.Model -> Lucid.Html ()
renderPostCard showModel post = do
  let postId = [i|post-#{ShowBlogPosts.id post}|]
  Lucid.article_
    [ Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6, "hover:shadow-lg", "transition-shadow"],
      Lucid.id_ postId
    ]
    $ do
      -- Date
      case ShowBlogPosts.publishedAt post of
        Just publishedTime ->
          Lucid.div_ [Lucid.class_ $ cls [Tokens.textSm, Tokens.textGray600, Tokens.mb2]] $ do
            Lucid.toHtml $ formatTime defaultTimeLocale "%B %e, %Y" publishedTime
        Nothing ->
          Lucid.div_ [Lucid.class_ $ cls [Tokens.textSm, Tokens.textGray600, Tokens.mb2]] $ do
            Lucid.toHtml $ formatTime defaultTimeLocale "%B %e, %Y" (ShowBlogPosts.createdAt post)

      -- Title
      Lucid.h2_ [Lucid.class_ $ cls [Tokens.text2xl, Tokens.fontBold, "mb-3"]] $ do
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
          Lucid.p_ [Lucid.class_ $ cls [Tokens.textGray700, Tokens.mb4]] $ Lucid.toHtml exc
        Nothing -> do
          -- Generate excerpt from content (first 150 characters)
          let contentPreview = Text.take 150 (ShowBlogPosts.content post)
          let excerpt = if Text.length (ShowBlogPosts.content post) > 150 then contentPreview <> "..." else contentPreview
          Lucid.p_ [Lucid.class_ $ cls [Tokens.textGray700, Tokens.mb4]] $ Lucid.toHtml excerpt

      -- Read more link and action buttons
      Lucid.div_ [Lucid.class_ $ cls ["flex", "items-center", "justify-between", "mt-4"]] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{showBlogPostGetUrl showModel post}|],
            hxGet_ [i|/#{showBlogPostGetUrl showModel post}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ $ cls ["text-blue-600", Tokens.fontBold, "hover:underline"]
          ]
          "Read more →"
