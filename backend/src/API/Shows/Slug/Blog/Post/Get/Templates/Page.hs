{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Blog.Post.Get.Templates.Page where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showBlogGetLink)
import Control.Monad (forM_, unless)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
showBlogGetUrl :: Slug -> Links.URI
showBlogGetUrl slug = Links.linkURI $ showBlogGetLink slug Nothing Nothing

--------------------------------------------------------------------------------

template :: Shows.Model -> ShowBlogPosts.Model -> UserMetadata.Model -> [ShowBlogTags.Model] -> Lucid.Html ()
template showModel post author tags = do
  Lucid.article_ [Lucid.class_ "max-w-4xl mx-auto px-4 py-12"] $ do
    -- Article header
    Lucid.header_ [Lucid.class_ "mb-8 pb-6 border-b-2 border-gray-800"] $ do
      -- Title
      Lucid.h1_ [Lucid.class_ "text-4xl md:text-5xl font-bold mb-4"] $ do
        Lucid.toHtml (ShowBlogPosts.title post)

      -- Meta information
      Lucid.div_ [Lucid.class_ "flex flex-wrap items-center gap-4 text-gray-600"] $ do
        -- Author
        Lucid.div_ [Lucid.class_ "flex items-center gap-2"] $ do
          Lucid.span_ [Lucid.class_ "font-bold"] "By"
          Lucid.span_ $ Lucid.toHtml (UserMetadata.mDisplayName author)

        -- Date
        Lucid.div_ $ do
          case ShowBlogPosts.publishedAt post of
            Just publishedTime ->
              Lucid.time_ [Lucid.datetime_ (Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d" publishedTime)] $
                Lucid.toHtml $
                  formatTime defaultTimeLocale "%B %e, %Y" publishedTime
            Nothing ->
              Lucid.time_ [Lucid.datetime_ (Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d" (ShowBlogPosts.createdAt post))] $
                Lucid.toHtml $
                  formatTime defaultTimeLocale "%B %e, %Y" (ShowBlogPosts.createdAt post)

      -- Tags
      unless (null tags) $ do
        Lucid.div_ [Lucid.class_ "mt-4 flex flex-wrap gap-2"] $ do
          forM_ tags $ \tag -> do
            Lucid.a_
              [ Lucid.href_ [i|/#{showBlogGetUrl (Shows.slug showModel)}?tag=#{ShowBlogTags.sbtmName tag}|],
                hxGet_ [i|/#{showBlogGetUrl (Shows.slug showModel)}?tag=#{ShowBlogTags.sbtmName tag}|],
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                Lucid.class_ "px-3 py-1 bg-gray-800 text-white text-sm font-bold hover:bg-gray-700"
              ]
              $ Lucid.toHtml (ShowBlogTags.sbtmName tag)

    -- Article content
    Lucid.div_ [Lucid.class_ "prose prose-lg max-w-none"] $ do
      -- Render content with basic formatting preservation
      -- Note: In a production app, you'd want to parse markdown or HTML here
      Lucid.div_ [Lucid.class_ "whitespace-pre-wrap"] $ do
        Lucid.toHtml (ShowBlogPosts.content post)

    -- Back to blog link
    Lucid.div_ [Lucid.class_ "mt-12 pt-6 border-t-2 border-gray-800"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogGetUrl (Shows.slug showModel)}|],
          hxGet_ [i|/#{showBlogGetUrl (Shows.slug showModel)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "text-blue-600 font-bold hover:underline"
        ]
        "← Back to blog"

--------------------------------------------------------------------------------

notFoundTemplate :: Slug -> Slug -> Lucid.Html ()
notFoundTemplate showSlug postSlug = do
  Lucid.div_ [Lucid.class_ "max-w-4xl mx-auto px-4 py-12"] $ do
    Lucid.div_ [Lucid.class_ "text-center"] $ do
      Lucid.h1_ [Lucid.class_ "text-4xl font-bold mb-4"] "Blog Post Not Found"
      Lucid.p_ [Lucid.class_ "text-xl text-gray-600 mb-8"] $ do
        "We couldn't find the blog post at: "
        Lucid.code_ [Lucid.class_ "bg-gray-100 px-2 py-1"] $ do
          Lucid.toHtml $ display showSlug <> "/blog/" <> display postSlug

      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogGetUrl showSlug}|],
          hxGet_ [i|/#{showBlogGetUrl showSlug}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "inline-block px-6 py-3 bg-gray-800 text-white font-bold hover:bg-gray-700"
        ]
        "← Back to blog"

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "max-w-4xl mx-auto px-4 py-12"] $ do
    Lucid.div_ [Lucid.class_ "bg-red-50 border-2 border-red-600 p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-2 text-red-600"] "Error"
      Lucid.p_ [Lucid.class_ "text-red-700"] $ Lucid.toHtml errorMsg
