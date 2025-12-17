{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Blog.Post.Get.Templates.Page where

--------------------------------------------------------------------------------

import API.Links (showBlogLinks)
import API.Types
import Control.Monad (forM_, unless)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Markdown (renderContent)
import Lucid qualified
import Lucid.Extras
import Lucid.Responsive (cls, md)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
showBlogGetUrl :: Slug -> Links.URI
showBlogGetUrl slug = Links.linkURI $ showBlogLinks.list slug Nothing Nothing

--------------------------------------------------------------------------------

template :: Shows.Model -> ShowBlogPosts.Model -> UserMetadata.Model -> [ShowBlogTags.Model] -> Lucid.Html ()
template showModel post author tags = do
  Lucid.article_ [Lucid.class_ $ cls ["max-w-4xl", "mx-auto", Tokens.px4, "py-12"]] $ do
    -- Article header
    Lucid.header_ [Lucid.class_ $ cls [Tokens.mb8, Tokens.pb2, "pb-6", "border-b-2", "border-gray-800"]] $ do
      -- Title
      Lucid.h1_ [Lucid.class_ $ cls ["text-4xl", md "text-5xl", Tokens.fontBold, Tokens.mb4]] $ do
        Lucid.toHtml (ShowBlogPosts.title post)

      -- Meta information
      Lucid.div_ [Lucid.class_ $ cls ["flex", "flex-wrap", "items-center", Tokens.gap4, Tokens.textGray600]] $ do
        -- Author
        Lucid.div_ [Lucid.class_ $ cls ["flex", "items-center", Tokens.gap2]] $ do
          Lucid.span_ [Lucid.class_ Tokens.fontBold] "By"
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
        Lucid.div_ [Lucid.class_ $ cls ["mt-4", "flex", "flex-wrap", Tokens.gap2]] $ do
          forM_ tags $ \tag -> do
            Lucid.a_
              [ Lucid.href_ [i|/#{showBlogGetUrl (Shows.slug showModel)}?tag=#{ShowBlogTags.sbtmName tag}|],
                hxGet_ [i|/#{showBlogGetUrl (Shows.slug showModel)}?tag=#{ShowBlogTags.sbtmName tag}|],
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                Lucid.class_ $ cls [Tokens.px3, "py-1", Tokens.bgGray800, Tokens.textWhite, Tokens.textSm, Tokens.fontBold, "hover:bg-gray-700"]
              ]
              $ Lucid.toHtml (ShowBlogTags.sbtmName tag)

    -- Article content
    renderContent (ShowBlogPosts.content post)

    -- Back to blog link
    Lucid.div_ [Lucid.class_ $ cls ["mt-12", Tokens.p6, "pt-6", "border-t-2", "border-gray-800"]] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogGetUrl (Shows.slug showModel)}|],
          hxGet_ [i|/#{showBlogGetUrl (Shows.slug showModel)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ $ cls ["text-blue-600", Tokens.fontBold, "hover:underline"]
        ]
        "← Back to blog"

--------------------------------------------------------------------------------

notFoundTemplate :: Slug -> Slug -> Lucid.Html ()
notFoundTemplate showSlug postSlug = do
  Lucid.div_ [Lucid.class_ $ cls ["max-w-4xl", "mx-auto", Tokens.px4, "py-12"]] $ do
    Lucid.div_ [Lucid.class_ "text-center"] $ do
      Lucid.h1_ [Lucid.class_ $ cls ["text-4xl", Tokens.fontBold, Tokens.mb4]] "Blog Post Not Found"
      Lucid.p_ [Lucid.class_ $ cls [Tokens.textXl, Tokens.textGray600, Tokens.mb8]] $ do
        "We couldn't find the blog post at: "
        Lucid.code_ [Lucid.class_ $ cls [Tokens.bgGray100, "px-2", "py-1"]] $ do
          Lucid.toHtml $ display showSlug <> "/blog/" <> display postSlug

      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogGetUrl showSlug}|],
          hxGet_ [i|/#{showBlogGetUrl showSlug}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ $ cls ["inline-block", Tokens.px6, "py-3", Tokens.bgGray800, Tokens.textWhite, Tokens.fontBold, "hover:bg-gray-700"]
        ]
        "← Back to blog"

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ $ cls ["max-w-4xl", "mx-auto", Tokens.px4, "py-12"]] $ do
    Lucid.div_ [Lucid.class_ $ cls [Tokens.errorBg, Tokens.border2, Tokens.errorBorder, Tokens.p6]] $ do
      Lucid.h2_ [Lucid.class_ $ cls [Tokens.text2xl, Tokens.fontBold, Tokens.mb2, Tokens.errorText]] "Error"
      Lucid.p_ [Lucid.class_ "text-red-700"] $ Lucid.toHtml errorMsg
