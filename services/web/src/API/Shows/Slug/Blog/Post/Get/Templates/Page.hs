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
import Design (base, class_, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
showBlogGetUrl :: Slug -> Links.URI
showBlogGetUrl slug = Links.linkURI $ showBlogLinks.list slug Nothing Nothing

--------------------------------------------------------------------------------

template :: Shows.Model -> ShowBlogPosts.Model -> UserMetadata.Model -> [ShowBlogTags.Model] -> Lucid.Html () -> Lucid.Html ()
template showModel post author tags renderedContent = do
  Lucid.article_ [class_ $ base ["max-w-4xl", "mx-auto", Tokens.px4, "py-12"]] $ do
    -- Article header
    Lucid.header_ [class_ $ base [Tokens.mb8, Tokens.pb2, "pb-6", "border-b-2", "Tokens.borderDefault"]] $ do
      -- Title
      Lucid.h1_ [class_ $ do { base ["text-4xl", Tokens.fontBold, Tokens.mb4]; tablet ["text-5xl"] }] $ do
        Lucid.toHtml (ShowBlogPosts.title post)

      -- Meta information
      Lucid.div_ [class_ $ base ["flex", "flex-wrap", "items-center", Tokens.gap4, Tokens.fgMuted]] $ do
        -- Author
        Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap2]] $ do
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
        Lucid.div_ [class_ $ base ["mt-4", "flex", "flex-wrap", Tokens.gap2]] $ do
          forM_ tags $ \tag -> do
            Lucid.a_
              [ Lucid.href_ [i|/#{showBlogGetUrl (Shows.slug showModel)}?tag=#{ShowBlogTags.sbtmName tag}|],
                hxGet_ [i|/#{showBlogGetUrl (Shows.slug showModel)}?tag=#{ShowBlogTags.sbtmName tag}|],
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                class_ $ base [Tokens.px3, "py-1", Tokens.bgInverse, Tokens.fgInverse, Tokens.textSm, Tokens.fontBold, "hover:opacity-80"]
              ]
              $ Lucid.toHtml (ShowBlogTags.sbtmName tag)

    -- Article content
    renderedContent

    -- Back to blog link
    Lucid.div_ [class_ $ base ["mt-12", Tokens.p6, "pt-6", "border-t-2", "Tokens.borderDefault"]] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogGetUrl (Shows.slug showModel)}|],
          hxGet_ [i|/#{showBlogGetUrl (Shows.slug showModel)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base [Tokens.infoText, Tokens.fontBold, "hover:underline"]
        ]
        "← Back to blog"

--------------------------------------------------------------------------------

notFoundTemplate :: Slug -> Slug -> Lucid.Html ()
notFoundTemplate showSlug postSlug = do
  Lucid.div_ [class_ $ base ["max-w-4xl", "mx-auto", Tokens.px4, "py-12"]] $ do
    Lucid.div_ [Lucid.class_ "text-center"] $ do
      Lucid.h1_ [class_ $ base ["text-4xl", Tokens.fontBold, Tokens.mb4]] "Blog Post Not Found"
      Lucid.p_ [class_ $ base [Tokens.textXl, Tokens.fgMuted, Tokens.mb8]] $ do
        "We couldn't find the blog post at: "
        Lucid.code_ [class_ $ base [Tokens.bgAlt, "px-2", "py-1"]] $ do
          Lucid.toHtml $ display showSlug <> "/blog/" <> display postSlug

      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogGetUrl showSlug}|],
          hxGet_ [i|/#{showBlogGetUrl showSlug}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base ["inline-block", Tokens.px6, "py-3", Tokens.bgInverse, Tokens.fgInverse, Tokens.fontBold, "hover:opacity-80"]
        ]
        "← Back to blog"

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [class_ $ base ["max-w-4xl", "mx-auto", Tokens.px4, "py-12"]] $ do
    Lucid.div_ [class_ $ base [Tokens.errorBg, Tokens.border2, Tokens.errorBorder, Tokens.p6]] $ do
      Lucid.h2_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2, Tokens.errorText]] "Error"
      Lucid.p_ [Lucid.class_ Tokens.errorText] $ Lucid.toHtml errorMsg
