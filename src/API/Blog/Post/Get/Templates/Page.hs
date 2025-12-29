{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Post.Get.Templates.Page
  ( template,
    notFoundTemplate,
    renderTags,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, blogLinks)
import API.Types
import Control.Monad (unless)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Lucid qualified as Layout
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Markdown (renderContent)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogLinks.list Nothing Nothing

blogGetTagUrl :: Text -> Links.URI
blogGetTagUrl tag = Links.linkURI $ blogLinks.list Nothing (Just tag)

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

--------------------------------------------------------------------------------

-- | Render tags for a blog post
renderTags :: [BlogTags.Model] -> Lucid.Html ()
renderTags tags = do
  Lucid.div_ [class_ $ base ["flex", Tokens.gap2, Tokens.mb6]] $ do
    mapM_ renderTag tags
  where
    renderTag :: BlogTags.Model -> Lucid.Html ()
    renderTag tag =
      Lucid.a_
        [ Lucid.href_ [i|/#{blogGetTagUrl (BlogTags.btmName tag)}|],
          hxGet_ [i|/#{blogGetTagUrl (BlogTags.btmName tag)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ tagStyle
        ]
        $ Lucid.toHtml
        $ "#" <> BlogTags.btmName tag

    tagStyle = "bg-gray-200 text-gray-800 px-2 py-1 text-xs font-mono hover:bg-gray-300 cursor-pointer"

-- | Main blog post template
template :: BlogPosts.Model -> UserMetadata.Model -> [BlogTags.Model] -> Lucid.Html ()
template post author tags = do
  -- Blog Post Content
  Lucid.article_ [class_ $ base [Tokens.cardBase, Tokens.fullWidth]] $ do
    -- Post Header
    Lucid.header_ [Lucid.class_ Tokens.mb8] $ do
      -- Title
      Lucid.h1_ [class_ $ base [Tokens.heading2xl, Tokens.mb4, "leading-tight"]] $
        Lucid.toHtml (BlogPosts.bpmTitle post)

      -- Hero Image (if present)
      case BlogPosts.bpmHeroImageUrl post of
        Just heroImageUrl -> do
          Lucid.div_ [Lucid.class_ Tokens.mb6] $ do
            Lucid.img_
              [ Lucid.src_ [i|/#{mediaGetUrl}/#{heroImageUrl}|],
                Lucid.alt_ $ BlogPosts.bpmTitle post,
                class_ $ base [Tokens.fullWidth, "h-auto", "border", "border-gray-300"]
              ]
        Nothing -> pure ()

      -- Metadata
      Lucid.div_ [class_ $ base ["flex items-center", Tokens.gap6, Tokens.metaText, Tokens.mb6]] $ do
        Lucid.div_ [class_ $ base ["flex items-center", Tokens.gap2]] $ do
          Lucid.div_ [Lucid.class_ "w-8 h-8 bg-gray-300 rounded-full flex items-center justify-center text-xs"] $
            Lucid.toHtml $
              Text.take 2 (display (UserMetadata.mDisplayName author))
          Lucid.span_ $ do
            "By "
            Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray800]] $
              Lucid.toHtml (display (UserMetadata.mDisplayName author))

        case BlogPosts.bpmPublishedAt post of
          Just publishedAt -> do
            let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
            Lucid.span_ $ Lucid.toHtml dateStr
          Nothing -> Lucid.span_ "Draft"

      -- Tags
      unless (null tags) $
        renderTags tags

    -- Post Content
    renderContent (BlogPosts.bpmContent post)

    -- Post Footer
    Lucid.footer_ [Lucid.class_ "mt-8 pt-8 border-t border-gray-300"] $ do
      -- Author Bio
      Lucid.div_ [class_ $ base [Tokens.bgGray100, Tokens.p6, "border-l-4", Tokens.borderGray800]] $ do
        Lucid.div_ [class_ $ base ["flex items-start", Tokens.gap4]] $ do
          Lucid.div_ [Lucid.class_ "w-16 h-16 bg-gray-300 rounded-full flex items-center justify-center text-xl"] $
            Lucid.toHtml $
              Text.take 2 (display (UserMetadata.mDisplayName author))
          Lucid.div_ $ do
            Lucid.h3_ [class_ $ base [Tokens.fontBold, Tokens.textLg, Tokens.mb2]] $
              Lucid.toHtml (display (UserMetadata.mDisplayName author))
            Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.textGray600, "leading-relaxed"]] $
              Lucid.toHtml $
                Text.pack $
                  "KPBJ " <> show (UserMetadata.mUserRole author) <> " • " <> Text.unpack (display (UserMetadata.mFullName author))

  -- Navigation
  Lucid.div_ [Lucid.class_ "mt-8 text-center"] $ do
    Lucid.a_
      [ Lucid.href_ [i|/#{blogGetUrl}|],
        hxGet_ [i|/#{blogGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        backButtonStyle
      ]
      "← BACK TO BLOG"
  where
    backButtonStyle = class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700", "inline-block"]

-- | Template for when blog post is not found
notFoundTemplate :: Slug -> Lucid.Html ()
notFoundTemplate slug = do
  Layout.cardSection $ do
    Lucid.div_ [class_ $ base ["text-center"]] $ do
      Lucid.h1_ [class_ $ base [Tokens.heading2xl, Tokens.mb4]] "Blog Post Not Found"
      Lucid.p_ [class_ $ base [Tokens.textGray600, Tokens.mb6]] $ do
        "The blog post with slug \""
        Lucid.code_ [class_ $ base ["bg-gray-100", "px-2", "py-1"]] $ Lucid.toHtml $ display slug
        "\" could not be found."
      Lucid.a_
        [ Lucid.href_ [i|/#{blogGetUrl}|],
          hxGet_ [i|/#{blogGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          backButtonStyle
        ]
        "← BACK TO BLOG"
  where
    backButtonStyle = class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700", "inline-block"]
