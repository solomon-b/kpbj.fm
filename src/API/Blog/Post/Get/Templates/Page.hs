{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Post.Get.Templates.Page
  ( template,
    notFoundTemplate,
    renderBlogContent,
    renderTags,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogGetLink, mediaGetLink)
import Control.Monad (unless)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing

blogGetTagUrl :: Text -> Links.URI
blogGetTagUrl tag = Links.linkURI $ blogGetLink Nothing (Just tag)

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI mediaGetLink

--------------------------------------------------------------------------------

-- | Render blog post content with safe HTML and paragraph breaks
--
-- This function assumes content has already been sanitized during form submission.
-- Content is rendered using Lucid.toHtmlRaw since it has been processed through xss-sanitize.
renderBlogContent :: Text -> Lucid.Html ()
renderBlogContent content = do
  let paragraphs = Text.splitOn "\n\n" content
  Lucid.div_ [Lucid.class_ "prose max-w-none text-gray-700 leading-relaxed space-y-6"] $ do
    mapM_ renderParagraph paragraphs
  where
    renderParagraph para =
      if Text.null (Text.strip para)
        then pure ()
        else Lucid.p_ $ Lucid.toHtmlRaw para

-- | Render tags for a blog post
renderTags :: [BlogTags.Model] -> Lucid.Html ()
renderTags tags = do
  Lucid.div_ [Lucid.class_ "flex gap-2 mb-6"] $ do
    mapM_ renderTag tags
  where
    renderTag :: BlogTags.Model -> Lucid.Html ()
    renderTag tag =
      Lucid.a_
        [ Lucid.href_ [i|/#{blogGetTagUrl (BlogTags.btmName tag)}|],
          hxGet_ [i|/#{blogGetTagUrl (BlogTags.btmName tag)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-200 text-gray-800 px-2 py-1 text-xs font-mono hover:bg-gray-300 cursor-pointer"
        ]
        $ Lucid.toHtml
        $ "#" <> BlogTags.btmName tag

-- | Main blog post template
template :: BlogPosts.Model -> UserMetadata.Model -> [BlogTags.Model] -> Lucid.Html ()
template post author tags = do
  -- Blog Post Content
  Lucid.article_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 w-full"] $ do
    -- Post Header
    Lucid.header_ [Lucid.class_ "mb-8"] $ do
      -- Title
      Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4 leading-tight"] $
        Lucid.toHtml (BlogPosts.bpmTitle post)

      -- Hero Image (if present)
      case BlogPosts.bpmHeroImageUrl post of
        Just heroImageUrl -> do
          Lucid.div_ [Lucid.class_ "mb-6"] $ do
            Lucid.img_
              [ Lucid.src_ [i|/#{mediaGetUrl}/#{heroImageUrl}|],
                Lucid.alt_ $ BlogPosts.bpmTitle post,
                Lucid.class_ "w-full h-auto border-2 border-gray-300"
              ]
        Nothing -> pure ()

      -- Metadata
      Lucid.div_ [Lucid.class_ "flex items-center gap-6 text-sm text-gray-600 mb-6"] $ do
        Lucid.div_ [Lucid.class_ "flex items-center gap-2"] $ do
          Lucid.div_ [Lucid.class_ "w-8 h-8 bg-gray-300 rounded-full flex items-center justify-center text-xs"] $
            Lucid.toHtml $
              Text.take 2 (display (UserMetadata.mDisplayName author))
          Lucid.span_ $ do
            "By "
            Lucid.span_ [Lucid.class_ "font-bold text-gray-800"] $
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
    renderBlogContent (BlogPosts.bpmContent post)

    -- Post Footer
    Lucid.footer_ [Lucid.class_ "mt-8 pt-8 border-t border-gray-300"] $ do
      -- Author Bio
      Lucid.div_ [Lucid.class_ "bg-gray-50 p-6 border-l-4 border-gray-800"] $ do
        Lucid.div_ [Lucid.class_ "flex items-start gap-4"] $ do
          Lucid.div_ [Lucid.class_ "w-16 h-16 bg-gray-300 rounded-full flex items-center justify-center text-xl"] $
            Lucid.toHtml $
              Text.take 2 (display (UserMetadata.mDisplayName author))
          Lucid.div_ $ do
            Lucid.h3_ [Lucid.class_ "font-bold text-lg mb-2"] $
              Lucid.toHtml (display (UserMetadata.mDisplayName author))
            Lucid.p_ [Lucid.class_ "text-sm text-gray-600 leading-relaxed"] $
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
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
      ]
      "← BACK TO BLOG"

-- | Template for when blog post is not found
notFoundTemplate :: Slug -> Lucid.Html ()
notFoundTemplate slug = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4"] "Blog Post Not Found"
    Lucid.p_ [Lucid.class_ "text-gray-600 mb-6"] $ do
      "The blog post with slug \""
      Lucid.code_ [Lucid.class_ "bg-gray-100 px-2 py-1"] $ Lucid.toHtml $ display slug
      "\" could not be found."
    Lucid.a_
      [ Lucid.href_ [i|/#{blogGetUrl}|],
        hxGet_ [i|/#{blogGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
      ]
      "← BACK TO BLOG"
