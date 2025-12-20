{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Get.Templates.PostCard
  ( renderBlogPostCard,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, blogLinks)
import API.Types
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Helpers.RelativeTime (formatRelativeTime)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

blogPostGetUrl :: BlogPosts.Id -> Slug -> Links.URI
blogPostGetUrl postId slug = Links.linkURI $ blogLinks.postWithSlug postId slug

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

--------------------------------------------------------------------------------

-- | Render a blog post card for the list view.
--
-- The entire card is wrapped in a link for easy tap/click navigation.
-- Layout shows:
-- - Post title
-- - Hero image (if present)
-- - Excerpt
-- - Author name + relative time
renderBlogPostCard :: UTCTime -> (BlogPosts.Model, UserMetadata.Model, [BlogTags.Model]) -> Lucid.Html ()
renderBlogPostCard currentTime (post, author, _tags) = do
  Lucid.a_
    [ Lucid.href_ [i|/#{blogPostGetUrl (BlogPosts.bpmId post) (BlogPosts.bpmSlug post)}|],
      hxGet_ [i|/#{blogPostGetUrl (BlogPosts.bpmId post) (BlogPosts.bpmSlug post)}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      class_ $ base ["block"]
    ]
    $ do
      Lucid.article_ [class_ $ base [Tokens.cardBase, Tokens.mb6]] $ do
        -- Post title
        renderTitle post

        -- Hero image (if present)
        renderHeroImage post

        -- Excerpt
        renderExcerpt post

        -- Author row: name + relative time
        renderAuthorRow currentTime post author

--------------------------------------------------------------------------------
-- Internal render helpers

-- | Render the author row with avatar, name, and relative time
renderAuthorRow :: UTCTime -> BlogPosts.Model -> UserMetadata.Model -> Lucid.Html ()
renderAuthorRow currentTime post author = do
  Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between", Tokens.mt4]] $ do
    Lucid.span_ [class_ $ base [Tokens.fontBold, "text-gray-900"]] $
      Lucid.toHtml (display (UserMetadata.mDisplayName author))
    Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.textGray600]] $
      Lucid.toHtml $ case BlogPosts.bpmPublishedAt post of
        Just publishedAt -> formatRelativeTime currentTime publishedAt
        Nothing -> "Draft"

-- | Render the post title
renderTitle :: BlogPosts.Model -> Lucid.Html ()
renderTitle post = do
  Lucid.h2_ [class_ $ base [Tokens.fontBold, "text-xl", Tokens.mb4]] $
    Lucid.toHtml (BlogPosts.bpmTitle post)

-- | Render the hero image if present
renderHeroImage :: BlogPosts.Model -> Lucid.Html ()
renderHeroImage post = do
  case BlogPosts.bpmHeroImageUrl post of
    Just heroImageUrl -> do
      Lucid.div_ [Lucid.class_ Tokens.mb4] $
        Lucid.img_
          [ Lucid.src_ [i|/#{mediaGetUrl}/#{heroImageUrl}|],
            Lucid.alt_ $ BlogPosts.bpmTitle post,
            class_ $ base [Tokens.fullWidth, "h-auto", Tokens.border2, Tokens.borderGray400]
          ]
    Nothing -> pure ()

-- | Render the post excerpt
renderExcerpt :: BlogPosts.Model -> Lucid.Html ()
renderExcerpt post = do
  case BlogPosts.bpmExcerpt post of
    Just excerpt ->
      Lucid.p_ [class_ $ base [Tokens.textGray700, "leading-relaxed"]] $
        Lucid.toHtml excerpt
    Nothing -> do
      let truncatedContent = Text.take 200 (BlogPosts.bpmContent post)
      Lucid.p_ [class_ $ base [Tokens.textGray700, "leading-relaxed"]] $
        Lucid.toHtml $
          truncatedContent <> if Text.length (BlogPosts.bpmContent post) > 200 then "..." else ""
