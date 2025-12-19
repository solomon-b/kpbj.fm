{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Get.Templates.PostCard
  ( renderBlogPostCard,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, blogLinks)
import API.Types
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design.StyleBuilder.Internal (cls)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

blogPostGetUrl :: BlogPosts.Id -> Slug -> Links.URI
blogPostGetUrl postId slug = Links.linkURI $ blogLinks.postWithSlug postId slug

blogGetTagUrl :: Text -> Links.URI
blogGetTagUrl tag = Links.linkURI $ blogLinks.list Nothing (Just tag)

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

--------------------------------------------------------------------------------

-- | Render a blog post card for the list view
renderBlogPostCard :: (BlogPosts.Model, [BlogTags.Model]) -> Lucid.Html ()
renderBlogPostCard (post, tags) = do
  Lucid.article_ [Lucid.class_ $ cls [Tokens.cardBase, Tokens.mb6]] $ do
    -- Hero Image (if present) - shown as thumbnail in listing
    case BlogPosts.bpmHeroImageUrl post of
      Just heroImageUrl -> do
        Lucid.div_ [Lucid.class_ Tokens.mb4] $ do
          Lucid.a_
            [ Lucid.href_ [i|/#{blogPostGetUrl (BlogPosts.bpmId post) (BlogPosts.bpmSlug post)}|],
              hxGet_ [i|/#{blogPostGetUrl (BlogPosts.bpmId post) (BlogPosts.bpmSlug post)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true"
            ]
            $ Lucid.img_
              [ Lucid.src_ [i|/#{mediaGetUrl}/#{heroImageUrl}|],
                Lucid.alt_ $ BlogPosts.bpmTitle post,
                Lucid.class_ $ cls [Tokens.fullWidth, "h-48 object-cover", Tokens.border2, Tokens.borderGray400]
              ]
      Nothing -> pure ()

    -- Title
    Lucid.h2_ [Lucid.class_ $ cls [Tokens.headingLg, "mb-3"]] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{blogPostGetUrl (BlogPosts.bpmId post) (BlogPosts.bpmSlug post)}|],
          hxGet_ [i|/#{blogPostGetUrl (BlogPosts.bpmId post) (BlogPosts.bpmSlug post)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "hover:underline"
        ]
        $ Lucid.toHtml (BlogPosts.bpmTitle post)

    -- Metadata
    Lucid.div_ [Lucid.class_ $ cls [Tokens.metaText, Tokens.mb4]] $ do
      case BlogPosts.bpmPublishedAt post of
        Just publishedAt -> do
          let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
          Lucid.toHtml $ "Published " <> dateStr
        Nothing -> "Draft"

    -- Excerpt
    case BlogPosts.bpmExcerpt post of
      Just excerpt ->
        Lucid.p_ [Lucid.class_ $ cls [Tokens.textGray700, Tokens.mb4, "leading-relaxed"]] $
          Lucid.toHtml excerpt
      Nothing -> do
        let truncatedContent = Text.take 200 (BlogPosts.bpmContent post)
        Lucid.p_ [Lucid.class_ $ cls [Tokens.textGray700, Tokens.mb4, "leading-relaxed"]] $
          Lucid.toHtml $
            truncatedContent <> if Text.length (BlogPosts.bpmContent post) > 200 then "..." else ""

    -- Read more link and tags
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ [Lucid.class_ $ cls ["flex items-center", Tokens.gap4, Tokens.textSm, Tokens.textGray600]] $ do
        renderTags tags
      Lucid.a_
        [ Lucid.href_ [i|/#{blogPostGetUrl (BlogPosts.bpmId post) (BlogPosts.bpmSlug post)}|],
          hxGet_ [i|/#{blogPostGetUrl (BlogPosts.bpmId post) (BlogPosts.bpmSlug post)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ Tokens.buttonPrimary
        ]
        "READ MORE"

-- | Render tags for a blog post
renderTags :: [BlogTags.Model] -> Lucid.Html ()
renderTags tags = do
  Lucid.div_ [Lucid.class_ $ cls ["flex", Tokens.gap2, Tokens.mb6]] $ do
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
