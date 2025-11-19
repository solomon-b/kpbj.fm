{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Get.Templates.PostCard
  ( renderBlogPostCard,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogGetLink, blogPostGetLink)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

blogPostGetUrl :: BlogPosts.Id -> Slug -> Links.URI
blogPostGetUrl postId slug = Links.linkURI $ blogPostGetLink postId slug

blogGetTagUrl :: Text -> Links.URI
blogGetTagUrl tag = Links.linkURI $ blogGetLink Nothing (Just tag)

--------------------------------------------------------------------------------

-- | Render a blog post card for the list view
renderBlogPostCard :: (BlogPosts.Model, [BlogTags.Model]) -> Lucid.Html ()
renderBlogPostCard (post, tags) = do
  Lucid.article_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-6"] $ do
    -- Title
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-3"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{blogPostGetUrl (BlogPosts.bpmId post) (BlogPosts.bpmSlug post)}|],
          hxGet_ [i|/#{blogPostGetUrl (BlogPosts.bpmId post) (BlogPosts.bpmSlug post)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "hover:underline"
        ]
        $ Lucid.toHtml (BlogPosts.bpmTitle post)

    -- Metadata
    Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-4"] $ do
      case BlogPosts.bpmPublishedAt post of
        Just publishedAt -> do
          let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
          Lucid.toHtml $ "Published " <> dateStr
        Nothing -> "Draft"

    -- Excerpt
    case BlogPosts.bpmExcerpt post of
      Just excerpt ->
        Lucid.p_ [Lucid.class_ "text-gray-700 mb-4 leading-relaxed"] $
          Lucid.toHtml excerpt
      Nothing -> do
        let truncatedContent = Text.take 200 (BlogPosts.bpmContent post)
        Lucid.p_ [Lucid.class_ "text-gray-700 mb-4 leading-relaxed"] $
          Lucid.toHtml $
            truncatedContent <> if Text.length (BlogPosts.bpmContent post) > 200 then "..." else ""

    -- Read more link and tags
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ [Lucid.class_ "flex items-center gap-4 text-sm text-gray-600"] $ do
        renderTags tags
      Lucid.a_
        [ Lucid.href_ [i|/#{blogPostGetUrl (BlogPosts.bpmId post) (BlogPosts.bpmSlug post)}|],
          hxGet_ [i|/#{blogPostGetUrl (BlogPosts.bpmId post) (BlogPosts.bpmSlug post)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-4 py-2 font-bold hover:bg-gray-700"
        ]
        "READ MORE"

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
