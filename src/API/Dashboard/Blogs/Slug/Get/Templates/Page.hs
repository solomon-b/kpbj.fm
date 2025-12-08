{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Blogs.Slug.Get.Templates.Page
  ( template,
    errorTemplate,
    notFoundTemplate,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardBlogsLinks, showBlogLinks)
import API.Types
import Control.Monad (unless)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardBlogsGetUrl :: Shows.Model -> Links.URI
dashboardBlogsGetUrl showModel = Links.linkURI $ dashboardBlogsLinks.list showModel.slug

blogEditGetUrl :: Shows.Id -> ShowBlogPosts.Id -> ShowBlogPosts.Model -> Links.URI
blogEditGetUrl showId postId post = Links.linkURI $ showBlogLinks.editGet showId postId post.slug

--------------------------------------------------------------------------------

-- | Dashboard blog post detail template
template :: UserMetadata.Model -> Shows.Model -> ShowBlogPosts.Model -> [ShowBlogTags.Model] -> Lucid.Html ()
template _userMeta showModel blogPost tags = do
  -- Back button and header
  Lucid.div_ [Lucid.class_ "mb-6"] $ do
    let backUrl = dashboardBlogsGetUrl showModel
    Lucid.a_
      [ Lucid.href_ [i|/#{backUrl}|],
        hxGet_ [i|/#{backUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "text-gray-600 hover:text-gray-900 text-sm inline-flex items-center gap-2"
      ]
      $ do
        Lucid.i_ [Lucid.class_ "fa-solid fa-arrow-left"] mempty
        "Back to Blog Posts"

  -- Main blog post container
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800"] $ do
    -- Blog post header
    Lucid.div_ [Lucid.class_ "border-b-2 border-gray-800 p-6"] $ do
      Lucid.div_ [Lucid.class_ "flex items-start justify-between mb-4"] $ do
        Lucid.div_ $ do
          Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] $ Lucid.toHtml blogPost.title

          -- Status badge
          Lucid.div_ [Lucid.class_ "mb-4"] $
            renderStatusBadge blogPost.status

        -- Edit button
        let editUrl = blogEditGetUrl showModel.id blogPost.id blogPost
        Lucid.a_
          [ Lucid.href_ [i|/#{editUrl}|],
            hxGet_ [i|/#{editUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700"
          ]
          "Edit Post"

      -- Blog post metadata grid
      Lucid.div_ [Lucid.class_ "grid grid-cols-2 gap-4 text-sm mb-4"] $ do
        -- Published date
        case blogPost.publishedAt of
          Just publishedAt -> do
            Lucid.div_ $ do
              Lucid.span_ [Lucid.class_ "font-bold text-gray-700"] "Published: "
              let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y at %I:%M %p" publishedAt
              Lucid.toHtml dateStr
          Nothing ->
            Lucid.div_ $ do
              Lucid.span_ [Lucid.class_ "font-bold text-gray-700"] "Published: "
              Lucid.span_ [Lucid.class_ "text-gray-500 italic"] "Not published"

        -- Created date
        Lucid.div_ $ do
          Lucid.span_ [Lucid.class_ "font-bold text-gray-700"] "Created: "
          let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" blogPost.createdAt
          Lucid.toHtml dateStr

        -- Updated date
        Lucid.div_ $ do
          Lucid.span_ [Lucid.class_ "font-bold text-gray-700"] "Last Updated: "
          let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y at %I:%M %p" blogPost.updatedAt
          Lucid.toHtml dateStr

      -- Tags
      unless (null tags) $ do
        Lucid.div_ [Lucid.class_ "flex flex-wrap gap-2 mt-4"] $ do
          mapM_ renderTag tags

    -- Excerpt section (if present)
    case blogPost.excerpt of
      Just excerpt -> do
        Lucid.div_ [Lucid.class_ "border-b-2 border-gray-800 p-6 bg-gray-50"] $ do
          Lucid.h2_ [Lucid.class_ "text-sm font-bold uppercase text-gray-600 mb-2"] "Excerpt"
          Lucid.p_ [Lucid.class_ "text-gray-700 italic"] $ Lucid.toHtml excerpt
      Nothing -> mempty

    -- Content section
    Lucid.div_ [Lucid.class_ "p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Content"
      Lucid.div_ [Lucid.class_ "prose prose-gray max-w-none"] $ do
        -- Render content as paragraphs split by double newlines
        mapM_ renderParagraph $ Text.splitOn "\n\n" blogPost.content

--------------------------------------------------------------------------------

-- | Render a tag badge
renderTag :: ShowBlogTags.Model -> Lucid.Html ()
renderTag tag =
  Lucid.span_ [Lucid.class_ "inline-block bg-blue-100 text-blue-800 px-2 py-1 rounded text-xs font-medium"] $
    Lucid.toHtml tag.sbtmName

-- | Render a paragraph of content
renderParagraph :: Text -> Lucid.Html ()
renderParagraph para =
  unless (Text.null (Text.strip para)) $
    Lucid.p_ [Lucid.class_ "mb-4 leading-relaxed"] $
      Lucid.toHtml para

-- | Render status badge with appropriate styling
renderStatusBadge :: BlogPostStatus -> Lucid.Html ()
renderStatusBadge Draft =
  Lucid.span_ [Lucid.class_ "inline-block bg-yellow-100 text-yellow-800 px-2 py-1 rounded text-xs font-bold"] "DRAFT"
renderStatusBadge Published =
  Lucid.span_ [Lucid.class_ "inline-block bg-green-100 text-green-800 px-2 py-1 rounded text-xs font-bold"] "PUBLISHED"
renderStatusBadge Deleted =
  Lucid.span_ [Lucid.class_ "inline-block bg-red-100 text-red-800 px-2 py-1 rounded text-xs font-bold"] "DELETED"

--------------------------------------------------------------------------------

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-4"] "Error Loading Blog Post"
    Lucid.p_ [Lucid.class_ "text-gray-700 mb-6"] $ Lucid.toHtml errorMsg

notFoundTemplate :: Lucid.Html ()
notFoundTemplate = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-4"] "Blog Post Not Found"
    Lucid.p_ [Lucid.class_ "text-gray-700 mb-6"] "We couldn't find the blog post you're looking for."
