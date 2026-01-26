{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Blogs.Slug.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardBlogsLinks)
import API.Types
import Control.Monad (unless)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardBlogsGetUrl :: Shows.Model -> Links.URI
dashboardBlogsGetUrl showModel = Links.linkURI $ dashboardBlogsLinks.list showModel.slug Nothing

blogEditGetUrl :: Slug -> ShowBlogPosts.Id -> Links.URI
blogEditGetUrl showSlug postId = Links.linkURI $ dashboardBlogsLinks.editGet showSlug postId

blogDeleteUrl :: Slug -> ShowBlogPosts.Id -> Links.URI
blogDeleteUrl showSlug postId = Links.linkURI $ dashboardBlogsLinks.delete showSlug postId

--------------------------------------------------------------------------------

-- | Dashboard blog post detail template
template :: UserMetadata.Model -> Shows.Model -> ShowBlogPosts.Model -> [ShowBlogTags.Model] -> Lucid.Html ()
template _userMeta showModel blogPost tags = do
  -- Back button and header
  Lucid.div_ [class_ $ base [Tokens.mb6]] $ do
    let backUrl = dashboardBlogsGetUrl showModel
    Lucid.a_
      [ Lucid.href_ [i|/#{backUrl}|],
        hxGet_ [i|/#{backUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base [Tokens.textGray600, "hover:text-gray-800 dark:hover:text-white", Tokens.textSm, "inline-flex", "items-center", Tokens.gap2]
      ]
      $ do
        Lucid.i_ [Lucid.class_ "fa-solid fa-arrow-left"] mempty
        "Back to Blog Posts"

  -- Main blog post container
  Lucid.div_ [class_ $ base [Tokens.bgWhite, "rounded"]] $ do
    -- Blog post header
    Lucid.div_ [class_ $ base ["border-b", Tokens.borderGray600, Tokens.p6]] $ do
      Lucid.div_ [class_ $ base ["flex", "items-start", "justify-between", Tokens.mb4]] $ do
        Lucid.div_ $ do
          Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] $ Lucid.toHtml blogPost.title

          -- Status badge
          Lucid.div_ [class_ $ base [Tokens.mb4]] $
            renderStatusBadge blogPost.status

        -- Edit and Delete buttons
        Lucid.div_ [class_ $ base ["flex", Tokens.gap2]] $ do
          let editUrl = blogEditGetUrl showModel.slug blogPost.id
          Lucid.a_
            [ Lucid.href_ [i|/#{editUrl}|],
              hxGet_ [i|/#{editUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              class_ $ base [Tokens.bgWhite, Tokens.textGray800, Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, "hover:bg-gray-700"]
            ]
            "Edit Post"
          -- Delete button
          let deleteUrl = blogDeleteUrl showModel.slug blogPost.id
              backUrl = dashboardBlogsGetUrl showModel
          Lucid.button_
            [ Lucid.type_ "button",
              hxDelete_ [i|/#{deleteUrl}|],
              hxTarget_ "#main-content",
              hxSwap_ "innerHTML",
              hxConfirm_ "Are you sure you want to delete this blog post?",
              hxOnAfterRequest_ [i|if(event.detail.successful) window.location.href='/#{backUrl}'|],
              class_ $ base ["bg-red-600", Tokens.textGray800, Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, "hover:bg-red-700"]
            ]
            "Delete"

      -- Blog post metadata grid
      Lucid.div_ [class_ $ base ["grid", "grid-cols-2", Tokens.gap4, Tokens.textSm, Tokens.mb4]] $ do
        -- Published date
        case blogPost.publishedAt of
          Just publishedAt -> do
            Lucid.div_ $ do
              Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray600]] "Published: "
              let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y at %I:%M %p" publishedAt
              Lucid.toHtml dateStr
          Nothing ->
            Lucid.div_ $ do
              Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray600]] "Published: "
              Lucid.span_ [class_ $ base ["text-gray-500 dark:text-gray-400", "italic"]] "Not published"

        -- Created date
        Lucid.div_ $ do
          Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray600]] "Created: "
          let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" blogPost.createdAt
          Lucid.toHtml dateStr

        -- Updated date
        Lucid.div_ $ do
          Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray600]] "Last Updated: "
          let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y at %I:%M %p" blogPost.updatedAt
          Lucid.toHtml dateStr

      -- Tags
      unless (null tags) $ do
        Lucid.div_ [class_ $ base ["flex", "flex-wrap", Tokens.gap2, "mt-4"]] $ do
          mapM_ renderTag tags

    -- Excerpt section (if present)
    case blogPost.excerpt of
      Just excerpt -> do
        Lucid.div_ [class_ $ base ["border-b", Tokens.borderGray600, Tokens.p6, Tokens.bgGray100]] $ do
          Lucid.h2_ [class_ $ base [Tokens.textSm, Tokens.fontBold, "uppercase", Tokens.textGray600, Tokens.mb2]] "Excerpt"
          Lucid.p_ [class_ $ base [Tokens.textGray600, "italic"]] $ Lucid.toHtml excerpt
      Nothing -> mempty

    -- Content section
    Lucid.div_ [class_ $ base [Tokens.p6]] $ do
      Lucid.h2_ [class_ $ base [Tokens.textLg, Tokens.fontBold, Tokens.mb4, "uppercase", "border-b", Tokens.borderGray600, "pb-2", Tokens.textGray800]] "Content"
      Lucid.div_ [class_ $ base ["prose", "prose-gray", "max-w-none"]] $ do
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
    Lucid.p_ [class_ $ base [Tokens.mb4, "leading-relaxed"]] $
      Lucid.toHtml para

-- | Render status badge with appropriate styling
renderStatusBadge :: BlogPostStatus -> Lucid.Html ()
renderStatusBadge Draft =
  Lucid.span_ [class_ $ base ["inline-block", "bg-yellow-100", "text-yellow-800", "px-2", "py-1", "rounded", "text-xs", Tokens.fontBold]] "DRAFT"
renderStatusBadge Published =
  Lucid.span_ [class_ $ base ["inline-block", "bg-green-100", "text-green-800", "px-2", "py-1", "rounded", "text-xs", Tokens.fontBold]] "PUBLISHED"
renderStatusBadge Deleted =
  Lucid.span_ [class_ $ base ["inline-block", "bg-red-100", "text-red-800", "px-2", "py-1", "rounded", "text-xs", Tokens.fontBold]] "DELETED"
