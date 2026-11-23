{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Blogs.Get.Templates.BlogPostRow
  ( renderBlogPostTableRow,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardBlogsLinks)
import API.Types
import Component.Badge qualified as Badge
import Component.Button ((&))
import Component.Button qualified as Button
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Markdown (markdownToPlainText)
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardBlogPostGetUrl :: Slug -> ShowBlogPosts.Id -> Links.URI
dashboardBlogPostGetUrl showSlug postId = Links.linkURI $ dashboardBlogsLinks.detail showSlug postId

dashboardBlogEditGetUrl :: Slug -> ShowBlogPosts.Id -> Links.URI
dashboardBlogEditGetUrl showSlug postId = Links.linkURI $ dashboardBlogsLinks.editGet showSlug postId

dashboardBlogDeleteUrl :: Slug -> ShowBlogPosts.Id -> Links.URI
dashboardBlogDeleteUrl showSlug postId = Links.linkURI $ dashboardBlogsLinks.delete showSlug postId

--------------------------------------------------------------------------------

-- | Render individual blog post as table row
renderBlogPostTableRow :: Shows.Model -> ShowBlogPosts.Model -> Lucid.Html ()
renderBlogPostTableRow showModel post = do
  let postId = post.id
      postRowId = [i|blog-post-row-#{postId}|]
      detailUrl = dashboardBlogPostGetUrl showModel.slug post.id
      editUrl = dashboardBlogEditGetUrl showModel.slug post.id
      deleteUrl = dashboardBlogDeleteUrl showModel.slug post.id
      cellLinkAttrs =
        [ class_ $ base [Tokens.p4, "cursor-pointer"],
          hxGet_ [i|/#{detailUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true"
        ]
  Lucid.tr_ [class_ $ base ["border-b-2", Tokens.borderMuted, Tokens.hoverBg], Lucid.id_ postRowId] $ do
    -- Title
    Lucid.td_ cellLinkAttrs $ do
      Lucid.span_ [class_ $ base [Tokens.fontBold]] $
        Lucid.toHtml post.title
      -- Strip Markdown before the truncation, so the snippet is plain text.
      let snippetSource = fromMaybe post.content post.excerpt
          plainSnippet = markdownToPlainText snippetSource
      Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, "mt-1"]] $ do
        Lucid.toHtml $ Text.take 100 plainSnippet
        if Text.length plainSnippet > 100 then "..." else ""

    -- Published date
    Lucid.td_ cellLinkAttrs $
      case post.publishedAt of
        Just publishedAt -> Lucid.toHtml $ Text.pack $ formatTime defaultTimeLocale "%b %d, %Y" publishedAt
        Nothing -> Lucid.span_ [class_ $ base [Tokens.fgMuted, "italic"]] "—"

    -- Status
    Lucid.td_ cellLinkAttrs $
      renderStatusBadge post.status

    -- Actions
    Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm, "text-center"]] $
      Button.group $ do
        Button.render (Button.defaults & Button.dashboardLink [i|/#{editUrl}|]) "Edit"
        Button.render
          ( Button.defaults
              & Button.danger
              & Button.with
                [ hxDelete_ [i|/#{deleteUrl}|],
                  hxTarget_ ("#" <> postRowId),
                  hxSwap_ "outerHTML",
                  hxConfirm_ "Are you sure you want to delete this blog post?"
                ]
          )
          "Delete"

-- | Render status badge with appropriate styling
renderStatusBadge :: BlogPostStatus -> Lucid.Html ()
renderStatusBadge = \case
  Draft -> Badge.badge Badge.Compact Badge.Plain "DRAFT"
  Published -> Badge.badge Badge.Compact Badge.Success "PUBLISHED"
  Deleted -> Badge.badge Badge.Compact Badge.Error "DELETED"
