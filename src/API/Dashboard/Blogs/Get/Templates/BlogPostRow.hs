{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Blogs.Get.Templates.BlogPostRow
  ( renderBlogPostTableRow,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardBlogsLinks)
import API.Types
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Base qualified as LucidBase
import Lucid.Extras (hxDelete_, hxGet_, hxPushUrl_, hxSwap_, hxTarget_, xData_, xOnChange_, xRef_)
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
  Lucid.tr_ [class_ $ base ["border-b-2", "border-gray-200 dark:border-gray-600", "hover:bg-gray-50 dark:hover:bg-gray-700"], Lucid.id_ postRowId] $ do
    -- Title
    Lucid.td_ cellLinkAttrs $ do
      Lucid.span_ [class_ $ base [Tokens.fontBold]] $
        Lucid.toHtml post.title
      case post.excerpt of
        Just excerpt ->
          Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.textGray600, "mt-1"]] $ do
            Lucid.toHtml $ Text.take 100 excerpt
            if Text.length excerpt > 100 then "..." else ""
        Nothing ->
          Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.textGray600, "mt-1"]] $ do
            Lucid.toHtml $ Text.take 100 post.content
            if Text.length post.content > 100 then "..." else ""

    -- Published date
    Lucid.td_ cellLinkAttrs $
      case post.publishedAt of
        Just publishedAt -> Lucid.toHtml $ Text.pack $ formatTime defaultTimeLocale "%b %d, %Y" publishedAt
        Nothing -> Lucid.span_ [class_ $ base ["text-gray-500 dark:text-gray-400", "italic"]] "—"

    -- Status
    Lucid.td_ cellLinkAttrs $
      renderStatusBadge post.status

    -- Actions dropdown
    Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
      Lucid.div_ [xData_ "{}"] $ do
        -- Hidden link for Edit
        Lucid.a_
          [ Lucid.href_ [i|/#{editUrl}|],
            hxGet_ [i|/#{editUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            xRef_ "editLink",
            Lucid.class_ "hidden"
          ]
          ""
        -- Hidden button for Delete
        Lucid.button_
          [ hxDelete_ [i|/#{deleteUrl}|],
            hxTarget_ ("#" <> postRowId),
            hxSwap_ "outerHTML",
            LucidBase.makeAttributes "hx-confirm" "Are you sure you want to delete this blog post?",
            xRef_ "deleteBtn",
            Lucid.class_ "hidden"
          ]
          ""
        -- Visible dropdown
        Lucid.select_
          [ Lucid.class_ "p-2 border border-gray-400 dark:border-gray-500 text-xs bg-white dark:bg-gray-800",
            xOnChange_
              [i|
              const action = $el.value;
              $el.value = '';
              if (action === 'edit') {
                $refs.editLink.click();
              } else if (action === 'delete') {
                $refs.deleteBtn.click();
              }
            |]
          ]
          $ do
            Lucid.option_ [Lucid.value_ ""] "Actions..."
            Lucid.option_ [Lucid.value_ "edit"] "Edit"
            Lucid.option_ [Lucid.value_ "delete"] "Delete"

-- | Render status badge with appropriate styling
renderStatusBadge :: BlogPostStatus -> Lucid.Html ()
renderStatusBadge Draft =
  Lucid.span_ [class_ $ base ["inline-block", Tokens.bgGray100, Tokens.textGray800, "px-2", "py-1", "rounded", Tokens.textXs, Tokens.fontBold]] "DRAFT"
renderStatusBadge Published =
  Lucid.span_ [class_ $ base ["inline-block", "bg-green-100", "text-green-800", "px-2", "py-1", "rounded", Tokens.textXs, Tokens.fontBold]] "PUBLISHED"
renderStatusBadge Deleted =
  Lucid.span_ [class_ $ base ["inline-block", "bg-red-100", "text-red-800", "px-2", "py-1", "rounded", Tokens.textXs, Tokens.fontBold]] "DELETED"
