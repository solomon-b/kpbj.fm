{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationBlog.Get.Templates.Page
  ( template,
    renderPostRow,
  )
where

--------------------------------------------------------------------------------

import API.Links (blogLinks, dashboardStationBlogLinks)
import API.Types
import Component.Badge qualified as Badge
import Component.Button ((&))
import Component.Button qualified as Button
import Component.Card qualified as Card
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    PaginationConfig (..),
    clickableCellAttrs,
    renderIndexTable,
    rowAttrs,
  )
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Domain.Types.PostStatus (BlogPostStatus (..))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

-- | Station blog posts list template
template ::
  [BlogPosts.Model] ->
  Int64 ->
  Bool ->
  Lucid.Html ()
template posts currentPage hasMore = do
  -- Blog posts table or empty state
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $
    if null posts
      then renderEmptyState
      else
        renderIndexTable
          IndexTableConfig
            { itcBodyId = "station-blog-table-body",
              itcHeaders =
                [ ColumnHeader "Title" AlignLeft,
                  ColumnHeader "Status" AlignLeft,
                  ColumnHeader "Created" AlignLeft,
                  ColumnHeader "Published" AlignLeft,
                  ColumnHeader "" AlignCenter
                ],
              itcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl}|] else Nothing,
              itcPaginationConfig =
                Just
                  PaginationConfig
                    { pcPrevPageUrl = if currentPage > 1 then Just [i|/#{prevPageUrl}|] else Nothing,
                      pcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl}|] else Nothing,
                      pcCurrentPage = currentPage
                    }
            }
          (mapM_ renderPostRow posts)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardStationBlogLinks.list (Just (currentPage + 1))
    prevPageUrl :: Links.URI
    prevPageUrl = Links.linkURI $ dashboardStationBlogLinks.list (Just (currentPage - 1))

renderPostRow :: BlogPosts.Model -> Lucid.Html ()
renderPostRow post =
  let postId = post.bpmId
      postSlug = post.bpmSlug
      title = post.bpmTitle
      status = post.bpmStatus
      createdAt = post.bpmCreatedAt
      publishedAt = post.bpmPublishedAt
      detailUri = Links.linkURI $ dashboardStationBlogLinks.detail postId postSlug
      detailUrl = [i|/#{detailUri}|]
      editUrl = Links.linkURI $ dashboardStationBlogLinks.editGet postId postSlug
      viewUrl = Links.linkURI $ blogLinks.postWithSlug postId postSlug
      deleteUrl = Links.linkURI $ dashboardStationBlogLinks.delete postId postSlug
      postIdText = display postId
      rowId = [i|post-row-#{postIdText}|]
      deleteConfirmMessage =
        "Are you sure you want to delete the blog post \""
          <> display title
          <> "\"? This action cannot be undone."
   in do
        Lucid.tr_ (rowAttrs rowId) $ do
          Lucid.td_ (clickableCellAttrs detailUrl) $
            Lucid.span_ [class_ $ base [Tokens.fontBold]] $
              Lucid.toHtml title

          Lucid.td_ (clickableCellAttrs detailUrl) $
            renderStatusBadge status

          Lucid.td_ (clickableCellAttrs detailUrl) $ do
            Lucid.div_ [class_ $ base [Tokens.textSm]] $ Lucid.toHtml (formatDateTime createdAt)

          Lucid.td_ (clickableCellAttrs detailUrl) $ do
            case publishedAt of
              Just pubAt -> Lucid.div_ [class_ $ base [Tokens.textSm]] $ Lucid.toHtml (formatDateTime pubAt)
              Nothing -> Lucid.span_ [class_ $ base [Tokens.fgMuted, Tokens.textSm]] "—"

          Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm, "text-center"]] $
            Button.group $ do
              Button.render (Button.defaults & Button.dashboardLink [i|/#{editUrl}|]) "Edit"
              Button.render (Button.defaults & Button.pageLink [i|/#{viewUrl}|]) "View"
              Button.render
                ( Button.defaults
                    & Button.danger
                    & Button.with
                      [ hxDelete_ [i|/#{deleteUrl}|],
                        hxTarget_ "#main-content",
                        hxSwap_ "innerHTML",
                        hxConfirm_ deleteConfirmMessage
                      ]
                )
                "Delete"

renderStatusBadge :: BlogPostStatus -> Lucid.Html ()
renderStatusBadge = \case
  Published -> Badge.badge Badge.Regular Badge.Success "Published"
  Draft -> Badge.badge Badge.Regular Badge.Warning "Draft"
  Deleted -> Badge.badge Badge.Regular Badge.Error "Deleted"

renderEmptyState :: Lucid.Html ()
renderEmptyState = do
  Card.card Card.EmptyState [] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Theme.fgMuted]] "No blog posts found."
    Lucid.p_ [class_ $ base [Theme.fgMuted, "mt-2"]] "Create a new post to get started."

formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y"
