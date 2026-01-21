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
import Component.ActionsDropdown qualified as ActionsDropdown
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
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.PostStatus (BlogPostStatus (..))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Lucid qualified
import Servant.Links qualified as Links

-- | Station blog posts list template
template ::
  [BlogPosts.Model] ->
  Int64 ->
  Bool ->
  Lucid.Html ()
template posts currentPage hasMore = do
  -- Blog posts table or empty state
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, "overflow-hidden", Tokens.mb8]] $
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
              Nothing -> Lucid.span_ [class_ $ base ["text-gray-400 dark:text-gray-500", Tokens.textSm]] "—"

          Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
            ActionsDropdown.render
              [ ActionsDropdown.navigateAction "edit" "Edit" [i|/#{editUrl}|],
                ActionsDropdown.navigateAction "view" "View" [i|/#{viewUrl}|],
                ActionsDropdown.htmxDeleteAction
                  "delete"
                  "Delete"
                  [i|/#{deleteUrl}|]
                  "#main-content"
                  ActionsDropdown.SwapInnerHTML
                  deleteConfirmMessage
              ]

renderStatusBadge :: BlogPostStatus -> Lucid.Html ()
renderStatusBadge status = do
  let (bgClass, textClass, statusText) = case status of
        Published -> ("bg-green-100", "text-green-800", "Published") :: (Text, Text, Text)
        Draft -> ("bg-yellow-100", "text-yellow-800", "Draft")
        Deleted -> ("bg-gray-100 dark:bg-gray-700", "text-gray-800 dark:text-gray-200", "Deleted")

  Lucid.span_
    [Lucid.class_ [i|inline-block px-3 py-1 text-sm font-bold rounded #{bgClass} #{textClass}|]]
    $ Lucid.toHtml statusText

renderEmptyState :: Lucid.Html ()
renderEmptyState = do
  Lucid.div_ [class_ $ base ["bg-gray-50 dark:bg-gray-700", Tokens.border2, "border-gray-300 dark:border-gray-600", "p-12", "text-center"]] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Tokens.textGray600]] "No blog posts found."
    Lucid.p_ [class_ $ base ["text-gray-500 dark:text-gray-400", "mt-2"]] "Create a new post to get started."

formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y"
