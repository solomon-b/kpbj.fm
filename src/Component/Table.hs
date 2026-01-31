{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Component.Table
  ( -- * Basic table rendering
    renderTableWithBodyId,
    TableConfig (..),
    ColumnHeader (..),
    ColumnAlign (..),

    -- * Index table with infinite scroll
    renderIndexTable,
    IndexTableConfig (..),

    -- * Fragment for infinite scroll appends
    renderTableFragment,

    -- * Row and cell helpers
    rowAttrs,
    clickableCellAttrs,

    -- * Pagination
    PaginationConfig (..),
    renderNoscriptPagination,
  )
where

--------------------------------------------------------------------------------

import Component.InfiniteScroll (renderEndOfContent, renderLoadingIndicator, renderSentinel)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.HTMX

--------------------------------------------------------------------------------

-- | Column alignment options
data ColumnAlign = AlignLeft | AlignRight | AlignCenter
  deriving (Show, Eq)

-- | Column header with optional alignment
data ColumnHeader = ColumnHeader
  { headerText :: Text,
    headerAlign :: ColumnAlign
  }
  deriving (Show, Eq)

-- | Configuration for table rendering
data TableConfig = TableConfig
  { headers :: [ColumnHeader],
    -- | Optional CSS classes to add to the table wrapper
    wrapperClass :: Text,
    -- | Optional CSS classes to add to the table element
    tableClass :: Text
  }

-- | Render a table with a specific tbody ID for HTMX targeting.
renderTableWithBodyId ::
  -- | ID for the tbody element
  Text ->
  -- | Table configuration
  TableConfig ->
  -- | Table body content (rows)
  Lucid.Html () ->
  Lucid.Html ()
renderTableWithBodyId bodyId config bodyContent =
  Lucid.div_ [class_ $ base [config.wrapperClass, "border-l", "border-r", "border-b", Theme.borderMuted]] $
    Lucid.table_ [Lucid.class_ config.tableClass] $ do
      Lucid.thead_ [class_ $ base [Tokens.bgInverse, Tokens.fgInverse]] $
        Lucid.tr_ $
          mapM_ renderHeader config.headers
      Lucid.tbody_ [Lucid.id_ bodyId] bodyContent
  where
    renderHeader :: ColumnHeader -> Lucid.Html ()
    renderHeader colHeader =
      Lucid.th_ [class_ $ base [Tokens.p4, alignClass colHeader.headerAlign]] $
        Lucid.toHtml colHeader.headerText

    alignClass :: ColumnAlign -> Text
    alignClass AlignLeft = "text-left"
    alignClass AlignRight = "text-right"
    alignClass AlignCenter = "text-center"

--------------------------------------------------------------------------------
-- Index Table with Infinite Scroll
--------------------------------------------------------------------------------

-- | Configuration for index tables with infinite scroll support.
data IndexTableConfig = IndexTableConfig
  { -- | Unique ID for tbody (for HTMX targeting)
    itcBodyId :: Text,
    -- | Column headers with alignment
    itcHeaders :: [ColumnHeader],
    -- | URL for next page (Nothing = no more pages)
    itcNextPageUrl :: Maybe Text,
    -- | Optional pagination config for noscript fallback
    itcPaginationConfig :: Maybe PaginationConfig
  }

-- | Configuration for noscript pagination fallback.
data PaginationConfig = PaginationConfig
  { pcPrevPageUrl :: Maybe Text,
    pcNextPageUrl :: Maybe Text,
    pcCurrentPage :: Int64
  }

-- | Render a complete index table with infinite scroll and pagination.
--
-- This is the recommended way to render dashboard index tables. It handles:
--
-- * Table with headers and body
-- * Infinite scroll sentinel or end-of-content indicator
-- * Loading indicator for HTMX requests
-- * Noscript pagination fallback
--
-- Note: Empty state handling should be done at the call site before calling this function.
--
-- Example usage:
--
-- @
-- Lucid.section_ [class_ $ base [Tokens.bgMain, Tokens.cardBorder]] $
--   if null users
--     then renderEmptyState
--     else
--       renderIndexTable
--         IndexTableConfig
--           { itcBodyId = "users-table-body",
--             itcHeaders = [ColumnHeader "Name" AlignLeft, ColumnHeader "Email" AlignLeft],
--             itcNextPageUrl = if hasMore then Just "/users?page=2" else Nothing,
--             itcPaginationConfig = Just PaginationConfig {...}
--           }
--         (mapM_ renderUserRow users)
-- @
renderIndexTable ::
  -- | Table configuration
  IndexTableConfig ->
  -- | Row content (should be tr elements)
  Lucid.Html () ->
  Lucid.Html ()
renderIndexTable config rowContent = do
  renderTableWithBodyId config.itcBodyId tableConfig $ do
    rowContent
    renderSentinelRow (length config.itcHeaders) ("#" <> config.itcBodyId) config.itcNextPageUrl

  -- Loading indicator (hidden by default, shown during HTMX requests)
  renderLoadingIndicator

  -- Fallback pagination for browsers without JavaScript
  case config.itcPaginationConfig of
    Nothing -> mempty
    Just paginationConfig ->
      Lucid.noscript_ $ renderNoscriptPagination paginationConfig
  where
    tableConfig =
      TableConfig
        { headers = config.itcHeaders,
          wrapperClass = "overflow-x-auto",
          tableClass = "w-full"
        }

--------------------------------------------------------------------------------
-- Table Fragment for Infinite Scroll
--------------------------------------------------------------------------------

-- | Render table rows with sentinel for infinite scroll appends.
--
-- Used by ItemsFragment handlers to return just the new rows plus
-- the sentinel/end indicator for HTMX to append to the table body.
--
-- Example usage:
--
-- @
-- renderTableFragment 6 "#users-table-body"
--   (if hasMore then Just "/users?page=3" else Nothing)
--   (mapM_ renderUserRow users)
-- @
renderTableFragment ::
  -- | Number of columns (for colspan)
  Int ->
  -- | Target tbody ID (with # prefix)
  Text ->
  -- | URL for next page (Nothing = end of content)
  Maybe Text ->
  -- | Row content
  Lucid.Html () ->
  Lucid.Html ()
renderTableFragment columnCount targetId nextPageUrl rowContent = do
  rowContent
  renderSentinelRow columnCount targetId nextPageUrl

--------------------------------------------------------------------------------
-- Row and Cell Helpers
--------------------------------------------------------------------------------

-- | Standard row attributes with hover styling and ID.
--
-- Example usage:
--
-- @
-- Lucid.tr_ (rowAttrs "user-row-123") $ do
--   Lucid.td_ ...
-- @
rowAttrs :: Text -> [Lucid.Attributes]
rowAttrs rowId =
  [ Lucid.id_ rowId,
    class_ $ base ["border-b", Theme.borderMuted, Theme.hoverBg]
  ]

-- | Clickable cell attributes for navigation.
--
-- Adds padding, cursor, and HTMX attributes for clicking through to detail pages.
--
-- Example usage:
--
-- @
-- Lucid.td_ (clickableCellAttrs "/users/123") $
--   Lucid.toHtml hostName
-- @
clickableCellAttrs :: Text -> [Lucid.Attributes]
clickableCellAttrs url =
  [ class_ $ base [Tokens.p4, "cursor-pointer"],
    hxGet_ url,
    hxTarget_ "#main-content",
    hxPushUrl_ "true"
  ]

--------------------------------------------------------------------------------
-- Pagination
--------------------------------------------------------------------------------

-- | Render noscript pagination fallback.
--
-- Provides previous/next navigation for browsers without JavaScript.
renderNoscriptPagination :: PaginationConfig -> Lucid.Html ()
renderNoscriptPagination config = do
  Lucid.div_ [class_ $ base ["flex", "justify-between", "items-center", "mt-4"]] $ do
    -- Previous button
    case config.pcPrevPageUrl of
      Just prevUrl ->
        Lucid.a_
          [ Lucid.href_ prevUrl,
            hxGet_ prevUrl,
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base [Tokens.bgInverse, Tokens.fgInverse, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700"]
          ]
          "<- PREVIOUS"
      Nothing ->
        Lucid.div_ [] mempty

    -- Page indicator
    Lucid.span_ [class_ $ base [Tokens.fgMuted, Tokens.fontBold]] $
      Lucid.toHtml $
        "Page " <> show config.pcCurrentPage

    -- Next button
    case config.pcNextPageUrl of
      Just nextUrl ->
        Lucid.a_
          [ Lucid.href_ nextUrl,
            hxGet_ nextUrl,
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base [Tokens.bgInverse, Tokens.fgInverse, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700"]
          ]
          "NEXT ->"
      Nothing ->
        Lucid.div_ [] mempty

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Render the sentinel or end-of-content row.
renderSentinelRow :: Int -> Text -> Maybe Text -> Lucid.Html ()
renderSentinelRow columnCount targetId = \case
  Just nextUrl ->
    Lucid.tr_ [Lucid.id_ "load-more-sentinel-row"] $
      Lucid.td_ [Lucid.colspan_ [i|#{columnCount}|]] $
        renderSentinel nextUrl targetId
  Nothing ->
    Lucid.tr_ [] $
      Lucid.td_
        [Lucid.colspan_ [i|#{columnCount}|]]
        renderEndOfContent
