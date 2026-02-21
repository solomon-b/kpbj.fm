{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Get.Templates.Page
  ( ShowsViewData (..),
    template,
    renderFilters,
  )
where

import API.Links (showsLinks)
import API.Shows.Get.Templates.Pagination (renderPagination)
import API.Types
import Component.Card.Show (renderShowCard)
import Component.InfiniteScroll (renderEndOfContent, renderLoadingIndicator, renderSentinel)
import Component.PageHeader (pageHeader)
import Control.Monad (forM_, unless)
import Data.Maybe (isNothing)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_, desktop, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.Filter (Filter (..))
import Domain.Types.PageNumber (PageNumber)
import Domain.Types.Search (Search)
import Domain.Types.ShowSortBy (ShowSortBy (..))
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Alpine
import Servant.Links qualified as Links

-- | All data needed to render the shows list page.
data ShowsViewData = ShowsViewData
  { svStorageBackend :: StorageBackend,
    svShows :: [Shows.Model],
    svTags :: [ShowTags.ShowTagWithCount],
    svCurrentPage :: PageNumber,
    svHasMore :: Bool,
    svTagFilter :: Maybe ShowTags.Id,
    svStatusFilter :: Maybe Shows.Status,
    svSearchFilter :: Maybe Search,
    svSortByFilter :: Maybe ShowSortBy
  }

-- | Main shows list template.
template :: ShowsViewData -> Lucid.Html ()
template vd = do
  let allShows = vd.svShows
      allTags = vd.svTags
      currentPage = vd.svCurrentPage
      hasMore = vd.svHasMore
      maybeTagId = vd.svTagFilter
      maybeStatus = vd.svStatusFilter
      maybeSearch = vd.svSearchFilter
      maybeSortBy = vd.svSortByFilter
      backend = vd.svStorageBackend
  -- Page container with Alpine state for filter panel
  Lucid.div_ [xData_ "{ filterOpen: false }", class_ $ base [Tokens.fullWidth]] $ do
    -- Header row with filter button on mobile
    Lucid.div_ [class_ $ base ["relative", "flex", "justify-between", "items-start"]] $ do
      pageHeader "SHOWS"
      -- Mobile-only filter toggle button
      Lucid.button_
        [ xOnClick_ "filterOpen = !filterOpen",
          class_ $ do base ["flex", "items-center", Tokens.gap2, Tokens.px3, Tokens.py2]; desktop ["hidden"]
        ]
        $ do
          Lucid.span_ [Lucid.class_ "text-sm"] "≡"
          "Filter"

      -- Mobile: Collapsible Filter Panel - overlays content below
      Lucid.div_
        [ xShow_ "filterOpen",
          xTransitionEnter_ "transition ease-out duration-200 origin-top",
          xTransitionEnterStart_ "opacity-0 scale-y-0",
          xTransitionEnterEnd_ "opacity-100 scale-y-100",
          xTransitionLeave_ "transition ease-in duration-150 origin-top",
          xTransitionLeaveStart_ "opacity-100 scale-y-100",
          xTransitionLeaveEnd_ "opacity-0 scale-y-0",
          class_ $ do base ["absolute", "left-1/2", "-translate-x-1/2", "w-screen", "top-full", "z-20", Tokens.bgMain, "shadow-lg"]; desktop ["hidden"]
        ]
        $ do
          renderFilters allTags maybeTagId maybeStatus maybeSearch maybeSortBy

    -- Desktop: Always-visible filters (hidden on mobile)
    Lucid.div_ [class_ $ do { base ["hidden", Tokens.mb6]; desktop ["block"] }] $ do
      renderFilters allTags maybeTagId maybeStatus maybeSearch maybeSortBy

    -- Shows Grid
    if null allShows
      then Lucid.div_ [class_ $ base [Tokens.p8, "text-center"]] $ do
        Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "No Shows Found"
        Lucid.p_ [Lucid.class_ Tokens.fgMuted] "Check back soon for new shows!"
      else do
        -- Single column on mobile, expand on larger screens
        -- Items container with stable ID for HTMX appending
        Lucid.div_ [Lucid.id_ "shows-list", class_ $ do { base ["grid", "grid-cols-1", Tokens.gap6, Tokens.mb8]; tablet ["grid-cols-2"]; desktop ["grid-cols-3"] }] $ do
          mapM_ (renderShowCard backend) allShows

        -- Loading indicator (hidden by default, shown during HTMX requests)
        renderLoadingIndicator

        -- Sentinel for infinite scroll or end indicator
        unless (null allShows) $
          if hasMore
            then renderSentinel [i|/#{nextPageUrl}|] "#shows-list"
            else renderEndOfContent

        -- Fallback pagination for browsers without JavaScript
        Lucid.noscript_ $
          unless (null allShows) $
            renderPagination currentPage hasMore maybeTagId maybeStatus maybeSearch maybeSortBy
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ showsLinks.list (Just (vd.svCurrentPage + 1)) (fmap (Filter . Just) vd.svTagFilter) (fmap (Filter . Just) vd.svStatusFilter) (fmap (Filter . Just) vd.svSearchFilter) (fmap (Filter . Just) vd.svSortByFilter)

-- | Render show filters (displayed in collapsible panel)
renderFilters :: [ShowTags.ShowTagWithCount] -> Maybe ShowTags.Id -> Maybe Shows.Status -> Maybe Search -> Maybe ShowSortBy -> Lucid.Html ()
renderFilters allTags maybeTagId maybeStatus maybeSearch maybeSortBy = do
  Lucid.div_ [class_ $ base [Tokens.p4]] $ do
    -- Stacked on mobile, horizontal row on desktop
    Lucid.form_ [Lucid.id_ "show-filters", class_ $ do { base ["space-y-4"]; desktop ["space-y-0", "flex", "items-end", Tokens.gap4, "flex-wrap"] }] $ do
      -- Search bar
      Lucid.div_ [class_ $ do { base []; desktop ["flex-1", "min-w-[200px]"] }] $ do
        Lucid.label_ [class_ $ base ["block", Tokens.textSm, Tokens.fontBold, Tokens.mb2], Lucid.for_ "search"] "Search"
        Lucid.input_
          [ Lucid.type_ "text",
            Lucid.id_ "search",
            Lucid.name_ "search",
            class_ $ base [Tokens.fullWidth, "border", "Tokens.borderMuted", "px-3", Tokens.py2],
            Lucid.placeholder_ "Search shows...",
            Lucid.value_ (maybe "" display maybeSearch)
          ]

      -- Tag filter (dropdown from database)
      unless (null allTags) $ do
        Lucid.div_ [class_ $ do { base []; desktop ["flex-1", "min-w-[150px]"] }] $ do
          Lucid.label_ [class_ $ base ["block", Tokens.textSm, Tokens.fontBold, Tokens.mb2], Lucid.for_ "tag"] "Tag"
          Lucid.select_
            [ Lucid.id_ "tag",
              Lucid.name_ "tag",
              class_ $ base [Tokens.fullWidth, "border", "Tokens.borderMuted", "px-3", Tokens.py2, Tokens.bgMain]
            ]
            $ do
              Lucid.option_ ([Lucid.value_ ""] <> [Lucid.selected_ "selected" | isNothing maybeTagId]) "All Tags"
              forM_ allTags $ \tag -> do
                let isSelected = Just (ShowTags.stwcId tag) == maybeTagId
                    tagIdText = display (ShowTags.stwcId tag)
                    tagLabel :: Text
                    tagLabel = ShowTags.stwcName tag <> " (" <> display (ShowTags.stwcCount tag) <> ")"
                Lucid.option_
                  ([Lucid.value_ tagIdText] <> [Lucid.selected_ "selected" | isSelected])
                  $ Lucid.toHtml tagLabel

      -- Status filter
      Lucid.div_ [class_ $ do { base []; desktop ["flex-1", "min-w-[120px]"] }] $ do
        Lucid.label_ [class_ $ base ["block", Tokens.textSm, Tokens.fontBold, Tokens.mb2], Lucid.for_ "status"] "Status"
        Lucid.select_
          [ Lucid.id_ "status",
            Lucid.name_ "status",
            class_ $ base [Tokens.fullWidth, "border", "Tokens.borderMuted", "px-3", Tokens.py2, Tokens.bgMain]
          ]
          $ do
            Lucid.option_ ([Lucid.value_ ""] <> [Lucid.selected_ "selected" | isNothing maybeStatus]) "All Shows"
            Lucid.option_ ([Lucid.value_ "active"] <> [Lucid.selected_ "selected" | maybeStatus == Just Shows.Active]) "Active"
            Lucid.option_ ([Lucid.value_ "inactive"] <> [Lucid.selected_ "selected" | maybeStatus == Just Shows.Inactive]) "Inactive"

      -- Sort By filter
      Lucid.div_ [class_ $ do { base []; desktop ["flex-1", "min-w-[160px]"] }] $ do
        Lucid.label_ [class_ $ base ["block", Tokens.textSm, Tokens.fontBold, Tokens.mb2], Lucid.for_ "sortBy"] "Sort By"
        Lucid.select_
          [ Lucid.id_ "sortBy",
            Lucid.name_ "sortBy",
            class_ $ base [Tokens.fullWidth, "border", "Tokens.borderMuted", "px-3", Tokens.py2, Tokens.bgMain]
          ]
          $ do
            Lucid.option_ ([Lucid.value_ "name_az"] <> [Lucid.selected_ "selected" | maybeSortBy == Just NameAZ || isNothing maybeSortBy]) "Name (A-Z)"
            Lucid.option_ ([Lucid.value_ "name_za"] <> [Lucid.selected_ "selected" | maybeSortBy == Just NameZA]) "Name (Z-A)"
            Lucid.option_ ([Lucid.value_ "created_newest"] <> [Lucid.selected_ "selected" | maybeSortBy == Just CreatedNewest]) "Newest First"
            Lucid.option_ ([Lucid.value_ "created_oldest"] <> [Lucid.selected_ "selected" | maybeSortBy == Just CreatedOldest]) "Oldest First"

      -- Action buttons
      Lucid.div_ [class_ $ base ["flex", Tokens.gap2]] $ do
        Lucid.button_
          [ Lucid.type_ "submit",
            class_ $ do base ["flex-1", Tokens.bgInverse, Tokens.fgInverse, Tokens.py2, Tokens.fontBold]; desktop ["flex-none", Tokens.px4]
          ]
          "Apply"
        Lucid.button_
          [ Lucid.type_ "button",
            Lucid.onclick_ "clearFilters()",
            class_ $ do base ["flex-1", "border", "Tokens.borderMuted", Tokens.py2]; desktop ["flex-none", Tokens.px4]
          ]
          "Clear"

    -- JavaScript for form submission with HTMX
    Lucid.script_ $
      "document.getElementById('show-filters').addEventListener('submit', function(e) {\n"
        <> "  e.preventDefault();\n"
        <> "  const formData = new FormData(this);\n"
        <> "  const params = new URLSearchParams();\n"
        <> "  for (const [key, value] of formData.entries()) {\n"
        <> "    if (value) params.append(key, value);\n"
        <> "  }\n"
        <> "  const url = '/shows' + (params.toString() ? '?' + params.toString() : '');\n"
        <> "  htmx.ajax('GET', url, {target: '#main-content', swap: 'innerHTML', pushUrl: url});\n"
        <> "});\n"
        <> "function clearFilters() {\n"
        <> "  document.getElementById('show-filters').reset();\n"
        <> "  history.pushState({}, '', '/shows');\n"
        <> "  htmx.ajax('GET', '/shows', {target: '#main-content', swap: 'innerHTML'});\n"
        <> "}\n"
