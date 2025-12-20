module API.Shows.Get.Templates.Page
  ( template,
    renderFilters,
  )
where

import API.Shows.Get.Templates.Pagination (renderPagination)
import Component.Card.Show (renderShowCard)
import Component.PageHeader (pageHeader)
import Control.Monad (unless)
import Data.Maybe (isNothing)
import Data.Text.Display (display)
import Design (base, class_, desktop, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.Genre (Genre)
import Domain.Types.PageNumber (PageNumber)
import Domain.Types.Search (Search)
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (xData_, xOnClick_, xShow_, xTransitionEnterEnd_, xTransitionEnterStart_, xTransitionEnter_, xTransitionLeaveEnd_, xTransitionLeaveStart_, xTransitionLeave_)

-- | Main shows list template
template :: [Shows.Model] -> PageNumber -> Bool -> Maybe Genre -> Maybe Shows.Status -> Maybe Search -> Lucid.Html ()
template allShows currentPage hasMore maybeGenre maybeStatus maybeSearch = do
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
          class_ $ do base ["absolute", "left-1/2", "-translate-x-1/2", "w-screen", "top-full", "z-20", Tokens.bgWhite, "shadow-lg"]; desktop ["hidden"]
        ]
        $ do
          renderFilters maybeGenre maybeStatus maybeSearch

    -- Desktop: Always-visible filters (hidden on mobile)
    Lucid.div_ [class_ $ do { base ["hidden", Tokens.mb6]; desktop ["block"] }] $ do
      renderFilters maybeGenre maybeStatus maybeSearch

    -- Shows Grid
    if null allShows
      then Lucid.div_ [class_ $ base [Tokens.p8, "text-center"]] $ do
        Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "No Shows Found"
        Lucid.p_ [Lucid.class_ Tokens.textGray600] "Check back soon for new shows!"
      else do
        -- Single column on mobile, expand on larger screens
        Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap6, Tokens.mb8]; tablet ["grid-cols-2"]; desktop ["grid-cols-3"] }] $ do
          mapM_ renderShowCard allShows

        -- Pagination
        unless (null allShows) $
          renderPagination currentPage hasMore

-- | Render show filters (displayed in collapsible panel)
renderFilters :: Maybe Genre -> Maybe Shows.Status -> Maybe Search -> Lucid.Html ()
renderFilters maybeGenre maybeStatus maybeSearch = do
  Lucid.div_ [class_ $ base [Tokens.p4]] $ do
    -- Stacked on mobile, horizontal row on desktop
    Lucid.form_ [Lucid.id_ "show-filters", class_ $ do { base ["space-y-4"]; desktop ["space-y-0", "flex", "items-end", Tokens.gap4] }] $ do
      -- Search bar
      Lucid.div_ [class_ $ do { base []; desktop ["flex-1"] }] $ do
        Lucid.label_ [class_ $ base ["block", Tokens.textSm, Tokens.fontBold, Tokens.mb2], Lucid.for_ "search"] "Search"
        Lucid.input_
          [ Lucid.type_ "text",
            Lucid.id_ "search",
            Lucid.name_ "search",
            class_ $ base [Tokens.fullWidth, "border", "border-gray-400", "px-3", Tokens.py2],
            Lucid.placeholder_ "Search shows...",
            Lucid.value_ (maybe "" display maybeSearch)
          ]

      -- Genre filter
      Lucid.div_ [class_ $ do { base []; desktop ["flex-1"] }] $ do
        Lucid.label_ [class_ $ base ["block", Tokens.textSm, Tokens.fontBold, Tokens.mb2], Lucid.for_ "genre"] "Genre"
        Lucid.select_
          [ Lucid.id_ "genre",
            Lucid.name_ "genre",
            class_ $ base [Tokens.fullWidth, "border", "border-gray-400", "px-3", Tokens.py2, Tokens.bgWhite]
          ]
          $ do
            Lucid.option_ ([Lucid.value_ ""] <> [Lucid.selected_ "selected" | isNothing maybeGenre]) "All Genres"
            Lucid.option_ ([Lucid.value_ "ambient"] <> [Lucid.selected_ "selected" | maybeGenre == Just "ambient"]) "Ambient"
            Lucid.option_ ([Lucid.value_ "electronic"] <> [Lucid.selected_ "selected" | maybeGenre == Just "electronic"]) "Electronic"
            Lucid.option_ ([Lucid.value_ "punk"] <> [Lucid.selected_ "selected" | maybeGenre == Just "punk"]) "Punk"
            Lucid.option_ ([Lucid.value_ "jazz"] <> [Lucid.selected_ "selected" | maybeGenre == Just "jazz"]) "Jazz"
            Lucid.option_ ([Lucid.value_ "hip-hop"] <> [Lucid.selected_ "selected" | maybeGenre == Just "hip-hop"]) "Hip Hop"
            Lucid.option_ ([Lucid.value_ "rock"] <> [Lucid.selected_ "selected" | maybeGenre == Just "rock"]) "Rock"

      -- Status filter
      Lucid.div_ [class_ $ do { base []; desktop ["flex-1"] }] $ do
        Lucid.label_ [class_ $ base ["block", Tokens.textSm, Tokens.fontBold, Tokens.mb2], Lucid.for_ "status"] "Status"
        Lucid.select_
          [ Lucid.id_ "status",
            Lucid.name_ "status",
            class_ $ base [Tokens.fullWidth, "border", "border-gray-400", "px-3", Tokens.py2, Tokens.bgWhite]
          ]
          $ do
            Lucid.option_ ([Lucid.value_ ""] <> [Lucid.selected_ "selected" | isNothing maybeStatus]) "All Shows"
            Lucid.option_ ([Lucid.value_ "active"] <> [Lucid.selected_ "selected" | maybeStatus == Just Shows.Active]) "Active"
            Lucid.option_ ([Lucid.value_ "hiatus"] <> [Lucid.selected_ "selected" | maybeStatus == Just Shows.Inactive]) "Inactive"

      -- Action buttons
      Lucid.div_ [class_ $ base ["flex", Tokens.gap2]] $ do
        Lucid.button_
          [ Lucid.type_ "submit",
            class_ $ do base ["flex-1", Tokens.bgGray800, Tokens.textWhite, Tokens.py2, Tokens.fontBold]; desktop ["flex-none", Tokens.px4]
          ]
          "Apply"
        Lucid.button_
          [ Lucid.type_ "button",
            Lucid.onclick_ "clearFilters()",
            class_ $ do base ["flex-1", "border", "border-gray-400", Tokens.py2]; desktop ["flex-none", Tokens.px4]
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
        <> "  htmx.ajax('GET', '/shows', {target: '#main-content', swap: 'innerHTML', pushUrl: '/shows'});\n"
        <> "}\n"
