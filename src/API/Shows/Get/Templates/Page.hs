module API.Shows.Get.Templates.Page
  ( template,
    renderFilters,
  )
where

import API.Shows.Get.Templates.Pagination (renderPagination)
import API.Shows.Get.Templates.ShowCard (renderShowCard)
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
import Lucid.Extras (xData_, xOnClick_, xShow_)

-- | Main shows list template
template :: [Shows.Model] -> PageNumber -> Bool -> Maybe Genre -> Maybe Shows.Status -> Maybe Search -> Lucid.Html ()
template allShows currentPage hasMore maybeGenre maybeStatus maybeSearch = do
  -- Page container with Alpine state for filter panel
  Lucid.div_ [xData_ "{ filterOpen: false }", class_ $ base [Tokens.fullWidth]] $ do
    -- Header: Title + Filter button
    Lucid.div_ [class_ $ base ["flex", "justify-between", "items-center", Tokens.mb6]] $ do
      Lucid.h1_ [class_ $ base [Tokens.textXl, Tokens.fontBold]] "Shows"
      Lucid.button_
        [ xOnClick_ "filterOpen = !filterOpen",
          class_ $ base ["flex", "items-center", Tokens.gap2, "border", "border-gray-400", "rounded", Tokens.px3, Tokens.py2]
        ]
        $ do
          -- Filter icon (using Unicode character)
          Lucid.span_ [Lucid.class_ "text-sm"] "≡"
          "Filter"

    -- Collapsible Filter Panel
    Lucid.div_ [xShow_ "filterOpen", class_ $ base [Tokens.mb6]] $ do
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
  Lucid.div_ [class_ $ base ["border", "border-gray-400", Tokens.p4]] $ do
    Lucid.form_ [Lucid.id_ "show-filters", Lucid.class_ "space-y-4"] $ do
      -- Search bar
      Lucid.div_ $ do
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
      Lucid.div_ $ do
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
      Lucid.div_ $ do
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
            class_ $ base ["flex-1", Tokens.bgGray800, Tokens.textWhite, Tokens.py2, Tokens.fontBold]
          ]
          "Apply"
        Lucid.button_
          [ Lucid.type_ "button",
            Lucid.onclick_ "clearFilters()",
            class_ $ base ["flex-1", "border", "border-gray-400", Tokens.py2]
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
