{-# LANGUAGE QuasiQuotes #-}

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
import Design.StyleBuilder.Internal (cls, lg, md)
import Design.Tokens qualified as Tokens
import Domain.Types.Genre (Genre)
import Domain.Types.PageNumber (PageNumber)
import Domain.Types.Search (Search)
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified

-- | Main shows list template
template :: [Shows.Model] -> PageNumber -> Bool -> Maybe Genre -> Maybe Shows.Status -> Maybe Search -> Lucid.Html ()
template allShows currentPage hasMore maybeGenre maybeStatus maybeSearch = do
  -- Shows Header
  Lucid.section_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, Tokens.mb8, "text-center", Tokens.fullWidth]] $ do
    Lucid.h1_ [Lucid.class_ Tokens.heading2xl] "ALL SHOWS"
    Lucid.p_ [Lucid.class_ $ cls [Tokens.textLg, Tokens.textGray600, Tokens.mb6]] "Browse KPBJ's diverse lineup of community radio shows"

  -- Show Filters
  renderFilters maybeGenre maybeStatus maybeSearch

  -- Shows Grid
  if null allShows
    then Lucid.div_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
      Lucid.h2_ [Lucid.class_ $ cls [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "No Shows Found"
      Lucid.p_ [Lucid.class_ Tokens.textGray600] "Check back soon for new shows!"
    else do
      Lucid.div_ [Lucid.class_ $ cls ["grid", "grid-cols-1", md "grid-cols-2", lg "grid-cols-3", Tokens.gap6, Tokens.mb8, Tokens.fullWidth]] $ do
        mapM_ renderShowCard allShows

      -- Pagination
      unless (null allShows) $
        renderPagination currentPage hasMore

-- | Render show filters
renderFilters :: Maybe Genre -> Maybe Shows.Status -> Maybe Search -> Lucid.Html ()
renderFilters maybeGenre maybeStatus maybeSearch = do
  Lucid.div_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.h2_ [Lucid.class_ $ cls [Tokens.textLg, Tokens.fontBold, Tokens.mb4, "uppercase", "border-b", "border-gray-800", Tokens.pb2]] "Filter Shows"

    Lucid.form_ [Lucid.id_ "show-filters", Lucid.class_ "space-y-4"] $ do
      -- Search bar
      Lucid.div_ [Lucid.class_ "col-span-full"] $ do
        Lucid.label_ [Lucid.class_ $ cls ["block", Tokens.textSm, Tokens.fontBold, Tokens.mb2], Lucid.for_ "search"] "SEARCH SHOWS"
        Lucid.div_ [Lucid.class_ "relative"] $ do
          Lucid.input_
            [ Lucid.type_ "text",
              Lucid.id_ "search",
              Lucid.name_ "search",
              Lucid.class_ $ cls [Tokens.fullWidth, Tokens.border2, "border-gray-600", "px-3", Tokens.py2, "pr-10", "font-mono"],
              Lucid.placeholder_ "Search by show title, description...",
              Lucid.value_ (maybe "" display maybeSearch)
            ]
          Lucid.button_
            [ Lucid.type_ "submit",
              Lucid.class_ $ cls ["absolute", "right-2", "top-1/2", "transform", "-translate-y-1/2", Tokens.textGray600, "hover:text-gray-800"]
            ]
            "🔍"

      Lucid.div_ [Lucid.class_ $ cls ["grid", "grid-cols-1", md "grid-cols-3", Tokens.gap4]] $ do
        -- Genre filter
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ $ cls ["block", Tokens.textSm, Tokens.fontBold, Tokens.mb2], Lucid.for_ "genre"] "GENRE"
          Lucid.select_
            [ Lucid.id_ "genre",
              Lucid.name_ "genre",
              Lucid.class_ $ cls [Tokens.fullWidth, Tokens.border2, "border-gray-600", "px-3", Tokens.py2, Tokens.bgWhite, "font-mono"]
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
          Lucid.label_ [Lucid.class_ $ cls ["block", Tokens.textSm, Tokens.fontBold, Tokens.mb2], Lucid.for_ "status"] "STATUS"
          Lucid.select_
            [ Lucid.id_ "status",
              Lucid.name_ "status",
              Lucid.class_ $ cls [Tokens.fullWidth, Tokens.border2, "border-gray-600", "px-3", Tokens.py2, Tokens.bgWhite, "font-mono"]
            ]
            $ do
              Lucid.option_ ([Lucid.value_ ""] <> [Lucid.selected_ "selected" | isNothing maybeStatus]) "All Shows"
              Lucid.option_ ([Lucid.value_ "active"] <> [Lucid.selected_ "selected" | maybeStatus == Just Shows.Active]) "Active"
              Lucid.option_ ([Lucid.value_ "hiatus"] <> [Lucid.selected_ "selected" | maybeStatus == Just Shows.Inactive]) "Inactive"

        -- Filter button
        Lucid.div_ [Lucid.class_ $ cls ["flex", "items-end"]] $ do
          Lucid.button_
            [ Lucid.type_ "submit",
              Lucid.class_ $ cls [Tokens.fullWidth, Tokens.bgGray800, Tokens.textWhite, Tokens.py2, Tokens.px4, Tokens.fontBold, "hover:bg-gray-700"]
            ]
            "FILTER"

    -- TODO: Replace with AlpineJS:
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
        <> "// Clear filters\n"
        <> "function clearFilters() {\n"
        <> "  document.getElementById('show-filters').reset();\n"
        <> "  htmx.ajax('GET', '/shows', {target: '#main-content', swap: 'innerHTML', pushUrl: '/shows'});\n"
        <> "}\n"

    -- Clear filters link
    Lucid.div_ [Lucid.class_ $ cls ["mt-4", "text-center"]] $ do
      Lucid.button_
        [ Lucid.onclick_ "clearFilters()",
          Lucid.class_ $ cls [Tokens.textSm, Tokens.textGray600, "hover:text-gray-800", "underline"]
        ]
        "Clear All Filters"
