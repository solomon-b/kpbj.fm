{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Get.Templates.Page
  ( template,
    renderFilters,
    renderTabs,
  )
where

import {-# SOURCE #-} API (showsGetLink, showsScheduleGetLink)
import API.Shows.Get.Templates.Pagination (renderPagination)
import API.Shows.Get.Templates.ShowCard (renderShowCard)
import Control.Monad (unless)
import Data.Maybe (isNothing)
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Domain.Types.Genre (Genre)
import Domain.Types.PageNumber (PageNumber)
import Domain.Types.Search (Search)
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

-- | Main shows list template
template :: [Shows.Model] -> PageNumber -> Bool -> Maybe Genre -> Maybe Shows.Status -> Maybe Search -> Lucid.Html ()
template allShows currentPage hasMore maybeGenre maybeStatus maybeSearch = do
  -- Shows Header
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 text-center w-full"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4"] "ALL SHOWS"
    Lucid.p_ [Lucid.class_ "text-lg text-gray-600 mb-6"] "Browse KPBJ's diverse lineup of community radio shows"

  -- Content Navigation Tabs
  renderTabs

  -- Show Filters
  renderFilters maybeGenre maybeStatus maybeSearch

  -- Shows Grid
  if null allShows
    then Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "No Shows Found"
      Lucid.p_ [Lucid.class_ "text-gray-600"] "Check back soon for new shows!"
    else do
      Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6 mb-8 w-full"] $ do
        mapM_ renderShowCard allShows

      -- Pagination
      unless (null allShows) $
        renderPagination currentPage hasMore

-- | Render tabs for switching between views
renderTabs :: Lucid.Html ()
renderTabs =
  let showsScheduleUrl = Links.linkURI $ showsScheduleGetLink Nothing
      showsListUrl = Links.linkURI $ showsGetLink Nothing Nothing Nothing Nothing
   in Lucid.div_ [Lucid.class_ "mb-8 w-full border-b-2 border-gray-800"] $ do
        Lucid.nav_ [Lucid.class_ "flex gap-8"] $ do
          -- Schedule tab (inactive)
          Lucid.a_
            [ Lucid.href_ [i|/#{showsScheduleUrl}|],
              hxGet_ [i|/#{showsScheduleUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "py-3 px-4 font-bold uppercase text-gray-600 hover:text-gray-800"
            ]
            "Schedule"

          -- All Shows tab (active)
          Lucid.a_
            [ Lucid.href_ [i|/#{showsListUrl}|],
              hxGet_ [i|/#{showsListUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "py-3 px-4 font-bold uppercase border-b-2 border-gray-800 bg-white -mb-0.5"
            ]
            "All Shows"

-- | Render show filters
renderFilters :: Maybe Genre -> Maybe Shows.Status -> Maybe Search -> Lucid.Html ()
renderFilters maybeGenre maybeStatus maybeSearch = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8 w-full"] $ do
    Lucid.h2_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Filter Shows"

    Lucid.form_ [Lucid.id_ "show-filters", Lucid.class_ "space-y-4"] $ do
      -- Search bar
      Lucid.div_ [Lucid.class_ "col-span-full"] $ do
        Lucid.label_ [Lucid.class_ "block text-sm font-bold mb-2", Lucid.for_ "search"] "SEARCH SHOWS"
        Lucid.div_ [Lucid.class_ "relative"] $ do
          Lucid.input_
            [ Lucid.type_ "text",
              Lucid.id_ "search",
              Lucid.name_ "search",
              Lucid.class_ "w-full border-2 border-gray-600 px-3 py-2 pr-10 font-mono",
              Lucid.placeholder_ "Search by show title, description...",
              Lucid.value_ (maybe "" display maybeSearch)
            ]
          Lucid.button_
            [ Lucid.type_ "submit",
              Lucid.class_ "absolute right-2 top-1/2 transform -translate-y-1/2 text-gray-600 hover:text-gray-800"
            ]
            "🔍"

      Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-3 gap-4"] $ do
        -- Genre filter
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block text-sm font-bold mb-2", Lucid.for_ "genre"] "GENRE"
          Lucid.select_
            [ Lucid.id_ "genre",
              Lucid.name_ "genre",
              Lucid.class_ "w-full border-2 border-gray-600 px-3 py-2 bg-white font-mono"
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
          Lucid.label_ [Lucid.class_ "block text-sm font-bold mb-2", Lucid.for_ "status"] "STATUS"
          Lucid.select_
            [ Lucid.id_ "status",
              Lucid.name_ "status",
              Lucid.class_ "w-full border-2 border-gray-600 px-3 py-2 bg-white font-mono"
            ]
            $ do
              Lucid.option_ ([Lucid.value_ ""] <> [Lucid.selected_ "selected" | isNothing maybeStatus]) "All Shows"
              Lucid.option_ ([Lucid.value_ "active"] <> [Lucid.selected_ "selected" | maybeStatus == Just Shows.Active]) "Active"
              Lucid.option_ ([Lucid.value_ "hiatus"] <> [Lucid.selected_ "selected" | maybeStatus == Just Shows.Inactive]) "Inactive"

        -- Filter button
        Lucid.div_ [Lucid.class_ "flex items-end"] $ do
          Lucid.button_
            [ Lucid.type_ "submit",
              Lucid.class_ "w-full bg-gray-800 text-white py-2 px-4 font-bold hover:bg-gray-700"
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
    Lucid.div_ [Lucid.class_ "mt-4 text-center"] $ do
      Lucid.button_
        [ Lucid.onclick_ "clearFilters()",
          Lucid.class_ "text-sm text-gray-600 hover:text-gray-800 underline"
        ]
        "Clear All Filters"
