module API.Archive.Get.Templates.SearchFilters (renderSearchFilters) where

--------------------------------------------------------------------------------

import Control.Monad (when)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Lucid qualified
import Lucid.Base (makeAttributes)
import Lucid.Extras (hxGet_, hxTarget_)

--------------------------------------------------------------------------------

renderSearchFilters ::
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Int64 ->
  Lucid.Html ()
renderSearchFilters mSearch mGenre mYear totalCount = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8"] $ do
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-12 gap-4"] $ do
      -- Search Bar
      Lucid.div_ [Lucid.class_ "lg:col-span-6"] $ do
        Lucid.label_ [Lucid.class_ "block text-sm font-bold mb-2"] "SEARCH EPISODES"
        Lucid.form_
          [ Lucid.class_ "flex",
            hxGet_ "/archive",
            hxTarget_ "#archive-content"
          ]
          $ do
            Lucid.input_
              [ Lucid.type_ "text",
                Lucid.name_ "q",
                Lucid.placeholder_ "Search episodes, shows, hosts...",
                Lucid.class_ "flex-grow border-2 border-gray-600 px-3 py-2 bg-white font-mono",
                Lucid.value_ (fromMaybe "" mSearch)
              ]
            -- Hidden inputs to preserve other filters
            when (isJust mGenre) $ do
              Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "genre", Lucid.value_ (fromMaybe "" mGenre)]
            when (isJust mYear) $ do
              Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "year", Lucid.value_ (Text.pack $ show $ fromMaybe 0 mYear)]
            Lucid.button_
              [ Lucid.type_ "submit",
                Lucid.class_ "bg-gray-800 text-white px-6 py-2 font-bold hover:bg-gray-700"
              ]
              "SEARCH"

      -- Genre Filter
      Lucid.div_ [Lucid.class_ "lg:col-span-3"] $ do
        Lucid.label_ [Lucid.class_ "block text-sm font-bold mb-2"] "GENRE"
        Lucid.form_
          [ hxGet_ "/archive",
            hxTarget_ "#archive-content",
            Lucid.class_ "w-full"
          ]
          $ do
            -- Hidden inputs to preserve other filters
            when (isJust mSearch) $ do
              Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "q", Lucid.value_ (fromMaybe "" mSearch)]
            when (isJust mYear) $ do
              Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "year", Lucid.value_ (Text.pack $ show $ fromMaybe 0 mYear)]
            Lucid.select_
              [ Lucid.name_ "genre",
                Lucid.class_ "w-full border-2 border-gray-600 px-3 py-2 bg-white font-mono text-sm",
                makeAttributes "onchange" "this.form.requestSubmit()"
              ]
              $ do
                Lucid.option_ ([Lucid.value_ ""] <> [Lucid.selected_ "selected" | isNothing mGenre]) "All Genres"
                Lucid.option_ ([Lucid.value_ "ambient"] <> [Lucid.selected_ "selected" | mGenre == Just "ambient"]) "Ambient"
                Lucid.option_ ([Lucid.value_ "electronic"] <> [Lucid.selected_ "selected" | mGenre == Just "electronic"]) "Electronic"
                Lucid.option_ ([Lucid.value_ "punk"] <> [Lucid.selected_ "selected" | mGenre == Just "punk"]) "Punk"
                Lucid.option_ ([Lucid.value_ "hip hop"] <> [Lucid.selected_ "selected" | mGenre == Just "hip hop"]) "Hip Hop"
                Lucid.option_ ([Lucid.value_ "jazz"] <> [Lucid.selected_ "selected" | mGenre == Just "jazz"]) "Jazz"
                Lucid.option_ ([Lucid.value_ "rock"] <> [Lucid.selected_ "selected" | mGenre == Just "rock"]) "Rock"
                Lucid.option_ ([Lucid.value_ "indie"] <> [Lucid.selected_ "selected" | mGenre == Just "indie"]) "Indie"
                Lucid.option_ ([Lucid.value_ "folk"] <> [Lucid.selected_ "selected" | mGenre == Just "folk"]) "Folk"

      -- Year Filter
      Lucid.div_ [Lucid.class_ "lg:col-span-3"] $ do
        Lucid.label_ [Lucid.class_ "block text-sm font-bold mb-2"] "YEAR"
        Lucid.form_
          [ hxGet_ "/archive",
            hxTarget_ "#archive-content",
            Lucid.class_ "w-full"
          ]
          $ do
            -- Hidden inputs to preserve other filters
            when (isJust mSearch) $ do
              Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "q", Lucid.value_ (fromMaybe "" mSearch)]
            when (isJust mGenre) $ do
              Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "genre", Lucid.value_ (fromMaybe "" mGenre)]
            Lucid.select_
              [ Lucid.name_ "year",
                Lucid.class_ "w-full border-2 border-gray-600 px-3 py-2 bg-white font-mono text-sm",
                makeAttributes "onchange" "this.form.requestSubmit()"
              ]
              $ do
                Lucid.option_ ([Lucid.value_ ""] <> [Lucid.selected_ "selected" | isNothing mYear]) "All Years"
                Lucid.option_ ([Lucid.value_ "2025"] <> [Lucid.selected_ "selected" | mYear == Just 2025]) "2025"
                Lucid.option_ ([Lucid.value_ "2024"] <> [Lucid.selected_ "selected" | mYear == Just 2024]) "2024"

    -- Episode count
    Lucid.div_ [Lucid.class_ "mt-6 pt-4 border-t border-gray-300 text-sm text-gray-600"] $ do
      Lucid.strong_ $ Lucid.toHtml $ show totalCount
      " episodes found"
