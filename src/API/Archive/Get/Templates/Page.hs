module API.Archive.Get.Templates.Page (template, episodeCardsOnly) where

--------------------------------------------------------------------------------

import API.Archive.Get.Templates.EpisodeCard (renderEpisodeCard)
import API.Archive.Get.Templates.SearchFilters (renderSearchFilters)
import Control.Monad (when)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Tables.Episodes qualified as Episodes
import Lucid qualified
import Lucid.Extras (hxGet_, hxSwapOob_, hxSwap_, hxTarget_)

--------------------------------------------------------------------------------

template ::
  [Episodes.EpisodeWithShow] ->
  Int64 ->
  Bool ->
  Int64 ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Lucid.Html ()
template episodes page hasMore totalCount mSearch mGenre mYear = do
  Lucid.div_ [Lucid.id_ "archive-content", Lucid.class_ "w-full"] $ do
    pageHeader
    renderSearchFilters mSearch mGenre mYear totalCount
    episodeGrid episodes

    -- Load More Button
    Lucid.div_ [Lucid.id_ "load-more-section"] $ do
      when hasMore $ do
        Lucid.div_ [Lucid.class_ "text-center mt-8"] $ do
          let loadMoreUrl = "/archive?page=" <> Text.pack (show (page + 1)) <> buildFilterParams mSearch mGenre mYear
          Lucid.button_
            [ Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700",
              hxGet_ loadMoreUrl,
              hxTarget_ "#episodes-grid",
              hxSwap_ "beforeend"
            ]
            "LOAD MORE EPISODES"
          Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mt-3"] $ do
            "Showing "
            Lucid.toHtml $ show $ min (page * 12) totalCount
            " of "
            Lucid.toHtml $ show totalCount
            " episodes"

pageHeader :: Lucid.Html ()
pageHeader =
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 text-center"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4"] "EPISODE ARCHIVE"
    Lucid.p_ [Lucid.class_ "text-lg text-gray-600 mb-6"] "Browse and discover episodes from all KPBJ shows"

episodeGrid ::
  [Episodes.EpisodeWithShow] ->
  Lucid.Html ()
episodeGrid episodes =
  Lucid.section_ [Lucid.class_ "space-y-6"] $ do
    if null episodes
      then do
        Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-2"] "No episodes found"
          Lucid.p_ [Lucid.class_ "text-gray-600"] "Try adjusting your filters or search terms."
      else do
        Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6", Lucid.id_ "episodes-grid"] $ do
          mapM_ renderEpisodeCard episodes

buildFilterParams :: Maybe Text -> Maybe Text -> Maybe Int -> Text
buildFilterParams mSearch mGenre mYear =
  let params =
        [ case mSearch of
            Just q -> "&q=" <> q
            Nothing -> "",
          case mGenre of
            Just g -> "&genre=" <> g
            Nothing -> "",
          case mYear of
            Just y -> "&year=" <> Text.pack (show y)
            Nothing -> ""
        ]
   in Text.concat params

-- | Render only episode cards plus updated load more button (for pagination)
episodeCardsOnly ::
  [Episodes.EpisodeWithShow] ->
  Int64 ->
  Bool ->
  Int64 ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Lucid.Html ()
episodeCardsOnly episodes page hasMore totalCount mSearch mGenre mYear = do
  -- Episode cards go directly into #episodes-grid (no wrapper)
  mapM_ renderEpisodeCard episodes

  -- Load more section replaces itself via hx-swap-oob
  Lucid.div_ [Lucid.id_ "load-more-section", hxSwapOob_ "true"] $ do
    when hasMore $ do
      Lucid.div_ [Lucid.class_ "text-center mt-8"] $ do
        let loadMoreUrl = "/archive?page=" <> Text.pack (show (page + 1)) <> buildFilterParams mSearch mGenre mYear
        Lucid.button_
          [ Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700",
            hxGet_ loadMoreUrl,
            hxTarget_ "#episodes-grid",
            hxSwap_ "beforeend"
          ]
          "LOAD MORE EPISODES"
        Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mt-3"] $ do
          "Showing "
          Lucid.toHtml $ show $ min (page * 12) totalCount
          " of "
          Lucid.toHtml $ show totalCount
          " episodes"
