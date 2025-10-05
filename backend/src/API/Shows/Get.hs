{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module API.Shows.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showGetLink, showsGetLink)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isNothing)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Genre (Genre (..))
import Domain.Types.HxRequest (HxRequest (..))
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.Search (Search (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Show qualified as Show
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- URL helpers
showsGetUrl :: Links.URI
showsGetUrl = Links.linkURI $ showsGetLink Nothing Nothing Nothing Nothing

showGetUrl :: Text -> Links.URI
showGetUrl slug = Links.linkURI $ showGetLink slug

showsGetPageUrl :: PageNumber -> Links.URI
showsGetPageUrl page = Links.linkURI $ showsGetLink (Just page) Nothing Nothing Nothing

showsGetGenreUrl :: Genre -> Links.URI
showsGetGenreUrl genre = Links.linkURI $ showsGetLink Nothing (Just genre) Nothing Nothing

showsGetStatusUrl :: Show.ShowStatus -> Links.URI
showsGetStatusUrl status = Links.linkURI $ showsGetLink Nothing Nothing (Just status) Nothing

showsSearchUrl :: Search -> Links.URI
showsSearchUrl search = Links.linkURI $ showsGetLink Nothing Nothing Nothing (Just search)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows"
    ( "shows"
        :> Servant.QueryParam "page" PageNumber
        :> Servant.QueryParam "genre" Genre
        :> Servant.QueryParam "status" Show.ShowStatus
        :> Servant.QueryParam "search" Search
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Render a show card for the list view
renderShowCard :: Show.ShowModel -> Lucid.Html ()
renderShowCard s = do
  let showSlug = s.slug
      showTitle = s.title
      showDescription = s.description
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    -- Show Image
    Lucid.div_ [Lucid.class_ "text-center mb-4"] $ do
      Lucid.div_ [Lucid.class_ "w-full aspect-square bg-gray-300 border-2 border-gray-600 flex items-center justify-center mb-4 text-lg"] $
        case s.logoUrl of
          Just logoUrl -> Lucid.img_ [Lucid.src_ logoUrl, Lucid.alt_ showTitle, Lucid.class_ "w-full h-full object-cover"]
          Nothing -> "[SHOW IMG]"

      -- Show Title and Basic Info
      Lucid.h3_ [Lucid.class_ "text-xl font-bold mb-2"]
        $ Lucid.a_
          [ Lucid.href_ [i|/#{showGetUrl showSlug}|],
            hxGet_ [i|/#{showGetUrl showSlug}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "hover:underline"
          ]
        $ Lucid.toHtml showTitle

      Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-3"] $ do
        -- Schedule info would come from show_schedules table
        "Schedule info here" -- TODO: Join with schedule data

      -- Genre tag
      case s.genre of
        Just genre ->
          Lucid.div_ [Lucid.class_ "text-xs bg-gray-200 text-gray-800 px-2 py-1 font-mono mb-3"] $
            "#" <> Lucid.toHtml genre
        Nothing -> mempty

    -- Description
    Lucid.p_ [Lucid.class_ "text-sm leading-relaxed mb-4"] $ do
      let truncatedDesc = Text.take 150 showDescription
      Lucid.toHtml $ truncatedDesc <> if Text.length showDescription > 150 then "..." else ""

    -- Actions
    Lucid.div_ [Lucid.class_ "flex gap-2"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showGetUrl showSlug}|],
          hxGet_ [i|/#{showGetUrl showSlug}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700 flex-grow text-center"
        ]
        "VIEW"
      Lucid.button_ [Lucid.class_ "border border-gray-800 px-3 py-2 text-sm hover:bg-gray-100"] "♡"

-- | Render pagination controls
renderPagination :: PageNumber -> Bool -> Lucid.Html ()
renderPagination currentPage hasMore = do
  Lucid.div_ [Lucid.class_ "flex justify-center mt-8"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center space-x-2"] $ do
      -- Previous button
      if currentPage > 1
        then
          Lucid.a_
            [ Lucid.href_ [i|/#{showsGetPageUrl (currentPage - 1)}|],
              hxGet_ [i|/#{showsGetPageUrl (currentPage - 1)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "px-3 py-1 text-gray-800 hover:bg-gray-200"
            ]
            "‹ Previous"
        else Lucid.span_ [Lucid.class_ "px-3 py-1 text-gray-400"] "‹ Previous"

      -- Current page
      Lucid.span_ [Lucid.class_ "px-3 py-1 bg-gray-800 text-white font-bold"] $
        Lucid.toHtml $
          display currentPage

      -- Next button
      if hasMore
        then
          Lucid.a_
            [ Lucid.href_ [i|/#{showsGetPageUrl (currentPage + 1)}|],
              hxGet_ [i|/#{showsGetPageUrl (currentPage + 1)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "px-3 py-1 text-gray-800 hover:bg-gray-200"
            ]
            "Next ›"
        else Lucid.span_ [Lucid.class_ "px-3 py-1 text-gray-400"] "Next ›"

-- | Template for general error
errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Error"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] $ Lucid.toHtml errorMsg
    Lucid.a_
      [ Lucid.href_ [i|/#{showsGetUrl}|],
        hxGet_ [i|/#{showsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
      ]
      "REFRESH"

-- | Render show filters
renderFilters :: Maybe Genre -> Maybe Show.ShowStatus -> Maybe Search -> Lucid.Html ()
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
              Lucid.option_ ([Lucid.value_ "active"] <> [Lucid.selected_ "selected" | maybeStatus == Just Show.Active]) "Active"
              Lucid.option_ ([Lucid.value_ "hiatus"] <> [Lucid.selected_ "selected" | maybeStatus == Just Show.Inactive]) "Inactive"

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

-- | Main shows template
template :: [Show.ShowModel] -> PageNumber -> Bool -> Maybe Genre -> Maybe Show.ShowStatus -> Maybe Search -> Lucid.Html ()
template allShows currentPage hasMore maybeGenre maybeStatus maybeSearch = do
  -- Shows Header
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 text-center w-full"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4"] "KPBJ SHOWS"
    Lucid.p_ [Lucid.class_ "text-lg text-gray-600 mb-6"] "Discover our diverse lineup of community radio shows"

  -- Show Filters
  renderFilters maybeGenre maybeStatus maybeSearch

  -- Shows Grid
  if null allShows
    then Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "No Shows Found"
      Lucid.p_ [Lucid.class_ "text-gray-600"] "Check back soon for new shows!"
    else do
      Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6 w-full"] $ do
        mapM_ renderShowCard allShows

      -- Pagination
      unless (null allShows) $
        renderPagination currentPage hasMore

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Maybe PageNumber ->
  Maybe Genre ->
  Maybe Show.ShowStatus ->
  Maybe Search ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer (fromMaybe 1 -> page) maybeGenre maybeStatus maybeSearch (coerce -> cookie) (fromMaybe IsNotHxRequest -> htmxRequest) = do
  getUserInfo cookie $ \(fmap snd -> mUserInfo) -> do
    let limit = 12
        offset = (coerce page - 1) * limit

    -- Fetch limit + 1 to check if there are more results
    showsResult <- getShows (limit + 1) offset maybeSearch maybeGenre maybeStatus

    case showsResult of
      Left err -> do
        Log.logInfo "Failed to fetch shows from database" (Aeson.object ["error" .= show err])
        renderTemplate htmxRequest mUserInfo (errorTemplate "Failed to load shows. Please try again.")
      Right allShows -> do
        let someShows = take (fromIntegral limit) allShows
            hasMore = length allShows > fromIntegral limit
            showsTemplate = template someShows page hasMore maybeGenre maybeStatus maybeSearch
        renderTemplate htmxRequest mUserInfo showsTemplate

getShows ::
  ( MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env
  ) =>
  Int64 ->
  Int64 ->
  Maybe Search ->
  Maybe Genre ->
  Maybe Show.ShowStatus ->
  m (Either HSQL.Pool.UsageError [Show.ShowModel])
getShows limit offset maybeSearch maybeGenre maybeStatus = do
  case maybeSearch of
    Just (Search searchTerm)
      | not (Text.null $ Text.strip searchTerm) ->
          -- If search term is provided, use search function
          execQuerySpan (Show.searchShows (Search $ Text.strip searchTerm) limit offset)
    _ ->
      -- No search term, use existing filter logic
      case (maybeGenre, maybeStatus) of
        (Just genre, Nothing) ->
          execQuerySpan (Show.getShowsByGenre genre limit offset)
        (Nothing, Just status) ->
          case status of
            Show.Active ->
              execQuerySpan Show.getActiveShows
            Show.Inactive ->
              execQuerySpan (Show.getAllShows limit offset)
        (Just genre, Just status) ->
          execQuerySpan (Show.getShowsByGenreAndStatus genre status limit offset)
        (Nothing, Nothing) ->
          execQuerySpan (Show.getAllShows limit offset)
