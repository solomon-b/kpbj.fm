{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showGetLink, showsGetLink)
import App.Auth qualified as Auth
import Component.Frame (UserInfo (..), loadContentOnly, loadFrame, loadFrameWithUser)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Show qualified as Show
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Show qualified as Show
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
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
showsGetUrl = Links.linkURI $ showsGetLink Nothing Nothing Nothing

showGetUrl :: Text -> Links.URI
showGetUrl slug = Links.linkURI $ showGetLink slug

showsGetPageUrl :: Int64 -> Links.URI
showsGetPageUrl page = Links.linkURI $ showsGetLink (Just page) Nothing Nothing

showsGetGenreUrl :: Text -> Links.URI
showsGetGenreUrl genre = Links.linkURI $ showsGetLink Nothing (Just genre) Nothing

showsGetStatusUrl :: Text -> Links.URI
showsGetStatusUrl status = Links.linkURI $ showsGetLink Nothing Nothing (Just status)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows"
    ( "shows"
        :> Servant.QueryParam "page" Int64
        :> Servant.QueryParam "genre" Text
        :> Servant.QueryParam "status" Text
        :> Servant.Header "Cookie" Text
        :> Servant.Header "HX-Request" Text
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Render a show card for the list view
renderShowCard :: Show.ShowModel -> Lucid.Html ()
renderShowCard show = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    -- Show Image
    Lucid.div_ [Lucid.class_ "text-center mb-4"] $ do
      Lucid.div_ [Lucid.class_ "w-full aspect-square bg-gray-300 border-2 border-gray-600 flex items-center justify-center mb-4 text-lg"] $
        case Show.smLogoUrl show of
          Just logoUrl -> Lucid.img_ [Lucid.src_ logoUrl, Lucid.alt_ (Show.smTitle show), Lucid.class_ "w-full h-full object-cover"]
          Nothing -> "[SHOW IMG]"

      -- Show Title and Basic Info
      Lucid.h3_ [Lucid.class_ "text-xl font-bold mb-2"]
        $ Lucid.a_
          [ Lucid.href_ [i|/#{showGetUrl (Show.smSlug show)}|],
            hxGet_ [i|/#{showGetUrl (Show.smSlug show)}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "hover:underline"
          ]
        $ Lucid.toHtml (Show.smTitle show)

      Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-3"] $ do
        -- Schedule info would come from show_schedules table
        "Schedule info here" -- TODO: Join with schedule data

      -- Genre tag
      case Show.smGenre show of
        Just genre ->
          Lucid.div_ [Lucid.class_ "text-xs bg-gray-200 text-gray-800 px-2 py-1 font-mono mb-3"] $
            "#" <> Lucid.toHtml genre
        Nothing -> mempty

    -- Description
    Lucid.p_ [Lucid.class_ "text-sm leading-relaxed mb-4"] $ do
      let truncatedDesc = Text.take 150 (Show.smDescription show)
      Lucid.toHtml $ truncatedDesc <> if Text.length (Show.smDescription show) > 150 then "..." else ""

    -- Actions
    Lucid.div_ [Lucid.class_ "flex gap-2"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showGetUrl (Show.smSlug show)}|],
          hxGet_ [i|/#{showGetUrl (Show.smSlug show)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700 flex-grow text-center"
        ]
        "VIEW"
      Lucid.button_ [Lucid.class_ "border border-gray-800 px-3 py-2 text-sm hover:bg-gray-100"] "♡"

-- | Render pagination controls
renderPagination :: Int64 -> Bool -> Lucid.Html ()
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
          show currentPage

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
renderFilters :: Maybe Text -> Maybe Text -> Lucid.Html ()
renderFilters maybeGenre maybeStatus = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8"] $ do
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-4 gap-4"] $ do
      -- Genre filter
      Lucid.div_ $ do
        Lucid.label_ [Lucid.class_ "block text-sm font-bold mb-2"] "GENRE"
        Lucid.select_ [Lucid.class_ "w-full border-2 border-gray-600 px-3 py-2 bg-white font-mono"] $ do
          Lucid.option_ [Lucid.value_ ""] "All Genres"
          Lucid.option_ [Lucid.value_ "ambient"] "Ambient"
          Lucid.option_ [Lucid.value_ "electronic"] "Electronic"
          Lucid.option_ [Lucid.value_ "punk"] "Punk"
          Lucid.option_ [Lucid.value_ "jazz"] "Jazz"
          Lucid.option_ [Lucid.value_ "hip-hop"] "Hip Hop"
          Lucid.option_ [Lucid.value_ "rock"] "Rock"

      -- Time slot filter (placeholder for future schedule integration)
      Lucid.div_ $ do
        Lucid.label_ [Lucid.class_ "block text-sm font-bold mb-2"] "TIME SLOT"
        Lucid.select_ [Lucid.class_ "w-full border-2 border-gray-600 px-3 py-2 bg-white font-mono"] $ do
          Lucid.option_ "Any Time"
          Lucid.option_ "Morning (6AM-12PM)"
          Lucid.option_ "Afternoon (12PM-6PM)"
          Lucid.option_ "Evening (6PM-11PM)"
          Lucid.option_ "Late Night (11PM-6AM)"

      -- Status filter
      Lucid.div_ $ do
        Lucid.label_ [Lucid.class_ "block text-sm font-bold mb-2"] "STATUS"
        Lucid.select_ [Lucid.class_ "w-full border-2 border-gray-600 px-3 py-2 bg-white font-mono"] $ do
          Lucid.option_ [Lucid.value_ ""] "All Shows"
          Lucid.option_ [Lucid.value_ "active"] "Active"
          Lucid.option_ [Lucid.value_ "hiatus"] "On Hiatus"

      -- Filter button
      Lucid.div_ [Lucid.class_ "flex items-end"] $ do
        Lucid.button_ [Lucid.class_ "w-full bg-gray-800 text-white py-2 px-4 font-bold hover:bg-gray-700"] "FILTER"

-- | Main shows template
template :: [Show.ShowModel] -> Int64 -> Bool -> Maybe Text -> Maybe Text -> Lucid.Html ()
template shows currentPage hasMore maybeGenre maybeStatus = do
  -- Shows Header
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 text-center w-full"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4"] "KPBJ SHOWS"
    Lucid.p_ [Lucid.class_ "text-lg text-gray-600 mb-6"] "Discover our diverse lineup of community radio shows"

    Lucid.pre_ [Lucid.class_ "text-xs leading-tight text-gray-800 mb-6"] $
      "╔══════════════════════════════════════════════════════════════════════════════╗\n"
        <> "║                          BROADCASTING 24/7                                  ║\n"
        <> "║                       95.9 FM • SHADOW HILLS                               ║\n"
        <> "║                   FREEFORM COMMUNITY RADIO                                  ║\n"
        <> "╚══════════════════════════════════════════════════════════════════════════════╝"

  -- Show Filters
  renderFilters maybeGenre maybeStatus

  -- Shows Grid
  if null shows
    then Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "No Shows Found"
      Lucid.p_ [Lucid.class_ "text-gray-600"] "Check back soon for new shows!"
    else do
      Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6"] $ do
        mapM_ renderShowCard shows

      -- Pagination
      unless (null shows) $
        renderPagination currentPage hasMore

-- | Render template with proper HTMX handling
renderTemplate :: (Log.MonadLog m, MonadCatch m) => Bool -> Maybe UserInfo -> Lucid.Html () -> m (Lucid.Html ())
renderTemplate isHtmxRequest mUserInfo templateContent =
  case mUserInfo of
    Just userInfo ->
      if isHtmxRequest
        then loadContentOnly templateContent
        else loadFrameWithUser userInfo templateContent
    Nothing ->
      if isHtmxRequest
        then loadContentOnly templateContent
        else loadFrame templateContent

checkHtmxRequest :: Maybe Text -> Bool
checkHtmxRequest = \case
  Just "true" -> True
  _ -> False

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
  Maybe Int64 ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  m (Lucid.Html ())
handler _tracer maybePage maybeGenre maybeStatus cookie hxRequest = do
  let page = fromMaybe 1 maybePage
      limit = 12
      offset = (page - 1) * limit
      isHtmxRequest = checkHtmxRequest hxRequest

  -- Get user info once upfront
  loginState <- Auth.userLoginState cookie
  mUserInfo <- case loginState of
    Auth.IsNotLoggedIn -> pure Nothing
    Auth.IsLoggedIn user -> do
      execQuerySpan (UserMetadata.getUserMetadata (User.mId user)) >>= \case
        Right (Just userMetadata) ->
          pure $ Just $ UserInfo {userDisplayName = UserMetadata.mDisplayName userMetadata}
        _ -> pure Nothing

  -- Get shows based on filters
  showsResult <- case (maybeGenre, maybeStatus) of
    (Just genre, Nothing) ->
      execQuerySpan (Show.getShowsByGenre genre limit offset)
    (Nothing, Just status) ->
      case status of
        "active" -> execQuerySpan (Show.getActiveShows)
        "hiatus" -> execQuerySpan (Show.getShowsByStatus "hiatus" limit offset)
        _ -> execQuerySpan (Show.getAllShows limit offset)
    (Just genre, Just status) ->
      execQuerySpan (Show.getShowsByGenreAndStatus genre status limit offset)
    (Nothing, Nothing) ->
      execQuerySpan (Show.getAllShows limit offset)

  case showsResult of
    Left _err -> do
      Log.logInfo "Failed to fetch shows from database" ()
      renderTemplate isHtmxRequest mUserInfo (errorTemplate "Failed to load shows. Please try again.")
    Right allShows -> do
      let shows = take (fromIntegral limit) allShows
          hasMore = length allShows > fromIntegral limit
          showsTemplate = template shows page hasMore maybeGenre maybeStatus
      renderTemplate isHtmxRequest mUserInfo showsTemplate
