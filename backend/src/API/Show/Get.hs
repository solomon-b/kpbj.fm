{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Show.Get where

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
import Data.Either (fromRight)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episode qualified as Episode
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
showGetUrl :: Text -> Links.URI
showGetUrl slug = Links.linkURI $ showGetLink slug

showsGetUrl :: Links.URI
showsGetUrl = Links.linkURI $ showsGetLink Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows/:slug"
    ( "shows"
        :> Servant.Capture "slug" Text
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Render an episode card
renderEpisodeCard :: Episode.EpisodeModel -> Lucid.Html ()
renderEpisodeCard episode = do
  Lucid.div_ [Lucid.class_ "flex gap-4 mb-6"] $ do
    -- Episode thumbnail
    Lucid.div_ [Lucid.class_ "w-24 h-24 bg-gray-300 border border-gray-600 flex items-center justify-center text-xs flex-shrink-0"] $ do
      case Episode.emArtworkUrl episode of
        Just artworkUrl -> Lucid.img_ [Lucid.src_ artworkUrl, Lucid.alt_ "Episode artwork", Lucid.class_ "w-full h-full object-cover"]
        Nothing -> "[EP IMG]"

    -- Episode info
    Lucid.div_ [Lucid.class_ "flex-grow"] $ do
      Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-2"] $ Lucid.toHtml (Episode.emTitle episode)

      Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-2"] $ do
        case Episode.emPublishedAt episode of
          Just publishedAt -> do
            let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
            "Aired: " <> Lucid.toHtml dateStr
          Nothing -> "Draft"

        case Episode.emDurationSeconds episode of
          Just duration -> " • Duration: " <> Lucid.toHtml (show (duration `div` 60)) <> "min"
          Nothing -> mempty

      -- Episode description
      case Episode.emDescription episode of
        Just desc -> do
          let truncatedDesc = Text.take 200 desc
          Lucid.p_ [Lucid.class_ "text-sm mb-4"] $
            Lucid.toHtml $
              truncatedDesc <> if Text.length desc > 200 then "..." else ""
        Nothing -> mempty

      -- Audio player (if audio file path exists)
      case Episode.emAudioFilePath episode of
        Just audioPath -> do
          Lucid.div_ [Lucid.class_ "bg-gray-100 border-2 border-gray-600 p-4 mb-4"] $ do
            Lucid.div_ [Lucid.class_ "flex items-center gap-4 mb-2"] $ do
              Lucid.button_
                [ Lucid.class_ "bg-gray-800 text-white px-6 py-2 font-bold hover:bg-gray-700",
                  Lucid.onclick_ [i|playEpisode('#{audioPath}')|]
                ]
                "▶ PLAY"
              Lucid.div_ [Lucid.class_ "flex-grow bg-gray-300 h-2 rounded"] $ do
                Lucid.div_ [Lucid.class_ "bg-gray-800 h-2 rounded w-0"] mempty
              case Episode.emDurationSeconds episode of
                Just duration ->
                  let minutes = duration `div` 60
                      seconds = duration `mod` 60
                   in Lucid.span_ [Lucid.class_ "text-sm font-mono"] $
                        "0:00 / "
                          <> Lucid.toHtml (show minutes)
                          <> ":"
                          <> (if seconds < 10 then "0" else "")
                          <> Lucid.toHtml (show seconds)
                Nothing -> mempty
        Nothing -> mempty

-- | Render show header with info
renderShowHeader :: Show.ShowModel -> [Episode.EpisodeModel] -> [Show.ShowHostWithUser] -> [Show.ShowScheduleModel] -> Lucid.Html ()
renderShowHeader showModel episodes hosts schedules = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8"] $ do
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-4 gap-8"] $ do
      -- Show Image
      Lucid.div_ [Lucid.class_ "lg:col-span-1"] $ do
        Lucid.div_ [Lucid.class_ "w-full aspect-square bg-gray-300 border-2 border-gray-600 flex items-center justify-center text-lg"] $ do
          case Show.smLogoUrl showModel of
            Just logoUrl -> Lucid.img_ [Lucid.src_ logoUrl, Lucid.alt_ (Show.smTitle showModel), Lucid.class_ "w-full h-full object-cover"]
            Nothing -> "[SHOW IMAGE]"

        -- Social/Subscribe Buttons
        Lucid.div_ [Lucid.class_ "mt-4 space-y-2"] $ do
          Lucid.button_ [Lucid.class_ "w-full bg-gray-800 text-white py-2 px-4 font-bold hover:bg-gray-700"] "♡ FOLLOW SHOW"
          Lucid.button_ [Lucid.class_ "w-full border-2 border-gray-800 bg-white text-gray-800 py-2 px-4 font-bold hover:bg-gray-100"] "🔔 NOTIFICATIONS"

      -- Show Info
      Lucid.div_ [Lucid.class_ "lg:col-span-3"] $ do
        Lucid.div_ [Lucid.class_ "mb-4"] $ do
          Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-2"] $ Lucid.toHtml (Text.toUpper $ Show.smTitle showModel)

          Lucid.div_ [Lucid.class_ "text-lg text-gray-600 mb-4"] $ do
            -- Show host information
            Lucid.span_ [Lucid.class_ "font-bold"] "Host: "
            case hosts of
              [] -> "TBD"
              (host : otherHosts) -> do
                let displayName = Show.shwuDisplayName host
                Lucid.toHtml displayName
                unless (null otherHosts) $ do
                  ", "
                  let otherNames = map (Lucid.toHtml . Show.shwuDisplayName) otherHosts
                  mconcat $ map (", " <>) otherNames
            " • "
            -- Show schedule information
            Lucid.span_ [Lucid.class_ "font-bold"] "Schedule: "
            case schedules of
              [] -> "TBD"
              (schedule : _) -> do
                let dayName = case Show.ssmDayOfWeek schedule of
                      0 -> "Sunday"
                      1 -> "Monday"
                      2 -> "Tuesday"
                      3 -> "Wednesday"
                      4 -> "Thursday"
                      5 -> "Friday"
                      6 -> "Saturday"
                      _ -> "Unknown"
                    startTime = Show.ssmStartTime schedule
                    endTime = Show.ssmEndTime schedule
                Lucid.toHtml $ dayName <> "s " <> startTime <> " - " <> endTime
            " • "
            case Show.smGenre showModel of
              Just genre -> Lucid.span_ [Lucid.class_ "font-bold"] "Genre: " <> Lucid.toHtml genre
              Nothing -> mempty

        -- Show Description
        Lucid.div_ [Lucid.class_ "mb-6"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-3 uppercase border-b border-gray-800 pb-2"] "About The Show"
          Lucid.p_ [Lucid.class_ "mb-4 leading-relaxed"] $ Lucid.toHtml (Show.smDescription showModel)

        -- Stats and Episode Count
        Lucid.div_ [Lucid.class_ "mb-6"] $ do
          Lucid.div_ [Lucid.class_ "grid grid-cols-2 md:grid-cols-4 gap-4 text-center"] $ do
            Lucid.div_ [Lucid.class_ "bg-gray-100 p-3 border border-gray-300"] $ do
              Lucid.div_ [Lucid.class_ "text-2xl font-bold"] $ Lucid.toHtml $ Prelude.show $ length episodes
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] "Episodes"

            Lucid.div_ [Lucid.class_ "bg-gray-100 p-3 border border-gray-300"] $ do
              Lucid.div_ [Lucid.class_ "text-2xl font-bold"] $ Lucid.toHtml $ Text.toUpper $ Text.pack $ Prelude.show $ Show.smStatus showModel
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] "Status"

            Lucid.div_ [Lucid.class_ "bg-gray-100 p-3 border border-gray-300"] $ do
              Lucid.div_ [Lucid.class_ "text-2xl font-bold"] $ Lucid.toHtml $ Text.toUpper $ Text.pack $ Prelude.show $ Show.smFrequency showModel
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] "Frequency"

            Lucid.div_ [Lucid.class_ "bg-gray-100 p-3 border border-gray-300"] $ do
              Lucid.div_ [Lucid.class_ "text-2xl font-bold"] $
                case Show.smDurationMinutes showModel of
                  Just duration -> Lucid.toHtml (Prelude.show duration) <> "min"
                  Nothing -> "TBD"
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] "Duration"

-- | Render breadcrumb navigation
renderBreadcrumb :: Show.ShowModel -> Lucid.Html ()
renderBreadcrumb showModel = do
  Lucid.nav_ [Lucid.class_ "bg-gray-100 px-4 py-2 border-b border-gray-300"] $ do
    Lucid.div_ [Lucid.class_ "max-w-6xl mx-auto"] $ do
      Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] $ do
        Lucid.a_ [Lucid.href_ "/", hxGet_ "/", hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "hover:text-gray-800"] "Home"
        Lucid.span_ [Lucid.class_ "mx-2"] "/"
        Lucid.a_ [Lucid.href_ [i|/#{showsGetUrl}|], hxGet_ [i|/#{showsGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "hover:text-gray-800"] "Shows"
        Lucid.span_ [Lucid.class_ "mx-2"] "/"
        Lucid.span_ [Lucid.class_ "text-gray-800 font-bold"] $ Lucid.toHtml (Show.smTitle showModel)

-- | Template for show not found
notFoundTemplate :: Text -> Lucid.Html ()
notFoundTemplate slug = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Show Not Found"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] $ "The show '" <> Lucid.toHtml slug <> "' could not be found."
    Lucid.a_
      [ Lucid.href_ [i|/#{showsGetUrl}|],
        hxGet_ [i|/#{showsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
      ]
      "BROWSE ALL SHOWS"

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
      "BROWSE ALL SHOWS"

-- | Main show page template
template :: Show.ShowModel -> [Episode.EpisodeModel] -> [Show.ShowHostWithUser] -> [Show.ShowScheduleModel] -> Lucid.Html ()
template showModel episodes hosts schedules = do
  -- Breadcrumb
  renderBreadcrumb showModel

  -- Show Header
  renderShowHeader showModel episodes hosts schedules

  -- Content Tabs Navigation
  Lucid.div_ [Lucid.class_ "mb-8"] $ do
    Lucid.div_ [Lucid.class_ "border-b-2 border-gray-800"] $ do
      Lucid.nav_ [Lucid.class_ "flex gap-8"] $ do
        Lucid.button_ [Lucid.class_ "py-3 px-4 font-bold uppercase border-b-2 border-gray-800 bg-white -mb-0.5"] "Episodes"
  -- TODO: Add blog tab when show blogs are implemented
  -- Lucid.a_ [Lucid.href_ "#", Lucid.class_ "py-3 px-4 font-bold uppercase text-gray-600 hover:text-gray-800"] "Blog"

  -- Main Content Grid
  Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-3 gap-8"] $ do
    -- Episodes Section
    Lucid.section_ [Lucid.class_ "lg:col-span-2"] $ do
      if null episodes
        then do
          Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
            Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "No Episodes Yet"
            Lucid.p_ [Lucid.class_ "text-gray-600 mb-6"] "This show hasn't published any episodes yet. Check back soon!"
        else do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-6 uppercase border-b border-gray-800 pb-2"] "Episodes"

          -- Featured/Latest Episode
          case episodes of
            (latestEpisode : otherEpisodes) -> do
              Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8"] $ do
                Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Latest Episode"
                renderEpisodeCard latestEpisode

              -- Other Episodes
              unless (null otherEpisodes) $ do
                Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
                  Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Previous Episodes"
                  mapM_ renderEpisodeCard otherEpisodes
            [] -> mempty

    -- Sidebar
    Lucid.div_ [Lucid.class_ "lg:col-span-1 space-y-6"] $ do
      -- Show Info Widget
      Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-4"] $ do
        Lucid.h3_ [Lucid.class_ "font-bold mb-4 text-center"] "SHOW INFO"
        Lucid.div_ [Lucid.class_ "space-y-2 text-sm"] $ do
          Lucid.div_ $ do
            Lucid.strong_ "Status: "
            Lucid.span_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml $ Text.toUpper $ Text.pack $ Prelude.show $ Show.smStatus showModel
          Lucid.div_ $ do
            Lucid.strong_ "Frequency: "
            Lucid.span_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml $ Text.toUpper $ Text.pack $ Prelude.show $ Show.smFrequency showModel
          case Show.smGenre showModel of
            Just genre -> Lucid.div_ $ do
              Lucid.strong_ "Genre: "
              Lucid.span_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml genre
            Nothing -> mempty

      -- Station Info
      Lucid.div_ [Lucid.class_ "bg-gray-800 text-white p-4"] $ do
        Lucid.h3_ [Lucid.class_ "font-bold mb-4 text-center"] "ABOUT KPBJ"
        Lucid.p_ [Lucid.class_ "text-sm leading-relaxed mb-4"] $
          "Community-powered radio serving Shadow Hills and beyond with underground music, "
            <> "local voices, and authentic programming since 2018."
        Lucid.div_ [Lucid.class_ "text-xs space-y-1"] $ do
          Lucid.div_ "📻 95.9 FM"
          Lucid.div_ "🌐 kpbj.fm"
          Lucid.div_ "📧 hello@kpbj.fm"
          Lucid.div_ "📞 (555) 959-KPBJ"

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
  Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer slug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie $ \(fmap snd -> mUserInfo) -> do
    showResult <- execQuerySpan (Show.getShowBySlug slug)

    case showResult of
      Left err -> do
        Log.logInfo "Failed to fetch show from database" (Aeson.object ["error" .= show err])
        renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load show. Please try again.")
      Right Nothing -> do
        Log.logInfo ("Show not found: " <> slug) ()
        renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
      Right (Just showModel) -> do
        episodesResult <- execQuerySpan (Episode.getEpisodesByShowId (Show.smId showModel))
        hostsResult <- execQuerySpan (Show.getShowHostsWithUsers (Show.smId showModel))
        schedulesResult <- execQuerySpan (Show.getShowSchedules (Show.smId showModel))

        case (episodesResult, hostsResult, schedulesResult) of
          (Right episodes, Right hosts, Right schedules) -> do
            let showTemplate = template showModel episodes hosts schedules
            renderTemplate hxRequest mUserInfo showTemplate
          _ ->
            -- If any query fails, show with empty data for the failed parts
            let episodes = fromRight [] episodesResult
                hosts = fromRight [] hostsResult
                schedules = fromRight [] schedulesResult
                showTemplate = template showModel episodes hosts schedules
             in renderTemplate hxRequest mUserInfo showTemplate
