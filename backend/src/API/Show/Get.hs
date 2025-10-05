{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Show.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showGetLink, showsGetLink)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad (unless, when)
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
import Effects.Database.Tables.ShowBlog qualified as ShowBlog
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

-- | Render a featured "Latest Episode" section with full details
renderLatestEpisode :: Episode.EpisodeModel -> [Episode.EpisodeTrackModel] -> Lucid.Html ()
renderLatestEpisode episode tracks = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Latest Episode"

    -- Episode header with image and info
    Lucid.div_ [Lucid.class_ "flex gap-4 mb-6"] $ do
      Lucid.div_ [Lucid.class_ "w-24 h-24 bg-gray-300 border border-gray-600 flex items-center justify-center text-xs flex-shrink-0"] $ do
        case episode.artworkUrl of
          Just artworkUrl -> Lucid.img_ [Lucid.src_ artworkUrl, Lucid.alt_ "Episode artwork", Lucid.class_ "w-full h-full object-cover"]
          Nothing -> "[EP IMG]"

      Lucid.div_ [Lucid.class_ "flex-grow"] $ do
        Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-2"] $ Lucid.toHtml episode.title
        Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-2"] $ do
          case episode.publishedAt of
            Just publishedAt -> do
              let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
              "Aired: " <> Lucid.toHtml dateStr
            Nothing -> "Draft"

          case episode.durationSeconds of
            Just duration ->
              let hours = duration `div` 3600
                  minutes = (duration `mod` 3600) `div` 60
               in if hours > 0
                    then " • Duration: " <> Lucid.toHtml (show hours) <> "h " <> Lucid.toHtml (show minutes) <> "min"
                    else " • Duration: " <> Lucid.toHtml (show minutes) <> "min"
            Nothing -> mempty

        case episode.description of
          Just desc -> Lucid.p_ [Lucid.class_ "text-sm mb-4"] $ Lucid.toHtml desc
          Nothing -> mempty

    -- Audio player (if audio file path exists)
    case episode.audioFilePath of
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
            case episode.durationSeconds of
              Just duration ->
                let hours = duration `div` 3600
                    minutes = (duration `mod` 3600) `div` 60
                    seconds = duration `mod` 60
                 in Lucid.span_ [Lucid.class_ "text-sm font-mono"] $
                      "0:00 / "
                        <> (if hours > 0 then Lucid.toHtml (show hours) <> ":" else "")
                        <> Lucid.toHtml (show minutes)
                        <> ":"
                        <> (if seconds < 10 then "0" else "")
                        <> Lucid.toHtml (show seconds)
              Nothing -> mempty
          Lucid.div_ [Lucid.class_ "text-xs text-gray-600"] $ do
            "Now Playing"
      Nothing -> mempty

    -- Track Listing
    unless (null tracks) $ do
      Lucid.div_ [Lucid.class_ "mb-6"] $ do
        Lucid.h4_ [Lucid.class_ "font-bold mb-3 text-sm uppercase"] "Track Listing"
        Lucid.div_ [Lucid.class_ "space-y-2 text-sm"] $ do
          mapM_ renderTrack (take 4 tracks)

          when (length tracks > 4) $ do
            Lucid.button_
              [ Lucid.class_ "text-xs text-gray-600 hover:text-gray-800 mt-2",
                Lucid.onclick_ "document.getElementById('all-tracks').classList.toggle('hidden')"
              ]
              $ "+ Show all " <> Lucid.toHtml (show (length tracks)) <> " tracks"
            Lucid.div_ [Lucid.id_ "all-tracks", Lucid.class_ "hidden space-y-2 mt-2"] $ do
              mapM_ renderTrack (drop 4 tracks)
  where
    renderTrack :: Episode.EpisodeTrackModel -> Lucid.Html ()
    renderTrack track = do
      Lucid.div_ [Lucid.class_ "flex justify-between p-2 hover:bg-gray-50"] $ do
        Lucid.div_ $ do
          Lucid.span_ [Lucid.class_ "font-medium"] $ "\"" <> Lucid.toHtml (track.title) <> "\""
          " - "
          Lucid.span_ $ Lucid.toHtml (track.artist)
          case track.album of
            Just album -> Lucid.span_ [Lucid.class_ "text-gray-600 ml-1"] $ " (" <> Lucid.toHtml album <> ")"
            Nothing -> mempty
        case track.duration of
          Just duration -> Lucid.span_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml duration
          Nothing -> mempty

-- | Render an episode card (for previous episodes list)
renderEpisodeCard :: Episode.EpisodeModel -> Lucid.Html ()
renderEpisodeCard episode = do
  Lucid.div_ [Lucid.class_ "flex gap-4 mb-6"] $ do
    -- Episode thumbnail
    Lucid.div_ [Lucid.class_ "w-24 h-24 bg-gray-300 border border-gray-600 flex items-center justify-center text-xs flex-shrink-0"] $ do
      case episode.artworkUrl of
        Just artworkUrl -> Lucid.img_ [Lucid.src_ artworkUrl, Lucid.alt_ "Episode artwork", Lucid.class_ "w-full h-full object-cover"]
        Nothing -> "[EP IMG]"

    -- Episode info
    Lucid.div_ [Lucid.class_ "flex-grow"] $ do
      Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-2"] $ Lucid.toHtml (episode.title)

      Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-2"] $ do
        case episode.publishedAt of
          Just publishedAt -> do
            let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
            "Aired: " <> Lucid.toHtml dateStr
          Nothing -> "Draft"

        case episode.durationSeconds of
          Just duration -> " • Duration: " <> Lucid.toHtml (show (duration `div` 60)) <> "min"
          Nothing -> mempty

      -- Episode description
      case episode.description of
        Just desc -> do
          let truncatedDesc = Text.take 200 desc
          Lucid.p_ [Lucid.class_ "text-sm mb-4"] $
            Lucid.toHtml $
              truncatedDesc <> if Text.length desc > 200 then "..." else ""
        Nothing -> mempty

      -- Audio player (if audio file path exists)
      case episode.audioFilePath of
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
              case episode.durationSeconds of
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
          case showModel.logoUrl of
            Just logoUrl -> Lucid.img_ [Lucid.src_ logoUrl, Lucid.alt_ (showModel.title), Lucid.class_ "w-full h-full object-cover"]
            Nothing -> "[SHOW IMAGE]"

        -- Social/Subscribe Buttons
        Lucid.div_ [Lucid.class_ "mt-4 space-y-2"] $ do
          Lucid.button_ [Lucid.class_ "w-full bg-gray-800 text-white py-2 px-4 font-bold hover:bg-gray-700"] "♡ FOLLOW SHOW"
          Lucid.button_ [Lucid.class_ "w-full border-2 border-gray-800 bg-white text-gray-800 py-2 px-4 font-bold hover:bg-gray-100"] "🔔 NOTIFICATIONS"

      -- Show Info
      Lucid.div_ [Lucid.class_ "lg:col-span-3"] $ do
        Lucid.div_ [Lucid.class_ "mb-4"] $ do
          Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-2"] $ Lucid.toHtml (Text.toUpper $ showModel.title)

          Lucid.div_ [Lucid.class_ "text-lg text-gray-600 mb-4"] $ do
            -- Show host information
            Lucid.span_ [Lucid.class_ "font-bold"] "Host: "
            case hosts of
              [] -> "TBD"
              (host : otherHosts) -> do
                let displayName = host.displayName
                Lucid.toHtml displayName
                unless (null otherHosts) $ do
                  ", "
                  let otherNames = map (Lucid.toHtml . (.displayName)) otherHosts
                  mconcat $ map (", " <>) otherNames
            " • "
            -- Show schedule information
            Lucid.span_ [Lucid.class_ "font-bold"] "Schedule: "
            case schedules of
              [] -> "TBD"
              (schedule : _) -> do
                let dayName = case schedule.dayOfWeek of
                      0 -> "Sunday"
                      1 -> "Monday"
                      2 -> "Tuesday"
                      3 -> "Wednesday"
                      4 -> "Thursday"
                      5 -> "Friday"
                      6 -> "Saturday"
                      _ -> "Unknown"
                    startTime = schedule.startTime
                    endTime = schedule.endTime
                Lucid.toHtml $ dayName <> "s " <> startTime <> " - " <> endTime
            " • "
            case showModel.genre of
              Just genre -> Lucid.span_ [Lucid.class_ "font-bold"] "Genre: " <> Lucid.toHtml genre
              Nothing -> mempty

        -- Show Description
        Lucid.div_ [Lucid.class_ "mb-6"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-3 uppercase border-b border-gray-800 pb-2"] "About The Show"
          Lucid.p_ [Lucid.class_ "mb-4 leading-relaxed"] $ Lucid.toHtml (showModel.description)

        -- Stats and Episode Count
        Lucid.div_ [Lucid.class_ "mb-6"] $ do
          Lucid.div_ [Lucid.class_ "grid grid-cols-2 md:grid-cols-4 gap-4 text-center"] $ do
            Lucid.div_ [Lucid.class_ "bg-gray-100 p-3 border border-gray-300"] $ do
              Lucid.div_ [Lucid.class_ "text-2xl font-bold"] $ Lucid.toHtml $ Prelude.show $ length episodes
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] "Episodes"

            Lucid.div_ [Lucid.class_ "bg-gray-100 p-3 border border-gray-300"] $ do
              Lucid.div_ [Lucid.class_ "text-2xl font-bold"] $ Lucid.toHtml $ Text.toUpper $ Text.pack $ Prelude.show $ showModel.status
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] "Status"

            Lucid.div_ [Lucid.class_ "bg-gray-100 p-3 border border-gray-300"] $ do
              Lucid.div_ [Lucid.class_ "text-2xl font-bold"] $ Lucid.toHtml $ Text.toUpper $ Text.pack $ Prelude.show $ showModel.frequency
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] "Frequency"

            Lucid.div_ [Lucid.class_ "bg-gray-100 p-3 border border-gray-300"] $ do
              Lucid.div_ [Lucid.class_ "text-2xl font-bold"] $
                case showModel.durationMinutes of
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
        Lucid.span_ [Lucid.class_ "text-gray-800 font-bold"] $ Lucid.toHtml (showModel.title)

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

-- | Render Host Bio section
renderHostBio :: Show.ShowHostWithUser -> Maybe Show.HostDetailsModel -> Lucid.Html ()
renderHostBio host mHostDetails = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] $
      "About " <> Lucid.toHtml (host.displayName)

    Lucid.div_ [Lucid.class_ "text-center mb-4"] $ do
      -- Host avatar
      Lucid.div_ [Lucid.class_ "w-24 h-24 bg-gray-300 border-2 border-gray-600 rounded-full mx-auto mb-3 flex items-center justify-center overflow-hidden"] $ do
        case host.avatarUrl of
          Just avatarUrl -> Lucid.img_ [Lucid.src_ avatarUrl, Lucid.alt_ "Host avatar", Lucid.class_ "w-full h-full object-cover"]
          Nothing -> Lucid.div_ [Lucid.class_ "text-xs"] "[HOST]"

      -- Host stats
      Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] $ do
        Lucid.div_ [Lucid.class_ "font-bold"] $ do
          "Host since: "
          let year = Text.pack $ formatTime defaultTimeLocale "%Y" (host.joinedAt)
          Lucid.toHtml year

    -- Bio text
    case mHostDetails >>= (.bio) of
      Just bioText -> Lucid.p_ [Lucid.class_ "text-sm mb-4 leading-relaxed"] $ Lucid.toHtml bioText
      Nothing -> mempty

    -- Contact and links
    Lucid.div_ [Lucid.class_ "text-xs space-y-1"] $ do
      case host.fullName of
        fullName | fullName /= "" ->
          Lucid.div_ $ do
            Lucid.strong_ "Name: "
            Lucid.toHtml fullName
        _ -> mempty

      case mHostDetails >>= (.websiteUrl) of
        Just website ->
          Lucid.div_ $ do
            Lucid.strong_ "Website: "
            Lucid.a_ [Lucid.href_ website, Lucid.target_ "_blank", Lucid.class_ "underline hover:no-underline"] $ Lucid.toHtml website
        Nothing -> mempty

      case mHostDetails >>= (.instagramHandle) of
        Just instagram ->
          Lucid.div_ $ do
            Lucid.strong_ "Instagram: "
            Lucid.toHtml $ "@" <> instagram
        Nothing -> mempty

      case mHostDetails >>= (.twitterHandle) of
        Just twitter ->
          Lucid.div_ $ do
            Lucid.strong_ "Twitter: "
            Lucid.toHtml $ "@" <> twitter
        Nothing -> mempty

-- | Render Show Stats section
renderShowStats :: Show.ShowModel -> [Episode.EpisodeModel] -> [Show.ShowScheduleModel] -> Lucid.Html ()
renderShowStats showModel episodes schedules = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Show Stats"

    Lucid.div_ [Lucid.class_ "space-y-3 text-sm"] $ do
      -- Total Episodes
      Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
        Lucid.span_ "Total Episodes:"
        Lucid.span_ [Lucid.class_ "font-bold"] $ Lucid.toHtml $ Prelude.show $ length episodes

      -- Average Duration
      unless (null episodes) $ do
        let durations = [d | ep <- episodes, Just d <- [ep.durationSeconds]]
            avgDuration = if null durations then 0 else sum durations `div` fromIntegral (length durations)
            avgMinutes = avgDuration `div` 60
        when (avgDuration > 0) $ do
          Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
            Lucid.span_ "Average Duration:"
            Lucid.span_ [Lucid.class_ "font-bold"] $ Lucid.toHtml (show avgMinutes) <> "min"

      -- Show Status
      Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
        Lucid.span_ "Status:"
        Lucid.span_ [Lucid.class_ "font-bold"] $ Lucid.toHtml $ Text.toUpper $ Text.pack $ Prelude.show $ showModel.status

      -- Next Show (if there's a schedule)
      unless (null schedules) $ do
        case schedules of
          (schedule : _) -> do
            Lucid.div_ [Lucid.class_ "flex justify-between"] $ do
              Lucid.span_ "Next Show:"
              let dayName = case schedule.dayOfWeek of
                    0 -> "Sun"
                    1 -> "Mon"
                    2 -> "Tue"
                    3 -> "Wed"
                    4 -> "Thu"
                    5 -> "Fri"
                    6 -> "Sat"
                    _ -> "???"
                  startTime = schedule.startTime
              Lucid.span_ [Lucid.class_ "font-bold text-green-700"] $ Lucid.toHtml $ dayName <> " " <> startTime
          _ -> mempty

-- | Render Recent Blog Posts section
renderRecentBlogPosts :: [ShowBlog.ShowBlogPostModel] -> Lucid.Html ()
renderRecentBlogPosts blogPosts = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Recent from Host Blog"

    if null blogPosts
      then Lucid.p_ [Lucid.class_ "text-sm text-gray-600"] "No blog posts yet."
      else do
        Lucid.div_ [Lucid.class_ "space-y-4"] $ do
          mapM_ renderBlogPostPreview (take 3 blogPosts)
  where
    renderBlogPostPreview :: ShowBlog.ShowBlogPostModel -> Lucid.Html ()
    renderBlogPostPreview post = do
      Lucid.article_ [Lucid.class_ "border-l-4 border-gray-800 pl-4"] $ do
        case post.publishedAt of
          Just publishedAt -> do
            let dateStr = Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" publishedAt
            Lucid.div_ [Lucid.class_ "text-xs text-gray-600 mb-1"] $ Lucid.toHtml dateStr
          Nothing -> mempty

        Lucid.h4_ [Lucid.class_ "font-bold text-sm mb-2"] $ Lucid.toHtml (post.title)

        case post.excerpt of
          Just excerpt -> do
            let truncated = if Text.length excerpt > 150 then Text.take 150 excerpt <> "..." else excerpt
            Lucid.p_ [Lucid.class_ "text-xs text-gray-700 leading-relaxed mb-2"] $ Lucid.toHtml truncated
          Nothing -> mempty

        Lucid.a_ [Lucid.href_ "#", Lucid.class_ "text-xs text-gray-800 underline hover:no-underline"] "Read more"

-- | Main show page template
template :: Show.ShowModel -> [Episode.EpisodeModel] -> Maybe [Episode.EpisodeTrackModel] -> [Show.ShowHostWithUser] -> [Show.ShowScheduleModel] -> Maybe Show.HostDetailsModel -> [ShowBlog.ShowBlogPostModel] -> Lucid.Html ()
template showModel episodes latestEpisodeTracks hosts schedules mHostDetails blogPosts = do
  -- Breadcrumb
  renderBreadcrumb showModel

  -- Show Header
  renderShowHeader showModel episodes hosts schedules

  -- Content Tabs Navigation
  Lucid.div_ [Lucid.class_ "mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "border-b-2 border-gray-800"] $ do
      Lucid.nav_ [Lucid.class_ "flex gap-8"] $ do
        Lucid.button_ [Lucid.class_ "py-3 px-4 font-bold uppercase border-b-2 border-gray-800 bg-white -mb-0.5"] "Episodes"
        Lucid.button_ [Lucid.class_ "py-3 px-4 font-bold uppercase text-gray-600 hover:text-gray-800"] "Blog"

  -- Main Content Grid
  Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-3 gap-8 w-full"] $ do
    -- Episodes Section
    Lucid.section_ [Lucid.class_ "lg:col-span-2"] $ do
      if null episodes
        then do
          Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
            Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "No Episodes Yet"
            Lucid.p_ [Lucid.class_ "text-gray-600 mb-6"] "This show hasn't published any episodes yet. Check back soon!"
        else do
          -- Featured/Latest Episode with tracks
          case (episodes, latestEpisodeTracks) of
            (latestEpisode : otherEpisodes, Just tracks) -> do
              renderLatestEpisode latestEpisode tracks

              -- Other Episodes
              unless (null otherEpisodes) $ do
                Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
                  Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Previous Episodes"
                  mapM_ renderEpisodeCard otherEpisodes
            (latestEpisode : otherEpisodes, Nothing) -> do
              -- Fallback if tracks failed to load
              renderLatestEpisode latestEpisode []

              unless (null otherEpisodes) $ do
                Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
                  Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Previous Episodes"
                  mapM_ renderEpisodeCard otherEpisodes
            _ -> mempty

    -- Sidebar
    Lucid.aside_ [Lucid.class_ "flex flex-col gap-8"] $ do
      -- Host Bio (show primary host)
      case hosts of
        (primaryHost : _) -> renderHostBio primaryHost mHostDetails
        [] -> mempty

      -- Show Stats
      renderShowStats showModel episodes schedules

      -- Recent Blog Posts
      renderRecentBlogPosts blogPosts

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
        episodesResult <- execQuerySpan (Episode.getEpisodesByShowId (showModel.id))
        hostsResult <- execQuerySpan (Show.getShowHostsWithUsers (showModel.id))
        schedulesResult <- execQuerySpan (Show.getShowSchedules (showModel.id))

        -- Fetch tracks for the latest episode if episodes exist
        latestEpisodeTracks <- case episodesResult of
          Right (latestEpisode : _) -> do
            tracksResult <- execQuerySpan (Episode.getTracksForEpisode (latestEpisode.id))
            pure $ case tracksResult of
              Right tracks -> Just tracks
              Left _ -> Nothing
          _ -> pure Nothing

        -- Fetch host details for the primary host
        mHostDetails <- case hostsResult of
          Right (primaryHost : _) -> do
            hostDetailsResult <- execQuerySpan (Show.getHostDetails (primaryHost.userId))
            pure $ case hostDetailsResult of
              Right details -> details
              Left _ -> Nothing
          _ -> pure Nothing

        -- Fetch recent blog posts for this show
        blogPostsResult <- execQuerySpan (ShowBlog.getPublishedShowBlogPosts (showModel.id) 3 0)
        let blogPosts = fromRight [] blogPostsResult

        case (episodesResult, hostsResult, schedulesResult) of
          (Right episodes, Right hosts, Right schedules) -> do
            let showTemplate = template showModel episodes latestEpisodeTracks hosts schedules mHostDetails blogPosts
            renderTemplate hxRequest mUserInfo showTemplate
          _ ->
            -- If any query fails, show with empty data for the failed parts
            let episodes = fromRight [] episodesResult
                hosts = fromRight [] hostsResult
                schedules = fromRight [] schedulesResult
                showTemplate = template showModel episodes latestEpisodeTracks hosts schedules mHostDetails blogPosts
             in renderTemplate hxRequest mUserInfo showTemplate
