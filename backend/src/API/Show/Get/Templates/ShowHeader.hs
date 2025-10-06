{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Show.Get.Templates.ShowHeader
  ( renderShowHeader,
    renderBreadcrumb,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showsGetLink)
import Control.Monad (unless)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Effects.Database.Tables.Episode qualified as Episode
import Effects.Database.Tables.Show qualified as Show
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
showsGetUrl :: Links.URI
showsGetUrl = Links.linkURI $ showsGetLink Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------

-- | Render show header with info
renderShowHeader :: Show.ShowModel -> [Episode.EpisodeModel] -> [Show.ShowHostWithUser] -> [Show.ShowScheduleModel] -> Lucid.Html ()
renderShowHeader showModel episodes hosts schedules = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8"] $ do
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-4 gap-8"] $ do
      -- Show Image
      Lucid.div_ [Lucid.class_ "lg:col-span-1"] $ do
        Lucid.div_ [Lucid.class_ "w-full aspect-square bg-gray-300 border-2 border-gray-600 flex items-center justify-center text-lg"] $ do
          case showModel.logoUrl of
            Just logoUrl -> Lucid.img_ [Lucid.src_ logoUrl, Lucid.alt_ showModel.title, Lucid.class_ "w-full h-full object-cover"]
            Nothing -> "[SHOW IMAGE]"

        -- Social/Subscribe Buttons
        Lucid.div_ [Lucid.class_ "mt-4 space-y-2"] $ do
          Lucid.button_ [Lucid.class_ "w-full bg-gray-800 text-white py-2 px-4 font-bold hover:bg-gray-700"] "♡ FOLLOW SHOW"
          Lucid.button_ [Lucid.class_ "w-full border-2 border-gray-800 bg-white text-gray-800 py-2 px-4 font-bold hover:bg-gray-100"] "🔔 NOTIFICATIONS"

      -- Show Info
      Lucid.div_ [Lucid.class_ "lg:col-span-3"] $ do
        Lucid.div_ [Lucid.class_ "mb-4"] $ do
          Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-2"] $ Lucid.toHtml (Text.toUpper showModel.title)

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
          Lucid.p_ [Lucid.class_ "mb-4 leading-relaxed"] $ Lucid.toHtml showModel.description

        -- Stats and Episode Count
        Lucid.div_ [Lucid.class_ "mb-6"] $ do
          Lucid.div_ [Lucid.class_ "grid grid-cols-2 md:grid-cols-4 gap-4 text-center"] $ do
            Lucid.div_ [Lucid.class_ "bg-gray-100 p-3 border border-gray-300"] $ do
              Lucid.div_ [Lucid.class_ "text-2xl font-bold"] $ Lucid.toHtml $ Prelude.show $ length episodes
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] "Episodes"

            Lucid.div_ [Lucid.class_ "bg-gray-100 p-3 border border-gray-300"] $ do
              Lucid.div_ [Lucid.class_ "text-2xl font-bold"] $ Lucid.toHtml $ Text.toUpper $ Text.pack $ Prelude.show showModel.status
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] "Status"

            Lucid.div_ [Lucid.class_ "bg-gray-100 p-3 border border-gray-300"] $ do
              Lucid.div_ [Lucid.class_ "text-2xl font-bold"] $ Lucid.toHtml $ Text.toUpper $ Text.pack $ Prelude.show showModel.frequency
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
        Lucid.span_ [Lucid.class_ "text-gray-800 font-bold"] $ Lucid.toHtml showModel.title
