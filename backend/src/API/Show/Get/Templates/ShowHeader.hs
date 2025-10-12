{-# LANGUAGE OverloadedRecordDot #-}

module API.Show.Get.Templates.ShowHeader
  ( renderShowHeader,
  )
where

--------------------------------------------------------------------------------

import Control.Monad (unless)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified

--------------------------------------------------------------------------------

-- | Render show header with info
renderShowHeader :: Shows.Model -> [ShowHost.ShowHostWithUser] -> [ShowSchedule.Model] -> Lucid.Html ()
renderShowHeader showModel hosts schedules = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-4 gap-8"] $ do
      -- Show Image
      Lucid.div_ [Lucid.class_ "lg:col-span-1"] $ do
        Lucid.div_ [Lucid.class_ "w-full aspect-square bg-gray-300 border-2 border-gray-600 flex items-center justify-center text-lg"] $ do
          case showModel.logoUrl of
            Just logoUrl -> Lucid.img_ [Lucid.src_ logoUrl, Lucid.alt_ showModel.title, Lucid.class_ "w-full h-full object-cover"]
            Nothing -> "[SHOW IMAGE]"

      -- Show Info
      Lucid.div_ [Lucid.class_ "lg:col-span-3"] $ do
        Lucid.div_ [Lucid.class_ "mb-4"] $ do
          Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-2"] $ Lucid.toHtml (Text.toUpper showModel.title)

          Lucid.div_ [Lucid.class_ "text-lg text-gray-600 mb-4"] $ do
            -- Show host information
            Lucid.span_ [Lucid.class_ "font-bold"] "Host: "
            case hosts of
              [] -> "TBD"
              (ShowHost.ShowHostWithUser {displayName = dn} : otherHosts) -> do
                Lucid.toHtml dn
                unless (null otherHosts) $ do
                  ", "
                  let otherNames = map (\(ShowHost.ShowHostWithUser {displayName = n}) -> Lucid.toHtml n) otherHosts
                  mconcat $ map (", " <>) otherNames
            " • "
            -- Show schedule information
            Lucid.span_ [Lucid.class_ "font-bold"] "Schedule: "
            case schedules of
              [] -> "TBD"
              (ShowSchedule.Model {dayOfWeek = dow, startTime = st, endTime = et} : _) -> do
                let dayName :: Text
                    dayName = case dow of
                      0 -> "Sunday"
                      1 -> "Monday"
                      2 -> "Tuesday"
                      3 -> "Wednesday"
                      4 -> "Thursday"
                      5 -> "Friday"
                      6 -> "Saturday"
                      _ -> "Unknown"
                Lucid.toHtml $ dayName <> "s " <> st <> " - " <> et
            " • "
            case showModel.genre of
              Just genre -> Lucid.span_ [Lucid.class_ "font-bold"] "Genre: " <> Lucid.toHtml genre
              Nothing -> mempty

        -- Show Description
        Lucid.div_ [Lucid.class_ "mb-6"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-3 uppercase border-b border-gray-800 pb-2"] "About The Show"
          Lucid.p_ [Lucid.class_ "mb-4 leading-relaxed"] $ Lucid.toHtml showModel.description
