{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Get.Templates.ShowHeader
  ( renderShowHeader,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (mediaGetLink)
import Control.Monad (unless)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI mediaGetLink

-- | Render show header with info
renderShowHeader :: Shows.Model -> [ShowHost.ShowHostWithUser] -> [ShowSchedule.Model] -> Lucid.Html ()
renderShowHeader showModel hosts schedules = do
  -- Banner Image (if present)
  case showModel.bannerUrl of
    Just bannerUrl -> do
      let bannerAlt = showModel.title <> " banner"
      Lucid.div_ [Lucid.class_ "w-full mb-8 border-2 border-gray-800 overflow-hidden"] $ do
        Lucid.img_ [Lucid.src_ [i|/#{mediaGetUrl}/#{bannerUrl}|], Lucid.alt_ bannerAlt, Lucid.class_ "w-full h-auto object-cover"]
    Nothing -> mempty

  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-4 gap-8"] $ do
      -- Show Logo
      Lucid.div_ [Lucid.class_ "lg:col-span-1"] $ do
        Lucid.div_ [Lucid.class_ "w-full aspect-square bg-gray-300 border-2 border-gray-600 flex items-center justify-center text-lg"] $ do
          case showModel.logoUrl of
            Just logoUrl -> do
              let logoAlt = showModel.title <> " logo"
              Lucid.img_ [Lucid.src_ [i|/#{mediaGetUrl}/#{logoUrl}|], Lucid.alt_ logoAlt, Lucid.class_ "w-full h-full object-cover"]
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
