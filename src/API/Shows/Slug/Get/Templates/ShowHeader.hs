{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Get.Templates.ShowHeader
  ( renderShowHeader,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks)
import API.Types
import Control.Monad (unless)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (DayOfWeek (..))
import Design (base, class_, desktop)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import OrphanInstances.TimeOfDay (formatTimeOfDay)
import Rel8 (Result)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

-- | Render show header with info
renderShowHeader :: Shows.Model -> [ShowHost.ShowHostWithUser] -> [ShowSchedule.ScheduleTemplate Result] -> Lucid.Html ()
renderShowHeader showModel hosts schedules = do
  -- Banner Image (if present)
  case showModel.bannerUrl of
    Just bannerUrl -> do
      let bannerAlt = showModel.title <> " banner"
      Lucid.div_ [class_ $ base [Tokens.fullWidth, Tokens.mb8, Tokens.cardBorder, "overflow-hidden"]] $ do
        Lucid.img_ [Lucid.src_ [i|/#{mediaGetUrl}/#{bannerUrl}|], Lucid.alt_ bannerAlt, Lucid.class_ "w-full h-auto object-cover"]
    Nothing -> mempty

  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.p8, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap8]; desktop ["grid-cols-4"] }] $ do
      -- Show Logo
      Lucid.div_ [class_ $ desktop ["col-span-1"]] $ do
        Lucid.div_ [class_ $ base [Tokens.fullWidth, "aspect-square", "bg-gray-300", Tokens.border2, "border-gray-600", "flex", "items-center", "justify-center", Tokens.textLg]] $ do
          case showModel.logoUrl of
            Just logoUrl -> do
              let logoAlt = showModel.title <> " logo"
              Lucid.img_ [Lucid.src_ [i|/#{mediaGetUrl}/#{logoUrl}|], Lucid.alt_ logoAlt, Lucid.class_ "w-full h-full object-cover"]
            Nothing -> "[SHOW IMAGE]"

      -- Show Info
      Lucid.div_ [class_ $ desktop ["col-span-3"]] $ do
        Lucid.div_ [Lucid.class_ Tokens.mb4] $ do
          Lucid.h1_ [class_ $ base [Tokens.text3xl, Tokens.fontBold, Tokens.mb2]] $ Lucid.toHtml (Text.toUpper showModel.title)

          Lucid.div_ [class_ $ base [Tokens.textLg, Tokens.textGray600, Tokens.mb4]] $ do
            -- Show host information
            Lucid.span_ [Lucid.class_ Tokens.fontBold] "Host: "
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
            Lucid.span_ [Lucid.class_ Tokens.fontBold] "Schedule: "
            case schedules of
              [] -> "TBD"
              (ShowSchedule.ScheduleTemplate {stDayOfWeek = mDow, stStartTime = st, stEndTime = et} : _) -> do
                case mDow of
                  Nothing -> "One-time show"
                  Just dow -> do
                    let dayName :: Text
                        dayName = case dow of
                          Sunday -> "Sunday"
                          Monday -> "Monday"
                          Tuesday -> "Tuesday"
                          Wednesday -> "Wednesday"
                          Thursday -> "Thursday"
                          Friday -> "Friday"
                          Saturday -> "Saturday"
                    Lucid.toHtml $ dayName <> "s " <> formatTimeOfDay st <> " - " <> formatTimeOfDay et
            " • "
            case showModel.genre of
              Just genre -> Lucid.span_ [Lucid.class_ Tokens.fontBold] "Genre: " <> Lucid.toHtml genre
              Nothing -> mempty

        -- Show Description
        Lucid.div_ [Lucid.class_ Tokens.mb6] $ do
          Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, "mb-3", "uppercase", "border-b", "border-gray-800", Tokens.pb2]] ""
          Lucid.p_ [class_ $ base [Tokens.mb4, "leading-relaxed"]] $ Lucid.toHtml showModel.description
