{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.Slug.Get.Templates.ShowHeader
  ( renderShowHeader,
  )
where

--------------------------------------------------------------------------------

import API.Links (showsLinks)
import API.Types
import Control.Monad (forM_, unless)
import Data.List (sortOn)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (DayOfWeek (..))
import Design (base, class_, desktop)
import Design.Tokens qualified as Tokens
import Domain.Types.Filter (Filter (..))
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.HTMX
import OrphanInstances.TimeOfDay (formatScheduleDual, formatWeeksOfMonth)
import Rel8 (Result)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Render show header with info
renderShowHeader :: StorageBackend -> Shows.Model -> [ShowHost.ShowHostWithUser] -> [ShowSchedule.ScheduleTemplate Result] -> [ShowTags.Model] -> Lucid.Html ()
renderShowHeader backend showModel hosts schedules tags = do
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", Tokens.p8, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap8]; desktop ["grid-cols-4"] }] $ do
      -- Show Logo
      Lucid.div_ [class_ $ desktop ["col-span-1"]] $ do
        Lucid.div_ [class_ $ base [Tokens.fullWidth, "aspect-square", "bg-gray-300 dark:bg-gray-600", Tokens.border2, "border-gray-600 dark:border-gray-500", "flex", "items-center", "justify-center", Tokens.textLg]] $ do
          case showModel.logoUrl of
            Just logoPath -> do
              let logoAlt = showModel.title <> " logo"
              Lucid.img_ [Lucid.src_ (buildMediaUrl backend logoPath), Lucid.alt_ logoAlt, class_ $ base [Tokens.fullWidth, "h-full", "object-cover"]]
            Nothing -> "[SHOW IMAGE]"

      -- Show Info
      Lucid.div_ [class_ $ desktop ["col-span-3"]] $ do
        Lucid.div_ [Lucid.class_ Tokens.mb4] $ do
          Lucid.h1_ [class_ $ base [Tokens.text3xl, Tokens.fontBold, Tokens.mb2]] $ Lucid.toHtml (Text.toUpper showModel.title)

          Lucid.div_ [class_ $ base [Tokens.textLg, Tokens.fgMuted, Tokens.mb4]] $ do
            -- Show host information
            Lucid.span_ [Lucid.class_ Tokens.fontBold] "Host: "
            case hosts of
              [] -> ""
              (ShowHost.ShowHostWithUser {displayName = dn} : otherHosts) -> do
                Lucid.toHtml dn
                unless (null otherHosts) $ do
                  ", "
                  let otherNames = map (\(ShowHost.ShowHostWithUser {displayName = n}) -> Lucid.toHtml n) otherHosts
                  mconcat $ map (", " <>) otherNames
            " • "
            -- Show schedule information (sorted by start time, earliest first)
            Lucid.span_ [Lucid.class_ Tokens.fontBold] "Schedule: "
            case sortOn ShowSchedule.stStartTime schedules of
              [] -> ""
              sortedSchedules -> do
                let scheduleTexts = map renderSchedule sortedSchedules
                Lucid.toHtml $ Text.intercalate " • " scheduleTexts

        -- Show Description (only if present)
        forM_ showModel.description $ \description ->
          Lucid.div_ [Lucid.class_ Tokens.mb6] $ do
            Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, "mb-3", "uppercase", "border-b", Tokens.borderDefault, Tokens.pb2]] "About The Show"
            Lucid.p_ [class_ $ base [Tokens.mb4, "leading-relaxed"]] $ Lucid.toHtml description

        -- Tags
        unless (null tags) $ do
          Lucid.div_ [class_ $ base ["flex", "flex-wrap", Tokens.gap2]] $ do
            forM_ tags $ \tag -> do
              let tagUrl = showsGetByTagUrl (ShowTags.stId tag)
              Lucid.a_
                [ Lucid.href_ [i|/#{tagUrl}|],
                  hxGet_ [i|/#{tagUrl}|],
                  hxTarget_ "#main-content",
                  hxPushUrl_ "true",
                  hxSwap_ "innerHTML",
                  class_ $ base [Tokens.px3, "py-1", Tokens.textSm, "border", "border-gray-400 dark:border-gray-500", "bg-gray-100 dark:bg-gray-700", "hover:bg-gray-200 dark:hover:bg-gray-600", "transition-colors"]
                ]
                $ Lucid.toHtml (ShowTags.stName tag)
  where
    renderSchedule :: ShowSchedule.ScheduleTemplate Result -> Text
    renderSchedule ShowSchedule.ScheduleTemplate {stDayOfWeek = mDow, stWeeksOfMonth = weeksOfMonth, stStartTime = st, stEndTime = et, stAirsTwiceDaily = airsTwice} =
      case mDow of
        Nothing -> "One-time show"
        Just dow ->
          let dayName :: Text
              dayName = case dow of
                Sunday -> "Sunday"
                Monday -> "Monday"
                Tuesday -> "Tuesday"
                Wednesday -> "Wednesday"
                Thursday -> "Thursday"
                Friday -> "Friday"
                Saturday -> "Saturday"
           in formatWeeksOfMonth weeksOfMonth <> dayName <> "s " <> formatScheduleDual st et airsTwice

    showsGetByTagUrl :: ShowTags.Id -> Links.URI
    showsGetByTagUrl tagId = Links.linkURI $ showsLinks.list Nothing (Just (Filter (Just tagId))) Nothing Nothing Nothing
