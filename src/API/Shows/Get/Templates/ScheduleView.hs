{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Get.Templates.ScheduleView
  ( renderScheduleView,
    renderWeekNavigation,
    renderDesktopSchedule,
    renderMobileSchedule,
  )
where

import {-# SOURCE #-} API (showGetLink, showsScheduleGetLink)
import Control.Monad (unless)
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, DayOfWeek (..), TimeOfDay (..), addDays, defaultTimeLocale, formatTime)
import Domain.Types.Slug (Slug)
import Domain.Types.WeekOffset (WeekOffset (..))
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OrphanInstances.TimeOfDay (formatTimeOfDay)
import Servant.Links qualified as Links

-- | Create URL for show page
showGetUrl :: Slug -> Links.URI
showGetUrl slug = Links.linkURI $ showGetLink slug

-- | Main schedule view template
renderScheduleView :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> WeekOffset -> Day -> Lucid.Html ()
renderScheduleView scheduledShows currentDayOfWeek currentTimeOfDay weekOffset weekStart = do
  -- Week Navigation
  renderWeekNavigation weekOffset weekStart

  -- Desktop Schedule Grid (hidden on mobile)
  renderDesktopSchedule scheduledShows currentDayOfWeek currentTimeOfDay

  -- Mobile Schedule (hidden on desktop)
  renderMobileSchedule scheduledShows currentDayOfWeek currentTimeOfDay

-- | Week navigation component
renderWeekNavigation :: WeekOffset -> Day -> Lucid.Html ()
renderWeekNavigation (WeekOffset weekOffset) weekStart = do
  let weekEnd = addDays 6 weekStart
      weekLabel =
        if weekOffset == 0
          then "CURRENT WEEK"
          else formatDateRange weekStart weekEnd
      prevWeekUrl = showsScheduleWeekUrl (WeekOffset (weekOffset - 1))
      nextWeekUrl = showsScheduleWeekUrl (WeekOffset (weekOffset + 1))

  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-4 mb-6"] $ do
    Lucid.div_ [Lucid.class_ "flex justify-between items-center"] $ do
      -- Previous week button
      Lucid.button_
        [ Lucid.class_ "bg-gray-800 text-white px-4 py-2 font-bold hover:bg-gray-700",
          hxGet_ [i|/#{prevWeekUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true"
        ]
        "← PREV WEEK"

      -- Week display
      Lucid.h2_ [Lucid.class_ "text-xl font-bold"] $ Lucid.toHtml weekLabel

      -- Next week button
      Lucid.button_
        [ Lucid.class_ "bg-gray-800 text-white px-4 py-2 font-bold hover:bg-gray-700",
          hxGet_ [i|/#{nextWeekUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true"
        ]
        "NEXT WEEK →"
  where
    showsScheduleWeekUrl :: WeekOffset -> Links.URI
    showsScheduleWeekUrl offset = Links.linkURI $ showsScheduleGetLink (Just offset)

    formatDateRange :: Day -> Day -> Text
    formatDateRange start end =
      let startStr = Text.pack $ formatTime defaultTimeLocale "%b %-d" start
          endStr = Text.pack $ formatTime defaultTimeLocale "%b %-d, %Y" end
       in startStr <> " - " <> endStr

-- | Render desktop schedule grid
renderDesktopSchedule :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> Lucid.Html ()
renderDesktopSchedule scheduledShows currentDayOfWeek currentTimeOfDay = do
  Lucid.div_ [Lucid.class_ "hidden lg:block"] $ do
    Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800"] $ do
      -- Schedule Header
      Lucid.div_ [Lucid.class_ "grid grid-cols-8 border-b-2 border-gray-800"] $ do
        Lucid.div_ [Lucid.class_ "p-4 font-bold text-center border-r border-gray-300"] "TIME"
        renderDayHeader Monday currentDayOfWeek "MON"
        renderDayHeader Tuesday currentDayOfWeek "TUE"
        renderDayHeader Wednesday currentDayOfWeek "WED"
        renderDayHeader Thursday currentDayOfWeek "THU"
        renderDayHeader Friday currentDayOfWeek "FRI"
        renderDayHeader Saturday currentDayOfWeek "SAT"
        renderDayHeader Sunday currentDayOfWeek "SUN"

      -- Schedule Rows - group shows by time slots
      let timeSlots = ["6AM", "9AM", "12PM", "3PM", "6PM", "9PM"]
      mapM_ (renderTimeSlotRow scheduledShows currentDayOfWeek currentTimeOfDay) timeSlots

-- | Render day header with conditional highlighting
renderDayHeader :: DayOfWeek -> Maybe DayOfWeek -> Text -> Lucid.Html ()
renderDayHeader day currentDay dayName = do
  let isCurrentDay = Just day == currentDay
      baseClasses = "p-4 font-bold text-center"
      bgClass = if isCurrentDay then " bg-gray-800 text-white" else ""
      borderClass = if day == Sunday then "" else " border-r border-gray-300"
      classes = baseClasses <> bgClass <> borderClass
  Lucid.div_ [Lucid.class_ classes] $ Lucid.toHtml dayName

-- | Render a single time slot row
renderTimeSlotRow :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> Text -> Lucid.Html ()
renderTimeSlotRow scheduledShows currentDayOfWeek currentTimeOfDay timeSlot = do
  Lucid.div_ [Lucid.class_ "grid grid-cols-8 border-b border-gray-300"] $ do
    -- Time label
    Lucid.div_ [Lucid.class_ "p-3 text-center border-r border-gray-300 bg-gray-50 font-bold"] $
      Lucid.toHtml timeSlot

    -- Each day of the week
    mapM_ (renderDayCell scheduledShows currentDayOfWeek currentTimeOfDay timeSlot) [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

-- | Render a single day cell for a time slot
renderDayCell :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> Text -> DayOfWeek -> Lucid.Html ()
renderDayCell scheduledShows currentDayOfWeek currentTimeOfDay timeSlot dayOfWeek = do
  let showsForSlot = filter (\s -> ShowSchedule.sswdDayOfWeek s == dayOfWeek && matchesTimeSlot (ShowSchedule.sswdStartTime s) timeSlot) scheduledShows
      isCurrentDay = Just dayOfWeek == currentDayOfWeek
      -- Check if any show in this slot is currently airing
      isLiveShow = case (currentDayOfWeek, currentTimeOfDay, showsForSlot) of
        (Just currentDay, Just currentTime, show' : _) ->
          currentDay == dayOfWeek
            && currentTime >= ShowSchedule.sswdStartTime show'
            && currentTime < ShowSchedule.sswdEndTime show'
        _ -> False
      baseClasses = "p-3 text-center"
      bgClass = if isCurrentDay then " bg-gray-100" else ""
      borderClass = if dayOfWeek == Sunday then "" else " border-r border-gray-300"
      classes = baseClasses <> bgClass <> borderClass

  Lucid.div_ [Lucid.class_ classes] $ do
    case showsForSlot of
      [] -> Lucid.toHtml ("" :: Text)
      (show' : _) -> do
        let showUrl = showGetUrl (ShowSchedule.sswdShowSlug show')
        Lucid.div_ [Lucid.class_ "text-xs"] $ do
          if isLiveShow
            then Lucid.div_ [Lucid.class_ "font-bold uppercase text-[10px] mb-1 text-gray-800 animate-pulse"] "● ON AIR ●"
            else mempty
          Lucid.div_ [Lucid.class_ "font-bold"] $ do
            Lucid.a_
              [ Lucid.href_ [i|/#{showUrl}|],
                hxGet_ [i|/#{showUrl}|],
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                Lucid.class_ "hover:underline"
              ]
              $ Lucid.toHtml (ShowSchedule.sswdShowTitle show')
          Lucid.div_ [Lucid.class_ "text-gray-600"] $
            Lucid.toHtml (ShowSchedule.sswdHostName show')

-- | Check if a start time matches a time slot
matchesTimeSlot :: TimeOfDay -> Text -> Bool
matchesTimeSlot (TimeOfDay hour _ _) slot =
  case parseSlot slot of
    Just slotHour -> hour >= slotHour && hour < slotHour + 3
    _ -> False

-- | Parse slot label like "6AM" or "3PM" to hour (0-23)
parseSlot :: Text -> Maybe Int
parseSlot slot =
  let numPart = Text.takeWhile isDigit slot
      isPM = Text.isSuffixOf "PM" slot
   in case reads (Text.unpack numPart) of
        [(hour, "")] ->
          Just $ if isPM && hour /= 12 then hour + 12 else if not isPM && hour == 12 then 0 else hour
        _ -> Nothing

-- | Render mobile schedule (day by day)
renderMobileSchedule :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> Lucid.Html ()
renderMobileSchedule scheduledShows currentDayOfWeek currentTimeOfDay = do
  Lucid.div_ [Lucid.class_ "lg:hidden space-y-4"] $ do
    -- Group shows by day
    let daysOfWeek = [(Monday, "MONDAY"), (Tuesday, "TUESDAY"), (Wednesday, "WEDNESDAY"), (Thursday, "THURSDAY"), (Friday, "FRIDAY"), (Saturday, "SATURDAY"), (Sunday, "SUNDAY")]
    mapM_ (renderMobileDay scheduledShows currentDayOfWeek currentTimeOfDay) daysOfWeek

    Lucid.div_ [Lucid.class_ "text-center"] $ do
      Lucid.p_ [Lucid.class_ "text-gray-600 italic"] "Showing current week's schedule"

-- | Render a single day for mobile view
renderMobileDay :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> (DayOfWeek, Text) -> Lucid.Html ()
renderMobileDay scheduledShows currentDayOfWeek currentTimeOfDay (dayOfWeek, dayName) = do
  let showsForDay = filter (\s -> ShowSchedule.sswdDayOfWeek s == dayOfWeek) scheduledShows
      sortedShows = sortBy (\a b -> compare (ShowSchedule.sswdStartTime a) (ShowSchedule.sswdStartTime b)) showsForDay
      isCurrentDay = Just dayOfWeek == currentDayOfWeek
      headerClass = if isCurrentDay then "font-bold mb-3 text-center bg-gray-800 text-white py-2 border border-gray-800" else "font-bold mb-3 text-center bg-gray-100 py-2 border border-gray-300"
      dayLabel = if isCurrentDay then dayName <> " - TODAY" else dayName

  unless (null sortedShows) $ do
    Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-4"] $ do
      Lucid.h3_ [Lucid.class_ headerClass] $
        Lucid.toHtml dayLabel
      Lucid.div_ [Lucid.class_ "space-y-3"] $ do
        mapM_ (renderMobileShowItem currentDayOfWeek currentTimeOfDay dayOfWeek) sortedShows

-- | Render a single show item for mobile
renderMobileShowItem :: Maybe DayOfWeek -> Maybe TimeOfDay -> DayOfWeek -> ShowSchedule.ScheduledShowWithDetails -> Lucid.Html ()
renderMobileShowItem currentDayOfWeek currentTimeOfDay dayOfWeek show' = do
  let showUrl = showGetUrl (ShowSchedule.sswdShowSlug show')
      startTimeText = formatTimeOfDay (ShowSchedule.sswdStartTime show')
      -- Check if this show is currently airing
      isLiveShow = case (currentDayOfWeek, currentTimeOfDay) of
        (Just currentDay, Just currentTime) ->
          currentDay == dayOfWeek
            && currentTime >= ShowSchedule.sswdStartTime show'
            && currentTime < ShowSchedule.sswdEndTime show'
        _ -> False
      containerClass = "flex justify-between items-center p-3 border border-gray-300"
  Lucid.div_ [Lucid.class_ containerClass] $ do
    Lucid.div_ $ do
      -- Add "ON AIR" badge for live shows with blink animation
      if isLiveShow
        then Lucid.div_ [Lucid.class_ "font-bold uppercase text-xs mb-1 text-gray-800 animate-pulse"] "● LIVE NOW"
        else mempty
      Lucid.div_ [Lucid.class_ "font-bold"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{showUrl}|],
            hxGet_ [i|/#{showUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "hover:underline"
          ]
          $ Lucid.toHtml (startTimeText <> " - " <> ShowSchedule.sswdShowTitle show')
      Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] $
        Lucid.toHtml (ShowSchedule.sswdHostName show')
