{-# LANGUAGE QuasiQuotes #-}

module API.Schedule.Get.Templates.Components
  ( renderScheduleView,
    renderWeekNavigation,
    renderDesktopSchedule,
    renderMobileSchedule,
  )
where

import API.Links (scheduleLink, showsLinks)
import API.Types
import Control.Monad (unless)
import Data.List (sortBy)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, DayOfWeek (..), TimeOfDay (..), addDays, defaultTimeLocale, formatTime)
import Design.StyleBuilder (also, base, desktop, styles, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.WeekOffset (WeekOffset (..))
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OrphanInstances.TimeOfDay (formatTimeOfDay)
import Servant.Links qualified as Links

-- | Create URL for show page
showGetUrl :: Slug -> Links.URI
showGetUrl slug = Links.linkURI $ showsLinks.detail slug Nothing

-- | Get the next day of the week (for overnight show handling)
succDay :: DayOfWeek -> DayOfWeek
succDay Sunday = Monday
succDay Monday = Tuesday
succDay Tuesday = Wednesday
succDay Wednesday = Thursday
succDay Thursday = Friday
succDay Friday = Saturday
succDay Saturday = Sunday

-- | Check if a show is currently live, handling overnight shows
--
-- For overnight shows (where end_time <= start_time), the show spans two calendar days.
-- E.g., Tuesday 23:00 - 00:00 means Tuesday 23:00 to Wednesday 00:00.
isShowLive :: Maybe DayOfWeek -> Maybe TimeOfDay -> DayOfWeek -> TimeOfDay -> TimeOfDay -> Bool
isShowLive mCurrentDay mCurrentTime showDay startTime endTime =
  case (mCurrentDay, mCurrentTime) of
    (Just currentDay, Just currentTime) ->
      let isOvernight = endTime <= startTime
       in if isOvernight
            then -- Show crosses midnight: live if (same day AND after start) OR (next day AND before end)
              (currentDay == showDay && currentTime >= startTime)
                || (currentDay == succDay showDay && currentTime < endTime)
            else -- Normal same-day show
              currentDay == showDay
                && currentTime >= startTime
                && currentTime < endTime
    _ -> False

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
      prevWeekUrl = scheduleWeekUrl (WeekOffset (weekOffset - 1))
      nextWeekUrl = scheduleWeekUrl (WeekOffset (weekOffset + 1))

  Lucid.div_ [Lucid.class_ weekNavContainerStyles] $ do
    Lucid.div_ [Lucid.class_ weekNavFlexStyles] $ do
      -- Previous week button
      Lucid.button_
        [ Lucid.class_ weekNavButtonStyles,
          hxGet_ [i|/#{prevWeekUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true"
        ]
        "← PREV"

      -- Week display
      Lucid.h2_ [Lucid.class_ weekLabelStyles] $ Lucid.toHtml weekLabel

      -- Next week button
      Lucid.button_
        [ Lucid.class_ weekNavButtonStyles,
          hxGet_ [i|/#{nextWeekUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true"
        ]
        "NEXT →"
  where
    scheduleWeekUrl :: WeekOffset -> Links.URI
    scheduleWeekUrl offset = Links.linkURI $ scheduleLink (Just offset)

    formatDateRange :: Day -> Day -> Text
    formatDateRange start end =
      let startStr = Text.pack $ formatTime defaultTimeLocale "%b %-d" start
          endStr = Text.pack $ formatTime defaultTimeLocale "%b %-d, %Y" end
       in startStr <> " - " <> endStr

--------------------------------------------------------------------------------
-- Week Navigation Styles

weekNavContainerStyles :: Text
weekNavContainerStyles = styles $ do
  base [Tokens.bgWhite, Tokens.cardBorder]
  base [Tokens.p3, Tokens.mb4]
  tablet [Tokens.p4, Tokens.mb6]

weekNavFlexStyles :: Text
weekNavFlexStyles = styles $ do
  base ["flex", "justify-between", "items-center", Tokens.gap2]
  tablet [Tokens.gap4]

weekNavButtonStyles :: Text
weekNavButtonStyles = styles $ do
  base [Tokens.bgGray800, Tokens.textWhite, Tokens.fontBold]
  base [Tokens.px3, Tokens.py2, Tokens.textSm]
  tablet [Tokens.px4, Tokens.textBase]
  also ["hover:bg-gray-700"]

weekLabelStyles :: Text
weekLabelStyles = styles $ do
  base [Tokens.fontBold, "text-center"]
  base [Tokens.textBase]
  tablet [Tokens.textXl]

-- | Render desktop schedule grid (Google Calendar style with spanning shows)
renderDesktopSchedule :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> Lucid.Html ()
renderDesktopSchedule scheduledShows currentDayOfWeek currentTimeOfDay = do
  Lucid.div_ [Lucid.class_ desktopContainerStyles] $ do
    Lucid.div_ [Lucid.class_ desktopGridWrapperStyles] $ do
      -- Schedule Header (fixed row outside the grid)
      Lucid.div_ [Lucid.class_ desktopHeaderRowStyles] $ do
        Lucid.div_ [Lucid.class_ timeHeaderCellStyles] "TIME"
        renderDayHeader Monday currentDayOfWeek "MON"
        renderDayHeader Tuesday currentDayOfWeek "TUE"
        renderDayHeader Wednesday currentDayOfWeek "WED"
        renderDayHeader Thursday currentDayOfWeek "THU"
        renderDayHeader Friday currentDayOfWeek "FRI"
        renderDayHeader Saturday currentDayOfWeek "SAT"
        renderDayHeader Sunday currentDayOfWeek "SUN"

      -- Main calendar grid: 8 columns (time + 7 days) x 24 rows (hours)
      -- Using CSS grid with explicit row placement for spanning shows
      Lucid.div_ [Lucid.class_ desktopGridStyles, Lucid.style_ "grid-template-rows: repeat(24, minmax(60px, auto));"] $ do
        -- Render time labels in first column
        mapM_ renderTimeLabel [0 .. 23]

        -- Render day columns with background
        mapM_ (renderDayColumn currentDayOfWeek) [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

        -- Render shows as absolutely positioned elements that span rows
        mapM_ (renderCalendarShow currentDayOfWeek currentTimeOfDay) scheduledShows

--------------------------------------------------------------------------------
-- Desktop Schedule Styles

desktopContainerStyles :: Text
desktopContainerStyles = styles $ do
  base ["hidden", "overflow-x-auto"]
  desktop ["block"]

desktopGridWrapperStyles :: Text
desktopGridWrapperStyles = styles $ do
  base [Tokens.bgWhite, Tokens.cardBorder, "min-w-[800px]"]

desktopHeaderRowStyles :: Text
desktopHeaderRowStyles = styles $ do
  base ["grid", "grid-cols-8", "border-b-2", "border-gray-800", "sticky", "top-0", Tokens.bgWhite, "z-10"]

desktopGridStyles :: Text
desktopGridStyles = styles $ do
  base ["grid", "grid-cols-8", "relative"]

timeHeaderCellStyles :: Text
timeHeaderCellStyles = styles $ do
  base [Tokens.p4, Tokens.fontBold, "text-center", "border-r", "border-gray-300"]

-- | Render day header with conditional highlighting
renderDayHeader :: DayOfWeek -> Maybe DayOfWeek -> Text -> Lucid.Html ()
renderDayHeader day currentDay dayName = do
  let isCurrentDay = Just day == currentDay
      headerStyles = dayHeaderStyles isCurrentDay (day /= Sunday)
  Lucid.div_ [Lucid.class_ headerStyles] $ Lucid.toHtml dayName

dayHeaderStyles :: Bool -> Bool -> Text
dayHeaderStyles isCurrentDay hasBorder = styles $ do
  base [Tokens.p4, Tokens.fontBold, "text-center"]
  if isCurrentDay
    then base [Tokens.bgGray800, Tokens.textWhite]
    else base []
  if hasBorder
    then base ["border-r", "border-gray-300"]
    else base []

-- | Render time label for a specific hour
renderTimeLabel :: Int -> Lucid.Html ()
renderTimeLabel hour = do
  let timeText = formatHour hour
      rowStyle = [i|grid-row: #{hour + 1}; grid-column: 1;|]
  Lucid.div_
    [ Lucid.class_ timeLabelStyles,
      Lucid.style_ rowStyle
    ]
    $ Lucid.toHtml timeText

timeLabelStyles :: Text
timeLabelStyles = styles $ do
  base [Tokens.p2, Tokens.textXs, "text-center", Tokens.fontBold]
  base ["border-r", "border-b", "border-gray-300", "bg-gray-50"]
  base ["flex", "items-start", "justify-center"]

-- | Format hour (0-23) as "12AM", "1AM", etc.
formatHour :: Int -> Text
formatHour hour
  | hour == 0 = "12AM"
  | hour < 12 = Text.pack (show hour) <> "AM"
  | hour == 12 = "12PM"
  | otherwise = Text.pack (show (hour - 12)) <> "PM"

-- | Render a day column background (all 24 rows)
renderDayColumn :: Maybe DayOfWeek -> DayOfWeek -> Lucid.Html ()
renderDayColumn currentDayOfWeek day = do
  let isCurrentDay = Just day == currentDayOfWeek
      dayCol = dayToColumn day
      bgClass :: Text
      bgClass = if isCurrentDay then "bg-gray-50" else "bg-white"
      borderClass :: Text
      borderClass = if day == Sunday then "border-b" else "border-r border-b"
      columnStyle = [i|grid-row: 1 / 25; grid-column: #{dayCol};|]
  Lucid.div_
    [ Lucid.class_ [i|#{bgClass} #{borderClass} border-gray-300|],
      Lucid.style_ columnStyle
    ]
    mempty

-- | Convert day of week to grid column number (2-8)
dayToColumn :: DayOfWeek -> Int
dayToColumn Monday = 2
dayToColumn Tuesday = 3
dayToColumn Wednesday = 4
dayToColumn Thursday = 5
dayToColumn Friday = 6
dayToColumn Saturday = 7
dayToColumn Sunday = 8

-- | Render a show as a calendar block that spans rows
renderCalendarShow :: Maybe DayOfWeek -> Maybe TimeOfDay -> ShowSchedule.ScheduledShowWithDetails -> Lucid.Html ()
renderCalendarShow currentDayOfWeek currentTimeOfDay show' = do
  let startTime = ShowSchedule.sswdStartTime show'
      endTime = ShowSchedule.sswdEndTime show'
      TimeOfDay startHour _ _ = startTime
      TimeOfDay endHour endMin _ = endTime
      -- Detect overnight shows (end_time <= start_time means crosses midnight)
      isOvernight = endTime <= startTime
      -- Calculate grid row positions (1-indexed, add 1 for header)
      startRow = startHour + 1
      -- For overnight shows, extend to end of grid (row 25 = midnight)
      endRow =
        if isOvernight
          then 25 -- Extend to end of day (midnight)
          else endHour + 1 + (if endMin > 0 then 1 else 0) -- Round up if minutes
      dayCol = dayToColumn (ShowSchedule.sswdDayOfWeek show')
      showUrl = showGetUrl (ShowSchedule.sswdShowSlug show')
      -- Check if this show is currently live (using helper that handles overnight)
      isLive = isShowLive currentDayOfWeek currentTimeOfDay (ShowSchedule.sswdDayOfWeek show') startTime endTime
      -- Styling
      gridStyle = [i|grid-row: #{startRow} / #{endRow}; grid-column: #{dayCol};|]

  Lucid.a_
    [ Lucid.href_ [i|/#{showUrl}|],
      hxGet_ [i|/#{showUrl}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ $ calendarShowStyles isLive,
      Lucid.style_ gridStyle
    ]
    $ do
      -- Live indicator
      if isLive
        then Lucid.div_ [Lucid.class_ liveIndicatorStyles] "● ON AIR ●"
        else mempty
      -- Show title
      Lucid.div_ [Lucid.class_ showTitleStyles] $
        Lucid.toHtml (ShowSchedule.sswdShowTitle show')
      -- Host name
      Lucid.div_ [Lucid.class_ showHostStyles] $
        Lucid.toHtml (ShowSchedule.sswdHostName show')
      -- Time range
      Lucid.div_ [Lucid.class_ showTimeStyles] $
        Lucid.toHtml $
          formatTimeOfDay (ShowSchedule.sswdStartTime show') <> " - " <> formatTimeOfDay (ShowSchedule.sswdEndTime show')

--------------------------------------------------------------------------------
-- Calendar Show Styles

calendarShowStyles :: Bool -> Text
calendarShowStyles isLive = styles $ do
  base [Tokens.bgWhite, Tokens.p2, "m-1", "rounded", "overflow-hidden", "block", "cursor-pointer"]
  if isLive
    then base [Tokens.border2, "border-gray-800"]
    else base ["border", "border-gray-400"]
  also ["hover:bg-gray-50"]

liveIndicatorStyles :: Text
liveIndicatorStyles = styles $ do
  base ["text-[10px]", Tokens.fontBold, "mb-1", Tokens.textGray800, "animate-pulse", "text-center"]

showTitleStyles :: Text
showTitleStyles = styles $ do
  base [Tokens.textXs, Tokens.fontBold, "mb-1"]

showHostStyles :: Text
showHostStyles = styles $ do
  base ["text-[10px]", Tokens.textGray600]

showTimeStyles :: Text
showTimeStyles = styles $ do
  base ["text-[10px]", "text-gray-500", "mt-1"]

-- | Render mobile schedule (day by day)
renderMobileSchedule :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> Lucid.Html ()
renderMobileSchedule scheduledShows currentDayOfWeek currentTimeOfDay = do
  Lucid.div_ [Lucid.class_ mobileContainerStyles] $ do
    -- Group shows by day
    let daysOfWeek = [(Monday, "MONDAY"), (Tuesday, "TUESDAY"), (Wednesday, "WEDNESDAY"), (Thursday, "THURSDAY"), (Friday, "FRIDAY"), (Saturday, "SATURDAY"), (Sunday, "SUNDAY")]
    mapM_ (renderMobileDay scheduledShows currentDayOfWeek currentTimeOfDay) daysOfWeek

    Lucid.div_ [Lucid.class_ "text-center"] $ do
      Lucid.p_ [Lucid.class_ mobileFooterStyles] "Showing current week's schedule"

--------------------------------------------------------------------------------
-- Mobile Schedule Styles

mobileContainerStyles :: Text
mobileContainerStyles = styles $ do
  base ["space-y-4"]
  desktop ["hidden"]

mobileFooterStyles :: Text
mobileFooterStyles = styles $ do
  base [Tokens.textGray600, "italic"]

mobileDayCardStyles :: Text
mobileDayCardStyles = styles $ do
  base [Tokens.bgWhite, Tokens.cardBorder]
  base [Tokens.p3]
  tablet [Tokens.p4]

mobileDayHeaderStyles :: Bool -> Text
mobileDayHeaderStyles isCurrentDay = styles $ do
  base [Tokens.fontBold, "mb-3", "text-center", Tokens.py2, "border"]
  if isCurrentDay
    then base [Tokens.bgGray800, Tokens.textWhite, "border-gray-800"]
    else base [Tokens.bgGray100, "border-gray-300"]

mobileShowItemStyles :: Text
mobileShowItemStyles = styles $ do
  base ["flex", "justify-between", "items-center", "border", "border-gray-300"]
  base [Tokens.p2]
  tablet [Tokens.p3]

mobileLiveBadgeStyles :: Text
mobileLiveBadgeStyles = styles $ do
  base [Tokens.fontBold, "uppercase", Tokens.textXs, "mb-1", Tokens.textGray800, "animate-pulse"]

-- | Render a single day for mobile view
renderMobileDay :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> (DayOfWeek, Text) -> Lucid.Html ()
renderMobileDay scheduledShows currentDayOfWeek currentTimeOfDay (dayOfWeek, dayName) = do
  let showsForDay = filter (\s -> ShowSchedule.sswdDayOfWeek s == dayOfWeek) scheduledShows
      sortedShows = sortBy (\a b -> compare (ShowSchedule.sswdStartTime a) (ShowSchedule.sswdStartTime b)) showsForDay
      isCurrentDay = Just dayOfWeek == currentDayOfWeek
      dayLabel = if isCurrentDay then dayName <> " - TODAY" else dayName

  unless (null sortedShows) $ do
    Lucid.div_ [Lucid.class_ mobileDayCardStyles] $ do
      Lucid.h3_ [Lucid.class_ $ mobileDayHeaderStyles isCurrentDay] $
        Lucid.toHtml dayLabel
      Lucid.div_ [Lucid.class_ "space-y-3"] $ do
        mapM_ (renderMobileShowItem currentDayOfWeek currentTimeOfDay dayOfWeek) sortedShows

-- | Render a single show item for mobile
renderMobileShowItem :: Maybe DayOfWeek -> Maybe TimeOfDay -> DayOfWeek -> ShowSchedule.ScheduledShowWithDetails -> Lucid.Html ()
renderMobileShowItem currentDayOfWeek currentTimeOfDay dayOfWeek show' = do
  let showUrl = showGetUrl (ShowSchedule.sswdShowSlug show')
      startTimeText = formatTimeOfDay (ShowSchedule.sswdStartTime show')
      -- Check if this show is currently airing (using helper that handles overnight shows)
      isLiveShow = isShowLive currentDayOfWeek currentTimeOfDay dayOfWeek (ShowSchedule.sswdStartTime show') (ShowSchedule.sswdEndTime show')
  Lucid.div_ [Lucid.class_ mobileShowItemStyles] $ do
    Lucid.div_ $ do
      -- Add "ON AIR" badge for live shows with blink animation
      if isLiveShow
        then Lucid.div_ [Lucid.class_ mobileLiveBadgeStyles] "● LIVE NOW"
        else mempty
      Lucid.div_ [Lucid.class_ Tokens.fontBold] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{showUrl}|],
            hxGet_ [i|/#{showUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "hover:underline"
          ]
          $ Lucid.toHtml (startTimeText <> " - " <> ShowSchedule.sswdShowTitle show')
      Lucid.div_ [Lucid.class_ mobileShowHostStyles] $
        Lucid.toHtml (ShowSchedule.sswdHostName show')

mobileShowHostStyles :: Text
mobileShowHostStyles = styles $ do
  base [Tokens.textGray600]
  base [Tokens.textXs]
  tablet [Tokens.textSm]
