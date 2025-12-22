{-# LANGUAGE QuasiQuotes #-}

module API.Schedule.Get.Templates.Components
  ( renderScheduleView,
    renderDesktopSchedule,
    renderMobileSchedule,
  )
where

import API.Links (scheduleLink, showsLinks)
import API.Types
import Data.List (sortBy)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, DayOfWeek (..), TimeOfDay (..), addDays, defaultTimeLocale, formatTime)
import Design (also, base, class_, desktop, unless, when)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.WeekOffset (WeekOffset (..))
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Lucid qualified
import Lucid.Base (Attributes)
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xData_, xOnClick_, xShow_)
import OrphanInstances.DayOfWeek (fromDayOfWeek)
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
  -- Desktop Schedule Grid (hidden on mobile) - includes week navigation
  renderDesktopSchedule scheduledShows currentDayOfWeek currentTimeOfDay weekOffset weekStart

  -- Mobile Schedule (hidden on desktop) - has its own day navigation
  renderMobileSchedule scheduledShows currentDayOfWeek currentTimeOfDay

-- | Week navigation component (desktop only)
renderWeekNavigation :: WeekOffset -> Day -> Lucid.Html ()
renderWeekNavigation (WeekOffset weekOffset) weekStart = do
  let weekEnd = addDays 6 weekStart
      weekLabel =
        if weekOffset == 0
          then "CURRENT WEEK"
          else formatDateRange weekStart weekEnd
      prevWeekUrl = scheduleWeekUrl (WeekOffset (weekOffset - 1))
      nextWeekUrl = scheduleWeekUrl (WeekOffset (weekOffset + 1))

  Lucid.div_ [weekNavContainerStyles] $ do
    Lucid.div_ [weekNavFlexStyles] $ do
      -- Previous week button
      Lucid.button_
        [ weekNavButtonStyles,
          hxGet_ [i|/#{prevWeekUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true"
        ]
        "← PREV"

      -- Week display
      Lucid.h2_ [weekLabelStyles] $ Lucid.toHtml weekLabel

      -- Next week button
      Lucid.button_
        [ weekNavButtonStyles,
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
-- Week Navigation Styles (desktop only)

weekNavContainerStyles :: Attributes
weekNavContainerStyles = class_ $ do
  base ["hidden"]
  desktop [Tokens.bgWhite, Tokens.cardBorder, Tokens.p4, Tokens.mb6, "block"]

weekNavFlexStyles :: Attributes
weekNavFlexStyles = class_ $ do
  base ["flex", "justify-between", "items-center", Tokens.gap4]

weekNavButtonStyles :: Attributes
weekNavButtonStyles = class_ $ do
  base [Tokens.bgGray800, Tokens.textWhite, Tokens.fontBold, Tokens.px4, Tokens.py2]
  also ["hover:bg-gray-700"]

weekLabelStyles :: Attributes
weekLabelStyles = class_ $ do
  base [Tokens.fontBold, "text-center", Tokens.textXl]

-- | Render desktop schedule grid (Google Calendar style with spanning shows)
renderDesktopSchedule :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> WeekOffset -> Day -> Lucid.Html ()
renderDesktopSchedule scheduledShows currentDayOfWeek currentTimeOfDay weekOffset weekStart = do
  -- Week navigation (desktop only)
  renderWeekNavigation weekOffset weekStart

  Lucid.div_ [desktopContainerStyles] $ do
    Lucid.div_ [desktopGridWrapperStyles] $ do
      -- Schedule Header (fixed row outside the grid)
      Lucid.div_ [desktopHeaderRowStyles] $ do
        Lucid.div_ [timeHeaderCellStyles] "TIME"
        renderDayHeader Monday currentDayOfWeek "MON"
        renderDayHeader Tuesday currentDayOfWeek "TUE"
        renderDayHeader Wednesday currentDayOfWeek "WED"
        renderDayHeader Thursday currentDayOfWeek "THU"
        renderDayHeader Friday currentDayOfWeek "FRI"
        renderDayHeader Saturday currentDayOfWeek "SAT"
        renderDayHeader Sunday currentDayOfWeek "SUN"

      -- Main calendar grid: 8 columns (time + 7 days) x 24 rows (hours)
      -- Using CSS grid with explicit row placement for spanning shows
      Lucid.div_ [desktopGridStyles, Lucid.style_ "grid-template-rows: repeat(24, minmax(60px, auto));"] $ do
        -- Render time labels in first column
        mapM_ renderTimeLabel [0 .. 23]

        -- Render day columns with background
        mapM_ (renderDayColumn currentDayOfWeek) [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

        -- Render shows as absolutely positioned elements that span rows
        mapM_ (renderCalendarShow currentDayOfWeek currentTimeOfDay) scheduledShows

--------------------------------------------------------------------------------
-- Desktop Schedule Styles

desktopContainerStyles :: Attributes
desktopContainerStyles = class_ $ do
  base ["hidden", "overflow-x-auto"]
  desktop ["block"]

desktopGridWrapperStyles :: Attributes
desktopGridWrapperStyles = class_ $ do
  base [Tokens.bgWhite, Tokens.cardBorder, "min-w-[800px]"]

desktopHeaderRowStyles :: Attributes
desktopHeaderRowStyles = class_ $ do
  base ["grid", "grid-cols-8", "border-b-2", "border-gray-800", "sticky", "top-0", Tokens.bgWhite, "z-10"]

desktopGridStyles :: Attributes
desktopGridStyles = class_ $ do
  base ["grid", "grid-cols-8", "relative"]

timeHeaderCellStyles :: Attributes
timeHeaderCellStyles = class_ $ do
  base [Tokens.p4, Tokens.fontBold, "text-center", "border-r", "border-gray-300"]

-- | Render day header with conditional highlighting
renderDayHeader :: DayOfWeek -> Maybe DayOfWeek -> Text -> Lucid.Html ()
renderDayHeader day currentDay dayName = do
  let isCurrentDay = Just day == currentDay
      headerStyles = dayHeaderStyles isCurrentDay (day /= Sunday)
  Lucid.div_ [headerStyles] $ Lucid.toHtml dayName

dayHeaderStyles :: Bool -> Bool -> Attributes
dayHeaderStyles isCurrentDay hasBorder = class_ $ do
  base [Tokens.p4, Tokens.fontBold, "text-center"]
  when isCurrentDay $ base [Tokens.bgGray800, Tokens.textWhite]
  when hasBorder $ base ["border-r", "border-gray-300"]

-- | Render time label for a specific hour
renderTimeLabel :: Int -> Lucid.Html ()
renderTimeLabel hour = do
  let timeText = formatHour hour
      rowStyle = [i|grid-row: #{hour + 1}; grid-column: 1;|]
  Lucid.div_
    [ timeLabelStyles,
      Lucid.style_ rowStyle
    ]
    $ Lucid.toHtml timeText

timeLabelStyles :: Attributes
timeLabelStyles = class_ $ do
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
      calendarShowStyles isLive,
      Lucid.style_ gridStyle
    ]
    $ do
      -- Live indicator
      if isLive
        then Lucid.div_ [liveIndicatorStyles] "● ON AIR ●"
        else mempty
      -- Show title
      Lucid.div_ [showTitleStyles] $
        Lucid.toHtml (ShowSchedule.sswdShowTitle show')
      -- Host name
      Lucid.div_ [showHostStyles] $
        Lucid.toHtml (ShowSchedule.sswdHostName show')
      -- Time range
      Lucid.div_ [showTimeStyles] $
        Lucid.toHtml $
          formatTimeOfDay (ShowSchedule.sswdStartTime show') <> " - " <> formatTimeOfDay (ShowSchedule.sswdEndTime show')

--------------------------------------------------------------------------------
-- Calendar Show Styles

calendarShowStyles :: Bool -> Attributes
calendarShowStyles isLive = class_ $ do
  base [Tokens.bgWhite, Tokens.p2, "m-1", "rounded", "overflow-hidden", "block", "cursor-pointer"]
  when isLive $ base [Tokens.border2, "border-gray-800"]
  unless isLive $ base ["border", "border-gray-400"]
  also ["hover:bg-gray-50"]

liveIndicatorStyles :: Attributes
liveIndicatorStyles = class_ $ do
  base ["text-[10px]", Tokens.fontBold, "mb-1", Tokens.textGray800, "animate-pulse", "text-center"]

showTitleStyles :: Attributes
showTitleStyles = class_ $ do
  base [Tokens.textXs, Tokens.fontBold, "mb-1"]

showHostStyles :: Attributes
showHostStyles = class_ $ do
  base ["text-[10px]", Tokens.textGray600]

showTimeStyles :: Attributes
showTimeStyles = class_ $ do
  base ["text-[10px]", "text-gray-500", "mt-1"]

-- | Render mobile schedule (single day view with day navigation)
renderMobileSchedule :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> Lucid.Html ()
renderMobileSchedule scheduledShows currentDayOfWeek currentTimeOfDay = do
  let daysOfWeek = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
      -- Default to current day, or Monday if not viewing current week
      initialDayIndex :: Int
      initialDayIndex = maybe 0 (fromIntegral . fromDayOfWeek) currentDayOfWeek
      alpineData = [i|{ currentDay: #{initialDayIndex} }|]

  Lucid.div_ [mobileContainerStyles, xData_ alpineData] $ do
    -- Mobile header row: "Schedule" on left, day nav on right
    Lucid.div_ [mobileHeaderRowStyles] $ do
      -- Title on left
      Lucid.h1_ [mobileHeaderTitleStyles] "Schedule"

      -- Day navigation on right
      Lucid.div_ [mobileDayNavStyles] $ do
        -- Previous day button
        Lucid.button_
          [ mobileDayNavButtonStyles,
            xOnClick_ "currentDay = currentDay > 0 ? currentDay - 1 : 6"
          ]
          "←"

        -- Current day label
        Lucid.span_ [mobileDayLabelStyles] $ do
          mapM_ (renderDayLabel currentDayOfWeek) (zip [0 ..] daysOfWeek)

        -- Next day button
        Lucid.button_
          [ mobileDayNavButtonStyles,
            xOnClick_ "currentDay = currentDay < 6 ? currentDay + 1 : 0"
          ]
          "→"

    -- Day content panels (one per day, shown/hidden by Alpine)
    mapM_ (renderMobileDayPanel scheduledShows currentDayOfWeek currentTimeOfDay) (zip [0 ..] daysOfWeek)

--------------------------------------------------------------------------------
-- Mobile Schedule Styles

mobileContainerStyles :: Attributes
mobileContainerStyles = class_ $ do
  base ["space-y-4"]
  desktop ["hidden"]

mobileHeaderRowStyles :: Attributes
mobileHeaderRowStyles = class_ $ do
  base ["flex", "justify-between", "items-center", Tokens.mb4]

mobileHeaderTitleStyles :: Attributes
mobileHeaderTitleStyles = class_ $ do
  base [Tokens.text2xl, Tokens.fontBold]

mobileDayNavStyles :: Attributes
mobileDayNavStyles = class_ $ do
  base ["flex", "items-center"]

mobileDayNavButtonStyles :: Attributes
mobileDayNavButtonStyles = class_ $ do
  base [Tokens.textXl, "font-black", Tokens.px3, Tokens.py2]
  also ["hover:text-gray-600", "cursor-pointer"]

mobileDayLabelStyles :: Attributes
mobileDayLabelStyles = class_ $ do
  base [Tokens.fontBold, Tokens.textLg]

-- | Render a day label that shows/hides based on Alpine state
renderDayLabel :: Maybe DayOfWeek -> (Int, DayOfWeek) -> Lucid.Html ()
renderDayLabel currentDayOfWeek (idx, day) = do
  let isToday = Just day == currentDayOfWeek
      dayText = if isToday then "Today" else dayOfWeekName day
      showCondition = [i|currentDay === #{idx :: Int}|]
  Lucid.span_ [xShow_ showCondition] $ Lucid.toHtml dayText

-- | Get display name for day of week
dayOfWeekName :: DayOfWeek -> Text
dayOfWeekName Monday = "Monday"
dayOfWeekName Tuesday = "Tuesday"
dayOfWeekName Wednesday = "Wednesday"
dayOfWeekName Thursday = "Thursday"
dayOfWeekName Friday = "Friday"
dayOfWeekName Saturday = "Saturday"
dayOfWeekName Sunday = "Sunday"

-- | Render a day panel (shown/hidden by Alpine based on currentDay)
renderMobileDayPanel :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> (Int, DayOfWeek) -> Lucid.Html ()
renderMobileDayPanel scheduledShows currentDayOfWeek currentTimeOfDay (idx, dayOfWeek) = do
  let showsForDay = filter (\s -> ShowSchedule.sswdDayOfWeek s == dayOfWeek) scheduledShows
      sortedShows = sortBy (\a b -> compare (ShowSchedule.sswdStartTime a) (ShowSchedule.sswdStartTime b)) showsForDay
      showCondition = [i|currentDay === #{idx :: Int}|]

  Lucid.div_ [xShow_ showCondition, Lucid.class_ "space-y-3"] $ do
    if null sortedShows
      then Lucid.p_ [mobileEmptyStyles] "No shows scheduled"
      else mapM_ (renderMobileShowCard currentDayOfWeek currentTimeOfDay dayOfWeek) sortedShows

mobileEmptyStyles :: Attributes
mobileEmptyStyles = class_ $ do
  base [Tokens.textGray600, "text-center", Tokens.py4, "italic"]

-- | Render a single show card for mobile (wireframe style)
renderMobileShowCard :: Maybe DayOfWeek -> Maybe TimeOfDay -> DayOfWeek -> ShowSchedule.ScheduledShowWithDetails -> Lucid.Html ()
renderMobileShowCard currentDayOfWeek currentTimeOfDay dayOfWeek show' = do
  let showUrl = showGetUrl (ShowSchedule.sswdShowSlug show')
      startTimeText = formatTimeOfDay (ShowSchedule.sswdStartTime show')
      isLiveShow = isShowLive currentDayOfWeek currentTimeOfDay dayOfWeek (ShowSchedule.sswdStartTime show') (ShowSchedule.sswdEndTime show')
      -- Calculate show duration in minutes for dynamic height
      durationMins = calculateDurationMinutes (ShowSchedule.sswdStartTime show') (ShowSchedule.sswdEndTime show')
      -- Scale: 1.5px per minute (1 hour = 90px, 2 hours = 180px)
      cardHeight = (durationMins * 3) `div` 2
      heightStyle = [i|min-height: #{cardHeight}px|]

  Lucid.div_ [mobileShowRowStyles] $ do
    -- Time label on left (with LIVE indicator below if applicable)
    Lucid.div_ [mobileTimeContainerStyles] $ do
      Lucid.div_ [mobileTimeStyles] $ Lucid.toHtml startTimeText
      if isLiveShow
        then Lucid.div_ [mobileLiveBadgeStyles] "*ON AIR*"
        else mempty

    -- Show card on right (with dynamic height based on duration)
    Lucid.a_
      [ Lucid.href_ [i|/#{showUrl}|],
        hxGet_ [i|/#{showUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        mobileShowCardStyles isLiveShow,
        Lucid.style_ heightStyle
      ]
      $ do
        -- Show title
        Lucid.div_ [mobileShowTitleStyles] $ Lucid.toHtml (ShowSchedule.sswdShowTitle show')

-- | Calculate duration in minutes between two times (handles overnight shows)
calculateDurationMinutes :: TimeOfDay -> TimeOfDay -> Int
calculateDurationMinutes (TimeOfDay startH startM _) (TimeOfDay endH endM _) =
  let startMins = startH * 60 + startM
      endMins = endH * 60 + endM
      -- Handle overnight shows (e.g., 11PM to 1AM)
      duration =
        if endMins <= startMins
          then (24 * 60 - startMins) + endMins
          else endMins - startMins
   in duration

mobileShowRowStyles :: Attributes
mobileShowRowStyles = class_ $ do
  base ["flex", "items-start", "gap-3"]

mobileTimeContainerStyles :: Attributes
mobileTimeContainerStyles = class_ $ do
  base ["w-20", "flex-shrink-0", "pt-3"]

mobileTimeStyles :: Attributes
mobileTimeStyles = class_ $ do
  base [Tokens.textSm, Tokens.textGray600]

mobileShowCardStyles :: Bool -> Attributes
mobileShowCardStyles isLive = class_ $ do
  base [Tokens.bgWhite, "rounded-xl", Tokens.p3, "flex-grow", "block"]
  base ["border", "border-gray-300"]
  when isLive $ base [Tokens.border2, "border-gray-800"]
  also ["hover:bg-gray-50"]

mobileShowTitleStyles :: Attributes
mobileShowTitleStyles = class_ $ do
  base [Tokens.fontBold, Tokens.mb2]

mobileLiveBadgeStyles :: Attributes
mobileLiveBadgeStyles = class_ $ do
  base [Tokens.fontBold, Tokens.textXs, Tokens.textGray800, "animate-pulse"]
