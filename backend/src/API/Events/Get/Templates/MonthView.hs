{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Events.Get.Templates.MonthView
  ( renderMonthContent,
    renderCalendarDay,
    CalendarDay (..),
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventGetLink, eventsGetLink)
import Data.Foldable (traverse_)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (MonthOfYear, Year)
import Domain.Types.PageView (PageView (..))
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Calendar day data structure (re-exported from main module)
-- This is defined here temporarily for the template to use
-- The main module will use this definition
data CalendarDay = CalendarDay
  { cdDay :: Int,
    cdIsCurrentMonth :: Bool,
    cdEvents :: [Events.EventModel]
  }

--------------------------------------------------------------------------------

-- URL helpers
eventGetUrl :: Text -> Links.URI
eventGetUrl slug = Links.linkURI $ eventGetLink slug

eventsGetMonthUrl :: Year -> MonthOfYear -> Maybe Text -> Links.URI
eventsGetMonthUrl year month maybeTag =
  Links.linkURI $ eventsGetLink maybeTag (Just $ MonthView year month)

monthName :: MonthOfYear -> Text
monthName 1 = "JANUARY"
monthName 2 = "FEBRUARY"
monthName 3 = "MARCH"
monthName 4 = "APRIL"
monthName 5 = "MAY"
monthName 6 = "JUNE"
monthName 7 = "JULY"
monthName 8 = "AUGUST"
monthName 9 = "SEPTEMBER"
monthName 10 = "OCTOBER"
monthName 11 = "NOVEMBER"
monthName 12 = "DECEMBER"
monthName _ = "UNKNOWN"

--------------------------------------------------------------------------------

-- | Render just the month calendar content (for HTMX updates)
renderMonthContent ::
  Year ->
  MonthOfYear ->
  Maybe Text ->
  [Events.EventTagWithCount] ->
  [[CalendarDay]] ->
  Lucid.Html ()
renderMonthContent year month _maybeTagFilter _eventTagsWithCounts calendarGrid = do
  let (prevYear, prevMonth) = if month == 1 then (year - 1, 12) else (year, month - 1)
      (nextYear, nextMonth) = if month == 12 then (year + 1, 1) else (year, month + 1)

  -- Calendar Month View
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between mb-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold"] $ Lucid.toHtml $ monthName month <> " " <> Text.pack (show year)
      Lucid.div_ [Lucid.class_ "flex gap-2"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{eventsGetMonthUrl prevYear prevMonth Nothing}|],
            hxGet_ [i|/#{eventsGetMonthUrl prevYear prevMonth Nothing}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "px-3 py-1 text-gray-600 hover:text-gray-800"
          ]
          $ Lucid.toHtml
          $ "‹ " <> monthName prevMonth
        Lucid.a_
          [ Lucid.href_ [i|/#{eventsGetMonthUrl nextYear nextMonth Nothing}|],
            hxGet_ [i|/#{eventsGetMonthUrl nextYear nextMonth Nothing}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "px-3 py-1 text-gray-600 hover:text-gray-800"
          ]
          $ Lucid.toHtml
          $ monthName nextMonth <> " ›"

    -- Calendar Grid
    Lucid.div_ [Lucid.class_ "grid grid-cols-7 gap-1 text-sm min-h-96"] $ do
      -- Header Row
      traverse_
        (Lucid.div_ [Lucid.class_ "p-2 font-bold text-center bg-gray-200"])
        ["SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"]

      -- Calendar Days
      traverse_ (traverse_ renderCalendarDay) calendarGrid

-- | Render a calendar day cell
renderCalendarDay :: CalendarDay -> Lucid.Html ()
renderCalendarDay day = do
  let dayClasses =
        if cdIsCurrentMonth day
          then "p-2 h-20 border border-gray-200"
          else "p-2 h-20 border border-gray-200 text-gray-400"

  Lucid.div_ [Lucid.class_ dayClasses] $ do
    Lucid.div_ [Lucid.class_ "font-bold"] $ Lucid.toHtml $ Text.pack $ show $ cdDay day
    traverse_ renderEventInDay (take 1 $ cdEvents day) -- Only show first event
  where
    renderEventInDay :: Events.EventModel -> Lucid.Html ()
    renderEventInDay event =
      Lucid.div_ [Lucid.class_ "text-xs truncate mt-1"]
        $ Lucid.a_
          [ Lucid.href_ [i|/#{eventGetUrl (Events.emSlug event)}|],
            hxGet_ [i|/#{eventGetUrl (Events.emSlug event)}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "hover:underline"
          ]
        $ Lucid.toHtml event.emTitle
