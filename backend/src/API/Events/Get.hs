{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Events.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventGetLink, eventsGetLink)
import App.Auth qualified as Auth
import Component.Frame (UserInfo (..), loadFrame, loadFrameWithUser)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isNothing)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (MonthOfYear, UTCTime (..), Year, addDays, fromGregorian, toGregorian, utctDay)
import Data.Time.Calendar (gregorianMonthLength)
import Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.PageView (PageView (..), isMonthView, isWeekView)
import Effects.Clock (MonadClock, currentSystemTime)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxSwap_, hxTarget_, hxTrigger_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

eventsGetUrl :: Links.URI
eventsGetUrl = Links.linkURI $ eventsGetLink Nothing Nothing

eventGetUrl :: Text -> Links.URI
eventGetUrl slug = Links.linkURI $ eventGetLink slug

eventsGetTypeUrl :: Text -> Links.URI
eventsGetTypeUrl eventType = Links.linkURI $ eventsGetLink (Just eventType) Nothing

eventsGetMonthUrl :: Year -> MonthOfYear -> Maybe Text -> Links.URI
eventsGetMonthUrl year month maybeTag =
  Links.linkURI $ eventsGetLink maybeTag (Just $ MonthView year month)

eventsGetWeekUrl :: Year -> Int -> Maybe Text -> Links.URI
eventsGetWeekUrl year weekNum maybeTag =
  Links.linkURI $ eventsGetLink maybeTag (Just $ WeekView year weekNum)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /events"
    ( "events"
        :> Servant.QueryParam "tag" Text
        :> Servant.QueryParam "view" PageView
        :> Servant.Header "Cookie" Text
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

data HxRequest = IsHxRequest | IsNotHxRequest
  deriving (Show)

instance Servant.FromHttpApiData HxRequest where
  parseQueryParam = \case
    "true" -> Right IsHxRequest
    _ -> Right IsNotHxRequest

--------------------------------------------------------------------------------

-- | Main events template for list view
header :: UTCTime -> PageView -> Maybe Text -> (Year, MonthOfYear) -> [Events.EventTagWithCount] -> Lucid.Html ()
header currentTime pageView maybeTagFilter currentMonth eventTagsWithCounts = do
  -- Events Header
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 text-center w-full"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4"] "COMMUNITY EVENTS"
    Lucid.p_ [Lucid.class_ "text-lg text-gray-600 mb-6"] "Connect with the KPBJ community through live shows, fundraisers, and gatherings"

  -- View Controls & Filters
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8 w-full"] $ do
    renderViewControls currentTime pageView currentMonth maybeTagFilter eventTagsWithCounts

-- | Render the events listing content
renderListContent :: [Events.EventModel] -> Lucid.Html ()
renderListContent events = do
  Lucid.div_ [Lucid.class_ "space-y-8"] $ do
    Lucid.section_ [Lucid.class_ "space-y-6"] $ do
      if null events
        then Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "No Events Scheduled"
          Lucid.p_ [Lucid.class_ "text-gray-600"] "Check back soon for upcoming community events!"
        else traverse_ renderEventCard events

-- | Render an event card for the list view
renderEventCard :: Events.EventModel -> Lucid.Html ()
renderEventCard event = do
  Lucid.article_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-6"] $ do
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-4 gap-6"] $ do
      -- Event image placeholder and type badge
      Lucid.div_ [Lucid.class_ "lg:col-span-1"] $ do
        Lucid.div_
          [Lucid.class_ "w-full aspect-square bg-gray-300 border-2 border-gray-600 flex items-center justify-center text-lg mb-4"]
          "[EVENT IMAGE]"
        Lucid.div_ [Lucid.class_ "text-center"] $ do
          Lucid.div_
            [Lucid.class_ "bg-gray-200 text-gray-800 px-2 py-1 text-xs font-bold"]
            "EVENT"

      -- Event details
      Lucid.div_ [Lucid.class_ "lg:col-span-3"] $ do
        -- Title
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-3"] $ do
          Lucid.a_
            [ Lucid.href_ [i|/#{eventGetUrl (Events.emSlug event)}|],
              hxGet_ [i|/#{eventGetUrl (Events.emSlug event)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "hover:underline"
            ]
            $ Lucid.toHtml event.emTitle

        -- Date and location info
        Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 gap-4 text-sm mb-4"] $ do
          Lucid.div_ $ do
            Lucid.div_ [Lucid.class_ "font-bold text-gray-800"] "📅 DATE & TIME"
            Lucid.div_ [Lucid.class_ "text-gray-600"] $
              Lucid.toHtml $
                formatTime defaultTimeLocale "%A, %B %d, %Y" event.emStartsAt
            Lucid.div_ [Lucid.class_ "text-gray-600"] $
              Lucid.toHtml $
                formatTime defaultTimeLocale "%l:%M %p" event.emStartsAt
                  <> " - "
                  <> formatTime defaultTimeLocale "%l:%M %p" event.emEndsAt
          Lucid.div_ $ do
            Lucid.div_ [Lucid.class_ "font-bold text-gray-800"] "📍 LOCATION"
            Lucid.div_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml event.emLocationName
            Lucid.div_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml event.emLocationAddress

        -- Description preview
        Lucid.p_ [Lucid.class_ "text-gray-700 mb-4 leading-relaxed"] $ do
          let truncatedDescription = Text.take 200 event.emDescription
          Lucid.toHtml $
            truncatedDescription <> if Text.length event.emDescription > 200 then "..." else ""

        -- View Event button
        Lucid.div_ [Lucid.class_ "flex items-center gap-4"] $ do
          Lucid.a_
            [ Lucid.href_ [i|/#{eventGetUrl (Events.emSlug event)}|],
              hxGet_ [i|/#{eventGetUrl (Events.emSlug event)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "bg-gray-800 text-white px-6 py-2 font-bold hover:bg-gray-700"
            ]
            "VIEW EVENT"

-- | Render just the month calendar content (for HTMX updates)
renderMonthContent :: Year -> MonthOfYear -> Maybe Text -> [Events.EventTagWithCount] -> [Events.EventModel] -> Lucid.Html ()
renderMonthContent year month _maybeTagFilter _eventTagsWithCounts events = do
  let calendarGrid = generateCalendarGrid year month events
      (prevYear, prevMonth) = if month == 1 then (year - 1, 12) else (year, month - 1)
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

-- | Render the week view content (for HTMX updates)
renderWeekContent :: Year -> Int -> Maybe Text -> [Events.EventTagWithCount] -> [Events.EventModel] -> Lucid.Html ()
renderWeekContent year weekNum _maybeTagFilter _eventTagsWithCounts events = do
  let weekGrid = generateWeekGrid year weekNum events
      ((prevYear, prevWeek), (nextYear, nextWeek)) = weekNavigation year weekNum
      startDate = weekStartDate year weekNum
      endDate = weekEndDate year weekNum
      weekTitle = formatWeekTitle startDate endDate

  -- Week Calendar View
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between mb-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold"] $ Lucid.toHtml weekTitle
      Lucid.div_ [Lucid.class_ "flex gap-2"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{eventsGetWeekUrl prevYear prevWeek Nothing}|],
            hxGet_ [i|/#{eventsGetWeekUrl prevYear prevWeek Nothing}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "px-3 py-1 text-gray-600 hover:text-gray-800"
          ]
          "‹ Previous Week"
        Lucid.a_
          [ Lucid.href_ [i|/#{eventsGetWeekUrl nextYear nextWeek Nothing}|],
            hxGet_ [i|/#{eventsGetWeekUrl nextYear nextWeek Nothing}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "px-3 py-1 text-gray-600 hover:text-gray-800"
          ]
          "Next Week ›"

    -- Week Grid
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-7 gap-4"] $ do
      traverse_ renderWeekDay weekGrid

-- | Render a single day in the week view
renderWeekDay :: WeekDay -> Lucid.Html ()
renderWeekDay day = do
  Lucid.div_ [Lucid.class_ "border border-gray-200 p-3"] $ do
    Lucid.div_ [Lucid.class_ "font-bold mb-2 text-center"] $
      Lucid.toHtml $
        wdDayName day <> " " <> Text.pack (show $ wdDayNumber day)

    if null (wdEvents day)
      then Lucid.div_ [Lucid.class_ "text-xs text-gray-400 text-center mt-8"] "No events"
      else Lucid.div_ [Lucid.class_ "space-y-2"] $ do
        traverse_ renderWeekEvent (wdEvents day)

-- | Render an event within a week day
renderWeekEvent :: Events.EventModel -> Lucid.Html ()
renderWeekEvent event = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 text-yellow-800 p-2 text-xs"] $ do
    Lucid.div_ [Lucid.class_ "font-bold"] $
      Lucid.toHtml $
        formatTime defaultTimeLocale "%l %p" (Events.emStartsAt event)
    Lucid.div_ [Lucid.class_ "truncate"]
      $ Lucid.a_
        [ Lucid.href_ [i|/#{eventGetUrl (Events.emSlug event)}|],
          hxGet_ [i|/#{eventGetUrl (Events.emSlug event)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "hover:underline"
        ]
      $ Lucid.toHtml (Events.emTitle event)

-- | Shared view controls component
renderViewControls :: UTCTime -> PageView -> (Year, MonthOfYear) -> Maybe Text -> [Events.EventTagWithCount] -> Lucid.Html ()
renderViewControls currentTime currentView currentMonth maybeTagFilter eventTagsWithCounts = do
  let currentWeek = utcTimeToYearWeek currentTime
  Lucid.div_ [Lucid.class_ "flex flex-col md:flex-row md:items-center md:justify-between gap-4"] $ do
    -- View Toggle
    Lucid.div_ [Lucid.class_ "flex items-center gap-4"] $ do
      Lucid.span_ [Lucid.class_ "font-bold text-sm"] "VIEW:"
      Lucid.div_ [Lucid.class_ "flex border-2 border-gray-800"] $ do
        let listClasses =
              if currentView == ListView
                then "px-4 py-2 bg-gray-800 text-white font-bold"
                else "px-4 py-2 bg-white text-gray-800 font-bold hover:bg-gray-100"
            monthClasses =
              if isMonthView currentView
                then "px-4 py-2 bg-gray-800 text-white font-bold"
                else "px-4 py-2 bg-white text-gray-800 font-bold hover:bg-gray-100"
            weekClasses =
              if isWeekView currentView
                then "px-4 py-2 bg-gray-800 text-white font-bold"
                else "px-4 py-2 bg-white text-gray-800 font-bold hover:bg-gray-100"
        Lucid.a_
          [ Lucid.id_ "view-control",
            Lucid.href_ [i|/#{Links.linkURI $ eventsGetLink maybeTagFilter (Just ListView)}|],
            hxGet_ [i|/#{Links.linkURI $ eventsGetLink maybeTagFilter (Just ListView)}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ listClasses
          ]
          "LIST"
        -- For month view, we'll use current month
        Lucid.a_
          [ Lucid.id_ "view-control",
            Lucid.href_ [i|/#{Links.linkURI $ eventsGetLink maybeTagFilter (Just (uncurry MonthView currentMonth))}|],
            hxGet_ [i|/#{Links.linkURI $ eventsGetLink maybeTagFilter (Just (uncurry MonthView currentMonth))}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ monthClasses
          ]
          "MONTH"
        -- For week view, we'll use current week
        Lucid.a_
          [ Lucid.id_ "view-control",
            Lucid.href_ [i|/#{eventsGetWeekUrl (fst currentWeek) (snd currentWeek) maybeTagFilter}|],
            hxGet_ [i|/#{eventsGetWeekUrl (fst currentWeek) (snd currentWeek) maybeTagFilter}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ weekClasses
          ]
          "WEEK"

    -- Filters
    Lucid.div_ [Lucid.class_ "flex flex-wrap items-center gap-4"] $ do
      Lucid.span_ [Lucid.class_ "font-bold text-sm"] "FILTER:"
      -- Event tag filter
      Lucid.select_
        [ Lucid.class_ "border-2 border-gray-600 p-2 font-mono text-sm",
          Lucid.name_ "tag",
          hxGet_ [i|/#{Links.linkURI $ eventsGetLink Nothing (Just currentView)}|],
          hxTarget_ "#events-content-container",
          hxSwap_ "innerHTML",
          hxTrigger_ "change"
        ]
        $ do
          Lucid.option_ ([Lucid.value_ ""] <> ([Lucid.selected_ "" | isNothing maybeTagFilter])) "All Events"
          traverse_ renderEventTagOption eventTagsWithCounts
  where
    renderEventTagOption :: Events.EventTagWithCount -> Lucid.Html ()
    renderEventTagOption tagWithCount =
      Lucid.option_
        ( [Lucid.value_ tagWithCount.etwcTag]
            <> ([Lucid.selected_ "" | Just tagWithCount.etwcTag == maybeTagFilter])
        )
        $ Lucid.toHtml
        $ tagWithCount.etwcTag
          <> " ("
          <> Text.pack (show tagWithCount.etwcCount)
          <> ")"

-- | Template for general error
errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Error"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] $ Lucid.toHtml errorMsg
    Lucid.a_
      [ Lucid.href_ [i|/#{eventsGetUrl}|],
        hxGet_ [i|/#{eventsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
      ]
      "REFRESH"

--------------------------------------------------------------------------------
-- Calendar Logic

data CalendarDay = CalendarDay
  { cdDay :: Int,
    cdIsCurrentMonth :: Bool,
    cdEvents :: [Events.EventModel]
  }

data WeekDay = WeekDay
  { wdDayName :: Text,
    wdDayNumber :: Int,
    wdDate :: UTCTime,
    wdEvents :: [Events.EventModel]
  }

-- | Generate calendar grid for a specific month/year
generateCalendarGrid :: Year -> MonthOfYear -> [Events.EventModel] -> [[CalendarDay]]
generateCalendarGrid year month events =
  let -- First day of the month
      firstDay = fromGregorian year month 1
      -- Number of days in the month
      daysInMonth = gregorianMonthLength year month
      -- What day of the week does the month start (0=Sunday, 6=Saturday)
      (_, _, startDayOfWeek) = toWeekDate firstDay
      sundayStartOffset = if startDayOfWeek == 7 then 0 else startDayOfWeek

      -- Previous month info for padding
      (prevYear, prevMonth) = if month == 1 then (year - 1, 12) else (year, month - 1)
      daysInPrevMonth = gregorianMonthLength prevYear prevMonth

      -- Generate days
      prevMonthDays = [CalendarDay (daysInPrevMonth - sundayStartOffset + j + 1) False [] | j <- [0 .. (sundayStartOffset - 1)]]
      currentMonthDays = [CalendarDay d True (eventsForDay d) | d <- [1 .. daysInMonth]]

      -- Calculate how many next month days we need (to fill out 6 weeks = 42 days)
      totalDaysShown = length prevMonthDays + length currentMonthDays
      nextMonthDaysNeeded = 42 - totalDaysShown
      nextMonthDays = [CalendarDay d False [] | d <- [1 .. nextMonthDaysNeeded]]

      allDays = prevMonthDays ++ currentMonthDays ++ nextMonthDays
   in chunksOf 7 allDays
  where
    eventsForDay :: Int -> [Events.EventModel]
    eventsForDay dayNum =
      filter
        ( \e ->
            let (_, _, eventDay) = toGregorian (utctDay e.emStartsAt)
             in fromIntegral eventDay == dayNum
        )
        events

    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

monthName :: MonthOfYear -> Text
monthName m = Text.toUpper $ Text.pack $ formatTime defaultTimeLocale "%B" (fromGregorian 0 m 1)

-- | Generate week grid for a specific year/week
generateWeekGrid :: Year -> Int -> [Events.EventModel] -> [WeekDay]
generateWeekGrid year weekNum events =
  let startDate = weekStartDate year weekNum
      days = [0 .. 6]
   in map (createWeekDay events startDate) days
  where
    createWeekDay :: [Events.EventModel] -> UTCTime -> Int -> WeekDay
    createWeekDay evts startUTC dayOffset =
      let dayDate = addDays (fromIntegral dayOffset) (utctDay startUTC)
          dayUTC = UTCTime dayDate 0
          (_, _, dayNum) = toGregorian dayDate
          dayName = case dayOffset of
            0 -> "SUN"
            1 -> "MON"
            2 -> "TUE"
            3 -> "WED"
            4 -> "THU"
            5 -> "FRI"
            6 -> "SAT"
            _ -> "UNK"
          dayEvents = filter (eventOnDay dayUTC) evts
       in WeekDay dayName dayNum dayUTC dayEvents

    eventOnDay :: UTCTime -> Events.EventModel -> Bool
    eventOnDay targetDay event =
      let eventDay = utctDay (Events.emStartsAt event)
          targetDayOnly = utctDay targetDay
       in eventDay == targetDayOnly

formatWeekTitle :: UTCTime -> UTCTime -> Text
formatWeekTitle startDate endDate =
  let startFormatted = Text.toUpper $ Text.pack $ formatTime defaultTimeLocale "%B %d" startDate
      endFormatted = Text.pack $ formatTime defaultTimeLocale "%d, %Y" endDate
   in "WEEK OF " <> startFormatted <> " - " <> endFormatted

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env,
    MonadClock m
  ) =>
  Tracer ->
  -- | Tag Query Param
  Maybe Text ->
  -- | Page View Query Param
  Maybe PageView ->
  -- | Cookie
  Maybe Text ->
  -- | @hx-request@ header
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer (normalizeTagFilter -> tagFilter) (fromMaybe ListView -> view) cookie hxRequest = do
  getUserInfo cookie $ \mUserInfo -> do
    now <- currentSystemTime
    eventTagsWithCounts <- getAllEventTags

    let limit = 50
        offset = 0

    template <-
      case view of
        MonthView year month -> do
          monthEvents <- execQuerySpan (Events.getEventsForMonth tagFilter year month)
          renderMonthTemplate now year month tagFilter eventTagsWithCounts monthEvents
        WeekView year weekNum -> do
          weekEvents <- execQuerySpan (Events.getEventsForWeek tagFilter year weekNum)
          renderWeekTemplate now year weekNum tagFilter eventTagsWithCounts weekEvents
        _ -> do
          events <- execQuerySpan (Events.getPublishedEvents tagFilter limit offset)
          renderListTemplate now (utcTimeToYearMonth now) tagFilter eventTagsWithCounts events

    case hxRequest of
      Just IsHxRequest ->
        pure template
      _ ->
        case mUserInfo of
          Just userInfo ->
            loadFrameWithUser userInfo template
          Nothing ->
            loadFrame template

utcTimeToYearMonth :: UTCTime -> (Year, MonthOfYear)
utcTimeToYearMonth = (\(y, m, _) -> (y, m)) . toGregorian . utctDay

utcTimeToYearWeek :: UTCTime -> (Year, Int)
utcTimeToYearWeek utc =
  let (year, weekNum, _) = toWeekDate (utctDay utc)
   in (year, weekNum)

weekStartDate :: Year -> Int -> UTCTime
weekStartDate year weekNum =
  let monday = fromWeekDate year weekNum 1
   in UTCTime monday 0

weekEndDate :: Year -> Int -> UTCTime
weekEndDate year weekNum =
  let sunday = fromWeekDate year weekNum 7
   in UTCTime sunday (24 * 60 * 60 - 1)

weekNavigation :: Year -> Int -> ((Year, Int), (Year, Int))
weekNavigation year weekNum =
  let (prevYear, prevWeek) =
        if weekNum == 1
          then (year - 1, 53)
          else (year, weekNum - 1)
      (nextYear, nextWeek) =
        if weekNum >= 52
          then (year + 1, 1)
          else (year, weekNum + 1)
   in ((prevYear, prevWeek), (nextYear, nextWeek))

normalizeTagFilter :: Maybe Text -> Maybe Text
normalizeTagFilter = \case
  Just "" -> Nothing
  other -> other

getUserInfo ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m
  ) =>
  Maybe Text ->
  (Maybe UserInfo -> m a) ->
  m a
getUserInfo cookie k =
  Auth.userLoginState cookie >>= \case
    Auth.IsNotLoggedIn ->
      k Nothing
    Auth.IsLoggedIn user ->
      execQuerySpan (UserMetadata.getUserMetadata user.mId) >>= \case
        Right (Just userMetadata) ->
          k $ Just $ UserInfo {userDisplayName = userMetadata.mDisplayName}
        _ ->
          k Nothing

getAllEventTags ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m
  ) =>
  m [Events.EventTagWithCount]
getAllEventTags = do
  tags <- execQuerySpan Events.getEventTagsWithCounts
  pure $ fromRight [] tags

renderTemplate ::
  ( MonadCatch m,
    MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env
  ) =>
  UTCTime ->
  Int64 ->
  Int64 ->
  Maybe Text ->
  [Events.EventTagWithCount] ->
  Maybe PageView ->
  m (Lucid.Html ())
renderTemplate now limit offset tagFilter eventTagsWithCounts = \case
  Just (MonthView year month) -> do
    monthEvents <- execQuerySpan (Events.getEventsForMonth tagFilter year month)
    renderMonthTemplate now year month tagFilter eventTagsWithCounts monthEvents
  Just (WeekView year weekNum) -> do
    weekEvents <- execQuerySpan (Events.getEventsForWeek tagFilter year weekNum)
    renderWeekTemplate now year weekNum tagFilter eventTagsWithCounts weekEvents
  _ -> do
    events <- execQuerySpan (Events.getPublishedEvents tagFilter limit offset)
    renderListTemplate now (utcTimeToYearMonth now) tagFilter eventTagsWithCounts events

renderListTemplate ::
  ( Log.MonadLog m,
    Show err,
    MonadCatch m
  ) =>
  UTCTime ->
  (Year, MonthOfYear) ->
  Maybe Text ->
  [Events.EventTagWithCount] ->
  Either err [Events.EventModel] ->
  m (Lucid.Html ())
renderListTemplate currentTime currentMonth maybeTagFilter eventTagsWithCounts = \case
  Left err -> do
    Log.logAttention "Failed to fetch events from database" (Aeson.object ["error" .= show err])
    pure (errorTemplate "Failed to load events. Please try again.")
  Right events -> pure $ do
    header currentTime ListView maybeTagFilter currentMonth eventTagsWithCounts
    Lucid.section_ [Lucid.id_ "events-content-container", Lucid.class_ "w-full"] $ do
      renderListContent events

renderMonthTemplate ::
  ( Log.MonadLog m,
    Show err,
    MonadCatch m
  ) =>
  UTCTime ->
  Year ->
  MonthOfYear ->
  Maybe Text ->
  [Events.EventTagWithCount] ->
  Either err [Events.EventModel] ->
  m (Lucid.Html ())
renderMonthTemplate currentTime year month maybeTagFilter eventTagsWithCounts = \case
  Left err -> do
    Log.logAttention "Failed to fetch events from database" (Aeson.object ["error" .= show err])
    pure (errorTemplate "Failed to load events. Please try again.")
  Right events -> pure $ do
    header currentTime (MonthView year month) maybeTagFilter (year, month) eventTagsWithCounts
    Lucid.section_ [Lucid.id_ "events-content-container", Lucid.class_ "w-full"] $ do
      renderMonthContent year month maybeTagFilter eventTagsWithCounts events

renderWeekTemplate ::
  ( Log.MonadLog m,
    Show err,
    MonadCatch m
  ) =>
  UTCTime ->
  Year ->
  Int ->
  Maybe Text ->
  [Events.EventTagWithCount] ->
  Either err [Events.EventModel] ->
  m (Lucid.Html ())
renderWeekTemplate currentTime year weekNum maybeTagFilter eventTagsWithCounts = \case
  Left err -> do
    Log.logAttention "Failed to fetch events from database" (Aeson.object ["error" .= show err])
    pure (errorTemplate "Failed to load events. Please try again.")
  Right events -> do
    let currentWeek = (year, fromIntegral weekNum)
    pure $ do
      header currentTime (WeekView year weekNum) maybeTagFilter currentWeek eventTagsWithCounts
      Lucid.section_ [Lucid.id_ "events-content-container", Lucid.class_ "w-full"] $ do
        renderWeekContent year weekNum maybeTagFilter eventTagsWithCounts events
