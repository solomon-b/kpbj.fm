{-# LANGUAGE ViewPatterns #-}

module API.Events.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventGetLink, eventsGetLink)
import API.Events.Get.Templates.Error (errorTemplate)
import API.Events.Get.Templates.List (renderListContent)
import API.Events.Get.Templates.MonthView (CalendarDay (..), renderMonthContent)
import API.Events.Get.Templates.Page (header)
import API.Events.Get.Templates.WeekView (WeekDay (..), renderWeekContent)
import App.Common (getUserInfo)
import Component.Frame (loadFrame, loadFrameWithUser)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (MonthOfYear, UTCTime (..), Year, addDays, fromGregorian, toGregorian, utctDay)
import Data.Time.Calendar (gregorianMonthLength)
import Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..))
import Domain.Types.PageView (PageView (..))
import Effects.Clock (MonadClock, currentSystemTime)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.EventTags qualified as EventTag
import Effects.Database.Tables.Events qualified as Events
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
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
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------
-- Calendar Logic
-- Data structures and generation algorithms stay here (business logic)

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
  Maybe Cookie ->
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
          Just (_user, userMetadata) ->
            loadFrameWithUser userMetadata template
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

getAllEventTags ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m
  ) =>
  m [EventTag.EventTagWithCount]
getAllEventTags = do
  tags <- execQuerySpan EventTag.getEventTagsWithCounts
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
  [EventTag.EventTagWithCount] ->
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
  [EventTag.EventTagWithCount] ->
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
  [EventTag.EventTagWithCount] ->
  Either err [Events.EventModel] ->
  m (Lucid.Html ())
renderMonthTemplate currentTime year month maybeTagFilter eventTagsWithCounts = \case
  Left err -> do
    Log.logAttention "Failed to fetch events from database" (Aeson.object ["error" .= show err])
    pure (errorTemplate "Failed to load events. Please try again.")
  Right events -> pure $ do
    let calendarGrid = generateCalendarGrid year month events
    header currentTime (MonthView year month) maybeTagFilter (year, month) eventTagsWithCounts
    Lucid.section_ [Lucid.id_ "events-content-container", Lucid.class_ "w-full"] $ do
      renderMonthContent year month maybeTagFilter eventTagsWithCounts calendarGrid

renderWeekTemplate ::
  ( Log.MonadLog m,
    Show err,
    MonadCatch m
  ) =>
  UTCTime ->
  Year ->
  Int ->
  Maybe Text ->
  [EventTag.EventTagWithCount] ->
  Either err [Events.EventModel] ->
  m (Lucid.Html ())
renderWeekTemplate currentTime year weekNum maybeTagFilter eventTagsWithCounts = \case
  Left err -> do
    Log.logAttention "Failed to fetch events from database" (Aeson.object ["error" .= show err])
    pure (errorTemplate "Failed to load events. Please try again.")
  Right events -> do
    let currentWeek = (year, fromIntegral weekNum)
        weekGrid = generateWeekGrid year weekNum events
        startDate = weekStartDate year weekNum
        endDate = weekEndDate year weekNum
        navigation = weekNavigation year weekNum
    pure $ do
      header currentTime (WeekView year weekNum) maybeTagFilter currentWeek eventTagsWithCounts
      Lucid.section_ [Lucid.id_ "events-content-container", Lucid.class_ "w-full"] $ do
        renderWeekContent year weekNum maybeTagFilter eventTagsWithCounts weekGrid startDate endDate navigation
