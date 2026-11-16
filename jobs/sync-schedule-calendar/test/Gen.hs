-- | Generators and fixtures that the specs share.
module Gen
  ( genRecurrence,
    genDay,
    genSeriesKey,
    genCalendarEvent,
    weeklySaturday,
    day,
  )
where

--------------------------------------------------------------------------------

import Data.Time (Day, DayOfWeek (..), LocalTime (..), TimeOfDay (..), addDays, addLocalTime, fromGregorian)
import Domain.Types.Recurrence (Recurrence, everyWeek, recurrenceFromRow, recurring)
import Event (CalendarEvent (..))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rrule (Rule (..))
import Series (Series (..), SeriesKey (..), SeriesKind (..))

--------------------------------------------------------------------------------

-- | Any weekday with any week set that the @weeks_of_month@ column can hold.
genRecurrence :: Gen Recurrence
genRecurrence = do
  dow <- Gen.element [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]
  weeks <- Gen.filter (not . null) (Gen.subsequence [1, 2, 3, 4, 5])
  pure (recurrenceFromRow dow weeks)

-- | A date from 2024 through 2030.
genDay :: Gen Day
genDay = (`addDays` fromGregorian 2024 1 1) <$> Gen.integral (Range.linear 0 2556)

genSeriesKey :: Gen SeriesKey
genSeriesKey = SeriesKey <$> Gen.integral (Range.linear 1 100000) <*> Gen.element [Primary, Replay]

-- | An event as 'Event.seriesToEvent' can give it. Each time is on the half-hour
-- grid of the schedule. No time is in the 02:00 hour, so none is in the spring
-- daylight-saving gap.
genCalendarEvent :: Gen CalendarEvent
genCalendarEvent = do
  key <- genSeriesKey
  summary <- Gen.text (Range.linear 1 30) Gen.unicode
  description <- Gen.text (Range.linear 0 80) Gen.unicode
  startDay <- genDay
  hour <- Gen.element ([0, 1] <> [3 .. 23])
  minute <- Gen.element [0, 30]
  lengthMinutes <- (* 30) <$> Gen.integral (Range.linear 1 8)
  recurrence <- genRecurrence
  lastDay <- Gen.maybe genDay
  let start = LocalTime startDay (TimeOfDay hour minute 0)
      end = addLocalTime (fromIntegral (lengthMinutes * 60 :: Int)) start
  pure
    CalendarEvent
      { ceKey = key,
        ceSummary = summary,
        ceDescription = description,
        ceStart = start,
        ceEnd = if todHour (localTimeOfDay end) == 2 then addLocalTime 3600 end else end,
        ceRule = Rule recurrence lastDay
      }

-- | A date literal.
day :: Integer -> Int -> Int -> Day
day = fromGregorian

-- | A live weekly show on Saturday from 18:00 to 20:00, open since 2026-01-03.
weeklySaturday :: Series
weeklySaturday =
  Series
    { sKey = SeriesKey 1 Primary,
      sTemplateId = 1,
      sRecurrence = recurring Saturday everyWeek,
      sStart = TimeOfDay 18 0 0,
      sLengthMinutes = 120,
      sFrom = day 2026 1 3,
      sUntil = Nothing,
      sShowLive = True,
      sTitle = "Night Drive",
      sSlug = "night-drive",
      sDescription = Just "Two hours of slow music.",
      sHosts = ["Ada", "Grace"]
    }
