{-# LANGUAGE LambdaCase #-}

-- | When a show airs.
--
-- This module holds the only parser and the only formatter for a recurrence. Every
-- caller goes through it, so the database, the schedule editor, the show pages and
-- the invitation summary all describe a given week set the same way.
--
-- The constructor stays unexported and callers eliminate through 'foldRecurrence'.
-- That keeps the sorted, duplicate-free invariant on the weeks, and it means a new
-- constructor gives 'foldRecurrence' another argument, so the compiler names every
-- site that has to handle it.
module Domain.Types.Recurrence
  ( -- * Type
    Recurrence,
    WeekOfMonth,

    -- * Construction
    recurring,
    recurrenceFromRow,
    parseRecurrence,
    parseWeeks,
    mkWeekOfMonth,
    everyWeek,

    -- * Elimination
    foldRecurrence,
    recurrenceDay,
    recurrenceWeeks,
    weekNumbers,
    weekNumber,
    airsEveryWeek,

    -- * Rendering
    formatRecurrence,
    weeksLabel,
    frequencyLabel,
    ordinal,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (DayOfWeek (..))
import OrphanInstances.DayOfWeek (dayOfWeekFromText)

--------------------------------------------------------------------------------

-- | A week of the month, 1 through 5.
--
-- Week 5 covers days 29 to 31, matching @CEIL(EXTRACT(DAY FROM date) / 7.0)@ in
-- @recurrence_airs_on@. A February outside a leap year has no week 5.
newtype WeekOfMonth = WeekOfMonth Int64
  deriving stock (Show, Eq, Ord)

-- | Build a 'WeekOfMonth', rejecting anything outside 1 to 5.
mkWeekOfMonth :: Int64 -> Maybe WeekOfMonth
mkWeekOfMonth n
  | n >= 1 && n <= 5 = Just (WeekOfMonth n)
  | otherwise = Nothing

-- | The underlying number, for encoding to @weeks_of_month@.
weekNumber :: WeekOfMonth -> Int64
weekNumber (WeekOfMonth n) = n

-- | All five weeks, which is how a show that airs every week is stored.
everyWeek :: NonEmpty WeekOfMonth
everyWeek = WeekOfMonth 1 :| [WeekOfMonth 2, WeekOfMonth 3, WeekOfMonth 4, WeekOfMonth 5]

--------------------------------------------------------------------------------

-- | The recurrence of a single schedule slot.
--
-- The weeks are always sorted and free of duplicates, which is what lets callers
-- compare two recurrences with '==' to decide whether a schedule changed. Build one
-- with 'recurring' or 'parseRecurrence'; both normalize.
data Recurrence = Recurring DayOfWeek (NonEmpty WeekOfMonth)
  deriving stock (Show, Eq, Ord)

-- | Build a recurrence, sorting and deduplicating the weeks.
recurring :: DayOfWeek -> NonEmpty WeekOfMonth -> Recurrence
recurring day weeks =
  Recurring day (NE.fromList (Set.toAscList (Set.fromList (NE.toList weeks))))

-- | Build a recurrence from a @schedule_templates@ row.
--
-- The column type is a plain @[Int64]@, but the table guarantees more than that:
-- @schedule_templates_weeks_of_month_check@ rejects an empty array and anything
-- outside 1 to 5. This drops any value the database cannot hold and falls back to
-- 'everyWeek' if that leaves nothing.
--
-- The fallback is unreachable through the constraint. It exists so display code can
-- take a 'Recurrence' directly rather than a 'Maybe' for a case the schema forbids.
recurrenceFromRow :: DayOfWeek -> [Int64] -> Recurrence
recurrenceFromRow day weeks =
  recurring day $ case NE.nonEmpty (mapMaybe mkWeekOfMonth weeks) of
    Just valid -> valid
    Nothing -> everyWeek

-- | Eliminate a 'Recurrence'.
--
-- The only way to read a recurrence apart. Adding a constructor gives this another
-- argument, so the compiler lists every caller that has to handle it.
foldRecurrence :: (DayOfWeek -> NonEmpty WeekOfMonth -> r) -> Recurrence -> r
foldRecurrence f (Recurring day weeks) = f day weeks

-- | The weekday this recurrence airs on.
recurrenceDay :: Recurrence -> DayOfWeek
recurrenceDay = foldRecurrence const

-- | The weeks of the month this recurrence airs on, sorted.
recurrenceWeeks :: Recurrence -> NonEmpty WeekOfMonth
recurrenceWeeks = foldRecurrence (\_ weeks -> weeks)

-- | The weeks as plain numbers, for the @weeks_of_month@ column.
weekNumbers :: Recurrence -> [Int64]
weekNumbers = NE.toList . NE.map weekNumber . recurrenceWeeks

-- | True when the recurrence covers every week of the month.
airsEveryWeek :: Recurrence -> Bool
airsEveryWeek r = recurrenceWeeks r == everyWeek

--------------------------------------------------------------------------------

-- | Parse a recurrence from the day name and week numbers a form submits.
--
-- The error strings are user-facing. They name the offending field so a bad
-- submission fails at the parse boundary rather than as an opaque database error
-- after the schedule diff has already decided which templates to close.
--
-- Out-of-range weeks are reported in the order they arrived, not sorted, so the
-- message points at what the caller sent.
parseRecurrence :: Text -> [Int64] -> Either Text Recurrence
parseRecurrence dayText weeks = do
  day <- maybe (Left $ "Invalid day of week: " <> dayText) Right (dayOfWeekFromText dayText)
  recurring day <$> parseWeeks weeks

-- | Parse the week numbers alone.
--
-- Separate from 'parseRecurrence' so a caller that validates other fields in between
-- can choose which error a submission with several bad fields reports.
parseWeeks :: [Int64] -> Either Text (NonEmpty WeekOfMonth)
parseWeeks [] = Left "Pick at least one week of the month."
parseWeeks weeks =
  case filter (\w -> w < 1 || w > 5) weeks of
    [] -> Right (NE.fromList (map WeekOfMonth weeks))
    outOfRange ->
      Left $
        "Invalid week of month: "
          <> Text.intercalate ", " (map (Text.pack . show) outOfRange)
          <> " (must be 1 to 5)."

--------------------------------------------------------------------------------

-- | An ordinal for a week number: @1@ becomes @"1st"@.
ordinal :: WeekOfMonth -> Text
ordinal (WeekOfMonth n) = case n of
  1 -> "1st"
  2 -> "2nd"
  3 -> "3rd"
  4 -> "4th"
  _ -> "5th"

-- | The weeks as a human label, or 'Nothing' when the show airs every week.
--
-- Callers wrap this however their surface needs: a prefix on the show page, a
-- parenthetical on the edit form, a standalone label on an invitation.
--
-- > weeksLabel (recurring Monday everyWeek)  == Nothing
-- > weeksLabel (recurring Monday [1,3])      == Just "1st & 3rd"
weeksLabel :: Recurrence -> Maybe Text
weeksLabel r
  | airsEveryWeek r = Nothing
  | otherwise = Just (Text.intercalate " & " (NE.toList (NE.map ordinal (recurrenceWeeks r))))

-- | The editor's frequency key for this recurrence.
--
-- These three values drive the WEEKLY, TWICE A MONTH and ONCE A MONTH buttons in
-- 'Component.ScheduleEditor'. Five weeks is weekly, one week is once a month, and
-- anything between is twice.
frequencyLabel :: Recurrence -> Text
frequencyLabel r = case NE.length (recurrenceWeeks r) of
  5 -> "weekly"
  1 -> "once"
  _ -> "twice"

-- | The recurrence as a phrase, for example @"1st & 3rd Mondays"@ or @"Mondays"@.
formatRecurrence :: Recurrence -> Text
formatRecurrence r =
  let days = display (recurrenceDay r) <> "s"
   in case weeksLabel r of
        Nothing -> days
        Just weeks -> weeks <> " " <> days
