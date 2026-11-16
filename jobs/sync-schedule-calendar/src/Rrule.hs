-- | The RRULE form of a schedule recurrence.
--
-- Google Calendar stores the recurrence of an event as an RFC 5545 @RRULE@ line.
-- This module holds the printer and the parser for the small part of that
-- grammar that the schedule needs. The job compares parsed rules, not text, so
-- it does not depend on how Google prints a rule.
--
-- See <https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10>.
module Rrule
  ( Rule (..),
    printRule,
    parseRule,
  )
where

--------------------------------------------------------------------------------

import Control.Monad (unless, when)
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, DayOfWeek (..), LocalTime (..), TimeOfDay (..), UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Domain.Types.Recurrence (Recurrence, WeekOfMonth, airsEveryWeek, everyWeek, mkWeekOfMonth, recurrenceDay, recurring, weekNumbers)
import Domain.Types.Timezone (pacificDay, pacificToUtc)

--------------------------------------------------------------------------------

-- | A recurrence with an optional end.
data Rule = Rule
  { ruleRecurrence :: Recurrence,
    -- | The last air date that the rule permits, inclusive, in station time.
    ruleLastDay :: Maybe Day
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Printer

-- | Print a rule as one @RRULE@ line.
--
-- A recurrence on every week is a weekly rule. Any other week set is a monthly
-- rule with one ordinal weekday for each week, for example @1SA,3SA@. RFC 5545
-- skips an ordinal that a month does not have, as @recurrence_airs_on@ does for
-- week 5.
--
-- @UNTIL@ is the last second of the last day in station time, written in UTC.
printRule :: Rule -> Text
printRule Rule {..} =
  "RRULE:" <> Text.intercalate ";" (frequency <> foldMap (\d -> ["UNTIL=" <> untilText d]) ruleLastDay)
  where
    code :: Text
    code = dayCode (recurrenceDay ruleRecurrence)

    frequency :: [Text]
    frequency
      | airsEveryWeek ruleRecurrence = ["FREQ=WEEKLY", "BYDAY=" <> code]
      | otherwise =
          [ "FREQ=MONTHLY",
            "BYDAY=" <> Text.intercalate "," [Text.pack (show week) <> code | week <- weekNumbers ruleRecurrence]
          ]

    untilText :: Day -> Text
    untilText d = Text.pack (formatTime defaultTimeLocale untilFormat (pacificToUtc (LocalTime d (TimeOfDay 23 59 59))))

untilFormat :: String
untilFormat = "%Y%m%dT%H%M%SZ"

dayCodes :: [(DayOfWeek, Text)]
dayCodes =
  [ (Sunday, "SU"),
    (Monday, "MO"),
    (Tuesday, "TU"),
    (Wednesday, "WE"),
    (Thursday, "TH"),
    (Friday, "FR"),
    (Saturday, "SA")
  ]

dayCode :: DayOfWeek -> Text
dayCode dow = fromMaybe "" (lookup dow dayCodes)

--------------------------------------------------------------------------------
-- Parser

-- | Parse one @RRULE@ line. The parts can be in any order.
--
-- The parser accepts the rules that 'printRule' writes, with @WKST@ and
-- @INTERVAL=1@ permitted. It rejects every other rule. A rejected rule is one
-- that a person wrote, and the job replaces it.
parseRule :: Text -> Either Text Rule
parseRule line = do
  body <- maybe (Left "The line does not start with RRULE:.") Right (Text.stripPrefix "RRULE:" line)
  parts <- Map.fromList <$> traverse parsePart (Text.splitOn ";" body)
  let unknown = Map.keys parts `without` ["FREQ", "BYDAY", "UNTIL", "WKST", "INTERVAL"]
  unless (null unknown) $ Left ("Parts that the schedule does not use: " <> Text.intercalate ", " unknown)
  when (Map.findWithDefault "1" "INTERVAL" parts /= "1") $ Left "INTERVAL is not 1."
  byDay <- part "BYDAY" parts
  recurrence <-
    part "FREQ" parts >>= \case
      "WEEKLY" -> (`recurring` everyWeek) <$> parseDayCode byDay
      "MONTHLY" -> parseOrdinalDays byDay
      other -> Left ("FREQ is " <> other <> ".")
  lastDay <- traverse parseUntil (Map.lookup "UNTIL" parts)
  pure (Rule recurrence lastDay)
  where
    without :: [Text] -> [Text] -> [Text]
    without xs ys = filter (`notElem` ys) xs

parsePart :: Text -> Either Text (Text, Text)
parsePart text = case Text.breakOn "=" text of
  (name, value)
    | not (Text.null name), Just v <- Text.stripPrefix "=" value -> Right (name, v)
  _ -> Left ("A part has no name or no value: " <> text)

part :: Text -> Map Text Text -> Either Text Text
part name = maybe (Left (name <> " is absent.")) Right . Map.lookup name

parseDayCode :: Text -> Either Text DayOfWeek
parseDayCode code =
  maybe (Left ("Not one weekday: " <> code)) Right (lookup code [(c, dow) | (dow, c) <- dayCodes])

-- | Parse a list such as @1SA,3SA@. Each item must name the same weekday.
parseOrdinalDays :: Text -> Either Text Recurrence
parseOrdinalDays text = do
  items <- traverse parseOrdinalDay (Text.splitOn "," text)
  case NonEmpty.nonEmpty items of
    Just nonEmpty@((_, dow) NonEmpty.:| _)
      | all ((== dow) . snd) items -> Right (recurring dow (fmap fst nonEmpty))
    _ -> Left ("Not ordinals of one weekday: " <> text)

parseOrdinalDay :: Text -> Either Text (WeekOfMonth, DayOfWeek)
parseOrdinalDay item = do
  let (digits, code) = Text.span isDigit item
  week <- case mkWeekOfMonth =<< readDigit digits of
    Just w -> Right w
    Nothing -> Left ("Not an ordinal from 1 to 5: " <> item)
  dow <- parseDayCode code
  pure (week, dow)
  where
    readDigit :: Text -> Maybe Int64
    readDigit digits = lookup digits [(Text.pack (show n), n) | n <- [1 .. 5]]

parseUntil :: Text -> Either Text Day
parseUntil text =
  maybe (Left ("UNTIL is not a UTC date-time: " <> text)) (Right . pacificDay) parsed
  where
    parsed :: Maybe UTCTime
    parsed = parseTimeM False defaultTimeLocale untilFormat (Text.unpack text)
