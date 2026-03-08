-- | Timezone utilities for converting UTC times to Pacific time.
--
-- KPBJ is a Pacific timezone station, so all user-facing times should be
-- displayed in America/Los_Angeles time (PST/PDT with proper DST handling).
module Domain.Types.Timezone
  ( -- * Pacific Time Conversion
    utcToPacific,
    pacificToUtc,
    formatPacificDate,
    formatPacificDateLong,
    formatPacificForDateTimeInput,
    parsePacificFromDateTimeInput,

    -- * Time Utilities
    addMinutesToTimeOfDay,
    parseTimeHHMM,
    parseDateYMD,

    -- * Re-exports
    LocalTime (..),
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, LocalTime (..), TimeOfDay (..), UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.Zones (TZ, loadSystemTZ, localTimeToUTCTZ, utcToLocalTimeTZ)
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------

-- | The Pacific timezone (America/Los_Angeles) with proper DST handling.
--
-- This is loaded once at startup using unsafePerformIO since timezone data
-- is static and loading it is referentially transparent.
{-# NOINLINE pacificTZ #-}
pacificTZ :: TZ
pacificTZ = unsafePerformIO $ loadSystemTZ "America/Los_Angeles"

-- | Convert a UTC time to Pacific local time with proper DST handling.
--
-- This correctly handles the transition between PST (UTC-8) and PDT (UTC-7)
-- based on the actual date.
utcToPacific :: UTCTime -> LocalTime
utcToPacific = utcToLocalTimeTZ pacificTZ

-- | Format a UTC time as a Pacific date string (e.g., "Feb 03, 2026").
formatPacificDate :: UTCTime -> Text
formatPacificDate utc =
  Text.pack $ formatTime defaultTimeLocale "%b %d, %Y" (utcToPacific utc)

-- | Format a UTC time as a long Pacific date string (e.g., "February 03, 2026").
formatPacificDateLong :: UTCTime -> Text
formatPacificDateLong utc =
  Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" (utcToPacific utc)

-- | Convert a Pacific local time to UTC with proper DST handling.
--
-- This is the inverse of 'utcToPacific', used when parsing user input that
-- is expected to be in Pacific time (e.g., from datetime-local form fields).
pacificToUtc :: LocalTime -> UTCTime
pacificToUtc = localTimeToUTCTZ pacificTZ

-- | Format a UTC time for use in an HTML5 datetime-local input.
--
-- The datetime-local input expects values in "YYYY-MM-DDTHH:MM" format
-- representing local time (not UTC). Since KPBJ operates in Pacific time,
-- this converts the UTC time to Pacific before formatting.
formatPacificForDateTimeInput :: UTCTime -> Text
formatPacificForDateTimeInput utc =
  Text.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" (utcToPacific utc)

-- | Parse a datetime-local input value as Pacific time and convert to UTC.
--
-- Returns Nothing if the format is invalid.
parsePacificFromDateTimeInput :: Text -> Maybe UTCTime
parsePacificFromDateTimeInput txt =
  pacificToUtc <$> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" (Text.unpack txt)

--------------------------------------------------------------------------------
-- Time Utilities

-- | Add minutes to a 'TimeOfDay', wrapping past midnight.
--
-- Seconds are preserved from the input.
addMinutesToTimeOfDay :: TimeOfDay -> Int -> TimeOfDay
addMinutesToTimeOfDay (TimeOfDay h m s) mins =
  let totalMinutes = h * 60 + m + mins
      newH = (totalMinutes `div` 60) `mod` 24
      newM = totalMinutes `mod` 60
   in TimeOfDay newH newM s

-- | Parse a time of day from @\"HH:MM\"@ format.
parseTimeHHMM :: Text -> Maybe TimeOfDay
parseTimeHHMM t = parseTimeM True defaultTimeLocale "%H:%M" (Text.unpack t)

-- | Parse a date from @\"YYYY-MM-DD\"@ format.
parseDateYMD :: Text -> Maybe Day
parseDateYMD t = parseTimeM True defaultTimeLocale "%Y-%m-%d" (Text.unpack t)
