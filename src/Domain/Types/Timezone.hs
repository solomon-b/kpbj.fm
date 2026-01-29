-- | Timezone utilities for converting UTC times to Pacific time.
--
-- KPBJ is a Pacific timezone station, so all user-facing times should be
-- displayed in America/Los_Angeles time (PST/PDT with proper DST handling).
module Domain.Types.Timezone
  ( -- * Pacific Time Conversion
    utcToPacific,
    utcToPacificDay,
    formatPacificDate,
    formatPacificDateLong,

    -- * Re-exports
    LocalTime (..),
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, LocalTime (..), UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Zones (TZ, loadSystemTZ, utcToLocalTimeTZ)
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

-- | Extract the Pacific date from a UTC time.
--
-- Use this when you need just the date portion (e.g., for episode scheduling).
-- A 7PM Pacific show stored as 3AM UTC the next day will correctly return
-- the Pacific date, not the UTC date.
utcToPacificDay :: UTCTime -> Day
utcToPacificDay = localDay . utcToPacific

-- | Format a UTC time as a Pacific date string (e.g., "Feb 03, 2026").
formatPacificDate :: UTCTime -> Text
formatPacificDate utc =
  Text.pack $ formatTime defaultTimeLocale "%b %d, %Y" (utcToPacific utc)

-- | Format a UTC time as a long Pacific date string (e.g., "February 03, 2026").
formatPacificDateLong :: UTCTime -> Text
formatPacificDateLong utc =
  Text.pack $ formatTime defaultTimeLocale "%B %d, %Y" (utcToPacific utc)
