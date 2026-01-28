{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.TimeOfDay
  ( formatTimeOfDay,
    add12Hours,
    formatScheduleDual,
    formatWeeksOfMonth,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (TimeOfDay (..))
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeValue (..), EncodeValue (..))

--------------------------------------------------------------------------------

instance EncodeValue TimeOfDay where
  encodeValue = Encoders.time

instance DecodeValue TimeOfDay where
  decodeValue = Decoders.time

--------------------------------------------------------------------------------

-- | Format TimeOfDay as HH:MM text for display.
formatTimeOfDay :: TimeOfDay -> Text
formatTimeOfDay = Text.pack . formatTime defaultTimeLocale "%H:%M"

-- | Add 12 hours to a TimeOfDay (wraps around midnight).
add12Hours :: TimeOfDay -> TimeOfDay
add12Hours (TimeOfDay h m s) = TimeOfDay ((h + 12) `mod` 24) m s

-- | Format schedule with optional dual-airing display.
--
-- For dual-airing shows, displays both time ranges:
-- e.g., "09:00 - 11:00 & 21:00 - 23:00"
formatScheduleDual ::
  -- | Start time
  TimeOfDay ->
  -- | End time
  TimeOfDay ->
  -- | Airs twice daily
  Bool ->
  Text
formatScheduleDual start end airsTwice
  | airsTwice =
      formatTimeOfDay start
        <> " - "
        <> formatTimeOfDay end
        <> " & "
        <> formatTimeOfDay (add12Hours start)
        <> " - "
        <> formatTimeOfDay (add12Hours end)
  | otherwise =
      formatTimeOfDay start <> " - " <> formatTimeOfDay end

-- | Format weeks of month as ordinal prefix.
--
-- Returns empty string for weekly shows (all weeks or Nothing).
-- For specific weeks, returns ordinals like "1st", "1st & 3rd", etc.
--
-- Examples:
--   Nothing        -> ""
--   Just [1,2,3,4,5] -> ""
--   Just [1]       -> "1st "
--   Just [1,3]     -> "1st & 3rd "
--   Just [2,4]     -> "2nd & 4th "
formatWeeksOfMonth :: Maybe [Int64] -> Text
formatWeeksOfMonth Nothing = ""
formatWeeksOfMonth (Just weeks)
  | sort weeks == [1, 2, 3, 4, 5] = ""
  | otherwise = Text.intercalate " & " (map ordinal (sort weeks)) <> " "
  where
    ordinal :: Int64 -> Text
    ordinal 1 = "1st"
    ordinal 2 = "2nd"
    ordinal 3 = "3rd"
    ordinal 4 = "4th"
    ordinal 5 = "5th"
    ordinal n = Text.pack (show n) <> "th"
