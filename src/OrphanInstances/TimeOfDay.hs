{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.TimeOfDay
  ( formatTimeOfDay,
    add12Hours,
    formatScheduleDual,
  )
where

--------------------------------------------------------------------------------

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
