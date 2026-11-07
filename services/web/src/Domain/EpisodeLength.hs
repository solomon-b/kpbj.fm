{-# LANGUAGE QuasiQuotes #-}

-- | The shortest and longest episode a schedule slot can hold.
--
-- The station break starts 'breakSeconds' before the end of every slot, and it
-- cuts any episode still on air. An episode must therefore end at least that
-- long before its slot ends. An episode must also end at most
-- 'lengthWindowSeconds' before the break, so playout fills a gap of known size.
-- Both limits are hard. A 1 hour slot holds 57:00 to 58:00 of audio.
module Domain.EpisodeLength
  ( slotSeconds,
    lengthWindowSeconds,
    maxEpisodeSeconds,
    minEpisodeSeconds,
    checkEpisodeFitsSlot,
    formatLength,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (TimeOfDay, timeOfDayToTime)
import Domain.BreakPlanner (breakSeconds)

--------------------------------------------------------------------------------

-- | The length of a slot in seconds.
--
-- An end at or before the start means the slot runs past midnight. This is the
-- @slot_length@ CASE in 'Effects.Database.Tables.ShowSchedule.breakKindsBetween'.
slotSeconds ::
  -- | Slot start
  TimeOfDay ->
  -- | Slot end
  TimeOfDay ->
  Int64
slotSeconds startTime endTime
  | endSeconds > startSeconds = endSeconds - startSeconds
  | otherwise = secondsPerDay - (startSeconds - endSeconds)
  where
    startSeconds = timeOfDaySeconds startTime
    endSeconds = timeOfDaySeconds endTime
    secondsPerDay = 24 * 60 * 60

-- | The longest episode, in seconds, that a slot can hold.
maxEpisodeSeconds ::
  -- | Slot start
  TimeOfDay ->
  -- | Slot end
  TimeOfDay ->
  Int64
maxEpisodeSeconds startTime endTime =
  slotSeconds startTime endTime - breakSeconds

-- | The largest gap, in seconds, between the end of an episode and the break.
lengthWindowSeconds :: Int64
lengthWindowSeconds = 60

-- | The shortest episode, in seconds, that a slot accepts.
minEpisodeSeconds ::
  -- | Slot start
  TimeOfDay ->
  -- | Slot end
  TimeOfDay ->
  Int64
minEpisodeSeconds startTime endTime =
  maxEpisodeSeconds startTime endTime - lengthWindowSeconds

-- | Check that an episode ends before the station break of its slot, and no
-- more than 'lengthWindowSeconds' before it.
--
-- The 'Left' holds a message for the host.
checkEpisodeFitsSlot ::
  -- | Slot start
  TimeOfDay ->
  -- | Slot end
  TimeOfDay ->
  -- | Episode length in seconds
  Int64 ->
  Either Text ()
checkEpisodeFitsSlot startTime endTime episodeSeconds
  | episodeSeconds > maxSeconds =
      Left
        [i|This episode runs #{formatLength episodeSeconds}. Episodes in this #{slotName} slot must run from #{formatLength minSeconds} to #{formatLength maxSeconds}, so the station break fits. Trim the audio and upload it again.|]
  | episodeSeconds < minSeconds =
      Left
        [i|This episode runs #{formatLength episodeSeconds}. Episodes in this #{slotName} slot must run from #{formatLength minSeconds} to #{formatLength maxSeconds}, so the episode fills the slot up to the station break. Add audio and upload it again.|]
  | otherwise = Right ()
  where
    maxSeconds = maxEpisodeSeconds startTime endTime
    minSeconds = minEpisodeSeconds startTime endTime
    slotName = describeSlotLength (slotSeconds startTime endTime)

-- | Format a length in seconds as @M:SS@, or as @H:MM:SS@ from one hour up.
formatLength :: Int64 -> Text
formatLength totalSeconds
  | hours > 0 = [i|#{hours}:#{twoDigits minutes}:#{twoDigits seconds}|]
  | otherwise = [i|#{minutes}:#{twoDigits seconds}|]
  where
    (hours, secondsInHour) = totalSeconds `divMod` 3600
    (minutes, seconds) = secondsInHour `divMod` 60

--------------------------------------------------------------------------------

-- | Name a slot length in words, for example "1 hour" or "1 hour 30 minute".
--
-- The words sit before "slot", so they stay singular.
describeSlotLength :: Int64 -> Text
describeSlotLength totalSeconds
  | minutes == 0 = [i|#{hours} hour|]
  | hours == 0 = [i|#{minutes} minute|]
  | otherwise = [i|#{hours} hour #{minutes} minute|]
  where
    totalMinutes = totalSeconds `div` 60
    (hours, minutes) = totalMinutes `divMod` 60

-- | Seconds since midnight. Fractions of a second are dropped.
timeOfDaySeconds :: TimeOfDay -> Int64
timeOfDaySeconds timeOfDay =
  floor (timeOfDayToTime timeOfDay)

-- | Pad a number below 100 to two digits.
twoDigits :: Int64 -> Text
twoDigits n =
  Text.justifyRight 2 '0' (Text.pack (show n))
