-- | The slot series: one validity window of one schedule template.
--
-- A series is the unit that the calendar sync works with. Each one becomes one
-- recurring event in the Google Calendar. A template with a replay gives two
-- series for each window, one for the primary slot and one for the replay.
module Series
  ( -- * Key
    SeriesKind (..),
    SeriesKey (..),
    renderSeriesKey,
    parseSeriesKey,

    -- * Series
    Series (..),
    seriesFromRow,
    clipOverlaps,

    -- * Airings
    airings,
    firstAiring,

    -- * State
    SeriesState (..),
    classify,
  )
where

--------------------------------------------------------------------------------

import Data.Foldable (toList)
import Data.Int (Int64)
import Data.List (mapAccumL, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Read qualified as Text.Read
import Data.Time (Day, TimeOfDay, addDays)
import Domain.Types.Recurrence (Recurrence, airsOn, recurrenceFromRow)
import Domain.Types.Timezone (slotDurationMins)
import Effects.Database.Tables.ShowSchedule (CalendarSeriesRow (..))

--------------------------------------------------------------------------------
-- Key

-- | The slot of a template that a series covers.
data SeriesKind = Primary | Replay
  deriving stock (Show, Eq, Ord)

-- | The identity of a series. The calendar event carries it, so the job can
-- find the event of a series with no record of its own.
data SeriesKey = SeriesKey
  { skValidityId :: Int64,
    skKind :: SeriesKind
  }
  deriving stock (Show, Eq, Ord)

-- | The text form of a key, for example @123-primary@.
renderSeriesKey :: SeriesKey -> Text
renderSeriesKey SeriesKey {..} =
  Text.pack (show skValidityId) <> "-" <> case skKind of
    Primary -> "primary"
    Replay -> "replay"

-- | The inverse of 'renderSeriesKey'.
parseSeriesKey :: Text -> Maybe SeriesKey
parseSeriesKey text = do
  let (idText, kindText) = Text.breakOn "-" text
  validityId <- case Text.Read.decimal idText of
    Right (n, "") -> Just n
    _ -> Nothing
  kind <- case kindText of
    "-primary" -> Just Primary
    "-replay" -> Just Replay
    _ -> Nothing
  pure (SeriesKey validityId kind)

--------------------------------------------------------------------------------
-- Series

data Series = Series
  { sKey :: SeriesKey,
    sTemplateId :: Int64,
    sRecurrence :: Recurrence,
    -- | The local start time. For a replay, this is the replay start time.
    sStart :: TimeOfDay,
    -- | A replay runs for the same length as its primary.
    sLengthMinutes :: Int,
    sFrom :: Day,
    -- | Exclusive. 'Nothing' means the window is open.
    sUntil :: Maybe Day,
    -- | The show is active and not deleted.
    sShowLive :: Bool,
    sTitle :: Text,
    sSlug :: Text,
    sDescription :: Maybe Text,
    sHosts :: [Text]
  }
  deriving stock (Show, Eq)

-- | The primary series of a row, and the replay series when the template has a
-- replay time. The replay airs on the same air date as the primary. This is the
-- rule of the @slot_windows@ SQL function.
seriesFromRow :: CalendarSeriesRow -> [Series]
seriesFromRow CalendarSeriesRow {..} =
  series Primary csrStartTime : [series Replay replayStart | replayStart <- toList csrReplayStartTime]
  where
    series :: SeriesKind -> TimeOfDay -> Series
    series kind start =
      Series
        { sKey = SeriesKey csrValidityId kind,
          sTemplateId = csrTemplateId,
          sRecurrence = recurrenceFromRow csrDayOfWeek csrWeeksOfMonth,
          sStart = start,
          sLengthMinutes = slotDurationMins csrStartTime csrEndTime,
          sFrom = csrEffectiveFrom,
          sUntil = csrEffectiveUntil,
          sShowLive = csrShowLive,
          sTitle = csrShowTitle,
          sSlug = display csrShowSlug,
          sDescription = csrShowDescription,
          sHosts = csrHostNames
        }

-- | Remove the overlap between two windows of one template.
--
-- The playout timeline shows one window for an air date that two validity rows
-- cover. So the later window must start where the earlier one ends. A window
-- that an earlier open window covers becomes empty.
clipOverlaps :: [Series] -> [Series]
clipOverlaps =
  -- The key breaks a tie between two windows that start on one date, so the
  -- result does not depend on the order of the input.
  concatMap (snd . mapAccumL clip Nothing . sortOn (\s -> (sFrom s, sKey s)))
    . Map.elems
    . Map.fromListWith (<>)
    . map (\s -> ((sTemplateId s, skKind (sKey s)), [s]))
  where
    -- The state is the latest end of the earlier windows.
    clip :: Maybe End -> Series -> (Maybe End, Series)
    clip covered s =
      let clipped = case covered of
            Nothing -> s
            Just Open -> s {sUntil = Just (sFrom s)}
            Just (ClosedAt end) -> s {sFrom = max end (sFrom s)}
       in (Just (maybe id max covered (seriesEnd s)), clipped)

    seriesEnd :: Series -> End
    seriesEnd = maybe Open ClosedAt . sUntil

-- | The end of a window. An open window ends after every closed one.
data End = ClosedAt Day | Open
  deriving stock (Eq, Ord)

--------------------------------------------------------------------------------
-- Airings

-- | The air dates of a series in ascending order. The list has no end when the
-- window is open.
airings :: Series -> [Day]
airings Series {..} =
  filter (airsOn sRecurrence) (takeWhile inWindow [sFrom ..])
  where
    inWindow :: Day -> Bool
    inWindow d = maybe True (d <) sUntil

-- | The first air date of a series.
--
-- The search stops after 366 days. A recurrence on week 5 alone can skip some
-- months, but it airs in each year.
firstAiring :: Series -> Maybe Day
firstAiring s = listToMaybe (takeWhile (<= addDays 366 (sFrom s)) (airings s))

--------------------------------------------------------------------------------
-- State

-- | What the sync must do with the calendar event of a series.
data SeriesState
  = -- | The window covers today or a later date. The job manages the full event.
    Live
  | -- | The window ended. The value is the last air date that the window
    -- permits. The job sets the end of the event and changes nothing else.
    Ended Day
  | -- | The series has no airing that the calendar must keep.
    Empty
  deriving stock (Show, Eq)

-- | The state of a series on a given date. The date must be the station date.
--
-- An open window of a show that is not live counts as ended today. Playout
-- airs only a live show.
classify :: Day -> Series -> SeriesState
classify today s = case (firstAiring s, effectiveEnd) of
  (Nothing, _) -> Empty
  (Just _, Nothing) -> Live
  (Just first, Just end)
    | end > today -> Live
    | pred end < first -> Empty
    | otherwise -> Ended (pred end)
  where
    effectiveEnd :: Maybe Day
    effectiveEnd
      | sShowLive s = sUntil s
      | otherwise = Just (maybe today (min today) (sUntil s))
