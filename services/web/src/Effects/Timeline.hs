-- | Collect the inputs of the playout timeline from the database.
--
-- 'Domain.Timeline.buildTimeline' is pure. This module reads the schedule, the
-- break plans, and the ephemeral pool, and hands them to it.
module Effects.Timeline
  ( lookAround,
    timelineAround,

    -- * Exported for testing
    slotInputs,
  )
where

--------------------------------------------------------------------------------

import App.Monad (AppM)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time (Day, NominalDiffTime, UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Domain.Timeline
import Domain.Types.Timezone (pacificDay)
import Effects.BreakPlan (ensurePlan, forecastDay, tracksForDay)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakPlans qualified as BreakPlans
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Hasql.Pool (UsageError)
import Log qualified

--------------------------------------------------------------------------------

-- | How far the timeline reaches on each side of the centre instant.
lookAround :: NominalDiffTime
lookAround = 12 * 3600

-- | Build the timeline around an instant.
--
-- The window runs from 'lookAround' before the centre to 'lookAround' after
-- it. Both ends move out to a whole hour. Today reads its stored plan, and
-- builds it first when it does not exist. A past day only reads its stored
-- plan. A later day uses the forecast.
timelineAround ::
  -- | The current time
  UTCTime ->
  -- | The centre instant
  UTCTime ->
  AppM (Either UsageError [Segment])
timelineAround now centre = runExceptT $ do
  windows <- ExceptT $ execQuery (Episodes.getSlotWindowsBetween from to)
  kinds <- ExceptT $ execQuery (ShowSchedule.breakKindsBetween from to)
  uploads <- ExceptT $ execQuery EphemeralUploads.getPlayableEphemeralUploads
  planned <- concat <$> traverse (ExceptT . plannedTracksOfDay now) days

  let (slots, droppedWindows) = slotInputs windows
  lift $ forM_ droppedWindows $ \window ->
    Log.logAttention "Slot window overlaps an earlier window" (show window)

  let tracksInWindow = [t | t <- planned, t.plBoundary >= from, t.plBoundary <= to]
      input =
        TimelineInput
          { tiFrom = from,
            tiTo = to,
            tiSlots = slots,
            tiBreaks = toBreakInputs (map fst kinds) tracksInWindow,
            tiEphemeral = mapMaybe toEphemeralCandidate uploads
          }
  pure (buildTimeline input)
  where
    from = floorToHour (addUTCTime (negate lookAround) centre)
    to = ceilingToHour (addUTCTime lookAround centre)
    -- A break belongs to the Pacific day of its start, 120 seconds before its
    -- boundary.
    days = [pacificDay (addUTCTime (-120) from) .. pacificDay (addUTCTime (-120) to)]

-- | The planned tracks of one day.
--
-- Only today's plan is built here. A past day reads its stored plan and gets
-- no tracks when it has no plan. A plan built late for a past day would
-- change the rotation of the days after it.
plannedTracksOfDay :: UTCTime -> Day -> AppM (Either UsageError [BreakPlans.PlannedTrack])
plannedTracksOfDay now day =
  case compare day today of
    LT -> tracksForDay day
    EQ -> runExceptT $ do
      ExceptT (ensurePlan day)
      ExceptT (tracksForDay day)
    GT -> forecastDay now day
  where
    today = pacificDay now

--------------------------------------------------------------------------------
-- Slots

-- | Turn time-ordered slot windows into slot inputs.
--
-- It drops a window that overlaps an earlier one, and returns the dropped
-- windows second, so the caller can log them.
slotInputs :: [Episodes.SlotWindow] -> ([SlotInput], [Episodes.SlotWindow])
slotInputs windows =
  let (kept, dropped) = splitOverlaps windows
   in (map toSlotInput kept, dropped)

-- | Split time-ordered windows into the kept ones and the dropped ones.
--
-- A window is dropped when it opens before the last kept window closes. A
-- window that opens exactly when the last one closes is kept.
splitOverlaps :: [Episodes.SlotWindow] -> ([Episodes.SlotWindow], [Episodes.SlotWindow])
splitOverlaps = go [] []
  where
    go kept dropped [] = (reverse kept, reverse dropped)
    go kept dropped (window : rest) = case kept of
      lastKept : _
        | window.swStart < lastKept.swEnd -> go kept (window : dropped) rest
      _ -> go (window : kept) dropped rest

-- | Convert one window to the input of the timeline.
toSlotInput :: Episodes.SlotWindow -> SlotInput
toSlotInput window =
  SlotInput
    { siStart = window.swStart,
      siEnd = window.swEnd,
      siShowTitle = window.swShowTitle,
      siEpisode = toCandidate <$> window.swEpisode
    }
  where
    toCandidate :: Episodes.SlotEpisode -> EpisodeCandidate
    toCandidate episode =
      EpisodeCandidate
        { ecId = episode.seId,
          ecShowTitle = window.swShowTitle,
          ecAudioKey = episode.seAudioFilePath,
          ecSeconds = episode.seDurationSeconds
        }

--------------------------------------------------------------------------------
-- Breaks

-- | One break input for each boundary, in the order of the boundaries.
--
-- A boundary with no planned track gets an empty break.
toBreakInputs :: [UTCTime] -> [BreakPlans.PlannedTrack] -> [BreakInput]
toBreakInputs boundaries planned =
  [ BreakInput boundary (fromMaybe [] (Map.lookup boundary tracksByBoundary))
  | boundary <- boundaries
  ]
  where
    -- The planned tracks arrive in play order. The flipped append puts each
    -- new track after the older ones, which keeps that order.
    tracksByBoundary :: Map UTCTime [BreakTrack]
    tracksByBoundary =
      Map.fromListWith
        (flip (++))
        [(t.plBoundary, [toBreakTrack t]) | t <- planned]

toBreakTrack :: BreakPlans.PlannedTrack -> BreakTrack
toBreakTrack track =
  BreakTrack
    { btSourceType = track.plSourceType,
      btTitle = track.plTitle,
      btAudioKey = track.plAudioFilePath,
      btSeconds = track.plSeconds,
      btBreakItemId = track.plBreakItemId
    }

--------------------------------------------------------------------------------
-- Ephemeral uploads

-- | The query only returns uploads with a length. An upload without one is
-- skipped, because the timeline cannot place it.
toEphemeralCandidate :: EphemeralUploads.Model -> Maybe EphemeralCandidate
toEphemeralCandidate upload = case upload.eumDurationSeconds of
  Nothing -> Nothing
  Just seconds ->
    Just
      EphemeralCandidate
        { ecdId = upload.eumId,
          ecdTitle = upload.eumTitle,
          ecdAudioKey = upload.eumAudioFilePath,
          ecdSeconds = seconds
        }

--------------------------------------------------------------------------------
-- Whole hours

-- | Round down to a whole hour. Every Pacific offset is a whole hour, so this
-- is also a whole Pacific hour.
floorToHour :: UTCTime -> UTCTime
floorToHour instant = posixSecondsToUTCTime (fromIntegral (hours * 3600))
  where
    hours = floor (utcTimeToPOSIXSeconds instant / 3600) :: Integer

-- | Round up to a whole hour.
ceilingToHour :: UTCTime -> UTCTime
ceilingToHour instant = posixSecondsToUTCTime (fromIntegral (hours * 3600))
  where
    hours = ceiling (utcTimeToPOSIXSeconds instant / 3600) :: Integer
