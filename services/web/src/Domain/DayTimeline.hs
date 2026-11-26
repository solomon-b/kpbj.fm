-- | The timeline of one day, as the dashboard shows it.
--
-- Each segment gets the slot that holds it and the problems staff must see.
-- The functions here are pure. 'Effects.Timeline.dayTimeline' collects the
-- input.
module Domain.DayTimeline
  ( -- * Input
    TimelineData (..),

    -- * Problems
    Flag (..),
    Problem (..),

    -- * Segments
    SegmentView (..),
    segmentViews,
    scheduled,
    problems,
    contentFillerSeconds,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Domain.BreakPlanner (breakSeconds)
import Domain.Timeline (Content (..), EpisodeCandidate, Item (..), Segment (..), fillerSeconds)
import Effects.Database.Tables.Episodes qualified as Episodes

--------------------------------------------------------------------------------
-- Input

-- | The timeline of a window, with the inputs that explain it.
data TimelineData = TimelineData
  { tdSegments :: [Segment],
    -- | The slot windows the timeline used, in time order.
    tdSlots :: [Episodes.SlotWindow],
    -- | The slot windows left out because they overlap an earlier window.
    tdDroppedSlots :: [Episodes.SlotWindow],
    -- | Every break boundary in the window, in time order.
    tdBreakBoundaries :: [UTCTime]
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Problems

-- | Why a segment needs attention.
data Flag
  = -- | The slot of the segment has no episode that can play. Either the host
    -- uploaded nothing, or the upload has no length. The second field is the
    -- rerun that airs in the segment, when one does.
    NoPlayableEpisode Episodes.SlotWindow (Maybe EpisodeCandidate)
  | -- | The content region of the segment holds more than 'fillerSeconds' of
    -- filler, in seconds.
    LongFiller Int64
  | -- | A slot window that overlaps an earlier window, so it does not air.
    -- The second field is the window that airs in its place.
    DroppedSlot Episodes.SlotWindow (Maybe Episodes.SlotWindow)
  deriving stock (Show, Eq)

-- | One problem of the day.
data Problem = Problem
  { -- | The start of the segment that shows the problem.
    problemSegmentStart :: UTCTime,
    -- | The slot that holds that segment.
    problemSlot :: Maybe Episodes.SlotWindow,
    problemFlag :: Flag
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Segments

-- | One segment with what the page needs to explain it.
data SegmentView = SegmentView
  { svSegment :: Segment,
    -- | The slot window whose [start, end) holds the segment start. Nothing
    -- when the segment is unscheduled.
    svSlot :: Maybe Episodes.SlotWindow,
    svFlags :: [Flag]
  }
  deriving stock (Show, Eq)

-- | Each segment of the day with its slot and its flags.
--
-- A dropped slot window flags the first segment that ends after the window
-- opens.
segmentViews :: TimelineData -> [SegmentView]
segmentViews timeline = map toView (tdSegments timeline)
  where
    toView segment =
      SegmentView
        { svSegment = segment,
          svSlot = slot,
          svFlags = noEpisode <> longFiller <> dropped
        }
      where
        slot = find (holds (segStart segment)) (tdSlots timeline)
        noEpisode = [NoPlayableEpisode window rerun | Just window <- [slot], isNothing window.swEpisode]
        rerun = listToMaybe [episode | Item {itemContent = RerunContent episode} <- segItems segment]
        fillerTotal = contentFillerSeconds (tdBreakBoundaries timeline) segment
        longFiller = [LongFiller fillerTotal | fillerTotal > fillerSeconds]
        dropped =
          [ DroppedSlot window (find (overlaps window) (tdSlots timeline))
          | window <- tdDroppedSlots timeline,
            fmap segStart (anchorOf window) == Just (segStart segment)
          ]

    holds instant window = window.swStart <= instant && instant < window.swEnd
    overlaps a b = a.swStart < b.swEnd && b.swStart < a.swEnd
    anchorOf window = find (\segment -> segEnd segment > window.swStart) (tdSegments timeline)

-- | The segments that a slot window holds, and the segments that show a
-- dropped slot window.
--
-- TODO: Show the unscheduled segments again when the schedule is full. Until
-- then, most hours are unscheduled and their filler hides the real problems.
scheduled :: [SegmentView] -> [SegmentView]
scheduled = filter (\view -> isJust (svSlot view) || any isDropped (svFlags view))
  where
    isDropped = \case
      DroppedSlot _ _ -> True
      _ -> False

-- | The problems of the day, in time order.
--
-- A slot window with no playable episode is one problem, on the first
-- segment of the window. Every other flag is one problem.
problems :: [SegmentView] -> [Problem]
problems views = concat (zipWith problemsOf (Nothing : map svSlot views) views)
  where
    problemsOf previousSlot view =
      [ Problem (segStart (svSegment view)) (svSlot view) flag
      | flag <- svFlags view,
        not (isRepeat previousSlot view flag)
      ]
    isRepeat previousSlot view = \case
      NoPlayableEpisode _ _ -> previousSlot == svSlot view
      _ -> False

-- | The seconds of filler in the content region of a segment.
--
-- A segment that ends at a break boundary has its last 'breakSeconds' as a
-- break region, or all of it when it is shorter. Filler there pads a short
-- break, so it does not count.
contentFillerSeconds ::
  -- | The break boundaries
  [UTCTime] ->
  Segment ->
  Int64
contentFillerSeconds boundaries segment =
  sum
    [ itemSeconds item
    | item@Item {itemContent = FillerContent} <- segItems segment,
      itemPlannedStart item < breakStart
    ]
  where
    segmentSeconds = floor (diffUTCTime (segEnd segment) (segStart segment)) :: Int64
    breakStart
      | segEnd segment `elem` boundaries =
          addUTCTime (negate (fromIntegral (min breakSeconds segmentSeconds))) (segEnd segment)
      | otherwise = segEnd segment
