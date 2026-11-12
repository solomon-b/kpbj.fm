-- | The playout timeline.
--
-- A pure function from the schedule, the planned breaks, and the ephemeral
-- pool to every item that airs, each at a fixed instant. Liquidsoap plays the
-- list and chooses nothing.
--
-- The timeline is a list of segments. A segment runs from one edge to the
-- next. The edges are the break boundaries and the slot starts. Each segment
-- has two regions:
--
-- 1. The content region. It holds the episode, filler, or ephemeral uploads.
-- 2. The break region, the last 'breakSeconds' of the segment. Only a segment
--    that ends at a break boundary has one.
--
-- Lengths and offsets are whole seconds. The same input always gives the same
-- timeline, because the random picks use a seed from the region start.
module Domain.Timeline
  ( -- * The filler
    fillerObjectKey,
    fillerSeconds,

    -- * Input
    EpisodeCandidate (..),
    SlotInput (..),
    BreakTrack (..),
    BreakInput (..),
    EphemeralCandidate (..),
    TimelineInput (..),

    -- * Output
    Content (..),
    Item (..),
    Segment (..),

    -- * Building
    buildTimeline,

    -- * Reading
    resumeAt,
    segmentContaining,
    segmentStartingAt,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.List (find, unfoldr)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Domain.BreakPlanner (breakSeconds)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.Episodes qualified as Episodes
import System.Random (StdGen, mkStdGen, uniformR)

--------------------------------------------------------------------------------
-- The filler

-- | The storage key of the filler file.
--
-- The filler covers short gaps, and every gap when the ephemeral pool is
-- empty.
fillerObjectKey :: Text
fillerObjectKey = "audio/filler/kpbj-filler.mp3"

-- | The planned length of one filler item, in seconds.
--
-- The file must be at least this long. A filler item never plays more than
-- this.
fillerSeconds :: Int64
fillerSeconds = 180

--------------------------------------------------------------------------------
-- Input

-- | The episode that airs in a slot.
data EpisodeCandidate = EpisodeCandidate
  { ecId :: Episodes.Id,
    ecShowTitle :: Text,
    ecAudioKey :: Text,
    ecSeconds :: Int64
  }
  deriving stock (Show, Eq)

-- | One scheduled slot.
data SlotInput = SlotInput
  { siStart :: UTCTime,
    siEnd :: UTCTime,
    siShowTitle :: Text,
    -- | Nothing when the show has no episode for this slot.
    siEpisode :: Maybe EpisodeCandidate
  }
  deriving stock (Show, Eq)

-- | One planned track of a break.
data BreakTrack = BreakTrack
  { btSourceType :: Text,
    btTitle :: Text,
    btAudioKey :: Text,
    btSeconds :: Int64,
    -- | The break item id. Nothing for a station ID.
    btBreakItemId :: Maybe Int64
  }
  deriving stock (Show, Eq)

-- | One break, identified by its boundary.
data BreakInput = BreakInput
  { biBoundary :: UTCTime,
    -- | In play order.
    biTracks :: [BreakTrack]
  }
  deriving stock (Show, Eq)

-- | An ephemeral upload that can fill time.
data EphemeralCandidate = EphemeralCandidate
  { ecdId :: EphemeralUploads.Id,
    ecdTitle :: Text,
    ecdAudioKey :: Text,
    ecdSeconds :: Int64
  }
  deriving stock (Show, Eq)

-- | Everything the timeline needs for one window.
data TimelineInput = TimelineInput
  { tiFrom :: UTCTime,
    tiTo :: UTCTime,
    -- | Not overlapping, in time order.
    tiSlots :: [SlotInput],
    -- | Every break boundary in [tiFrom, tiTo], in time order.
    tiBreaks :: [BreakInput],
    tiEphemeral :: [EphemeralCandidate]
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Output

-- | What an item plays.
data Content
  = EpisodeContent EpisodeCandidate
  | EphemeralContent EphemeralCandidate
  | BreakContent BreakTrack
  | FillerContent
  deriving stock (Show, Eq)

-- | One item of the timeline.
data Item = Item
  { itemContent :: Content,
    -- | When the item starts to air. 'resumeAt' moves it later.
    itemStart :: UTCTime,
    -- | When the timeline plans the item to start. It equals 'itemStart' when
    -- the timeline is built. 'resumeAt' does not change it.
    itemPlannedStart :: UTCTime,
    -- | How long the item airs. Always more than 0.
    itemSeconds :: Int64,
    -- | The offset into the file where play starts.
    itemCueIn :: Int64,
    -- | The offset into the file where play stops. Nothing means the end of
    -- the file.
    itemCueOut :: Maybe Int64
  }
  deriving stock (Show, Eq)

-- | The items from one edge to the next.
data Segment = Segment
  { segStart :: UTCTime,
    segEnd :: UTCTime,
    -- | In play order. They fill the segment with no gap.
    segItems :: [Item]
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Building

-- | Build every segment of the window.
--
-- The edges are every break boundary and every slot start in
-- [tiFrom, tiTo], with duplicates removed. A segment runs from one edge to
-- the next. Time before the first edge and after the last edge gets no
-- segment.
--
-- When two slots are back to back, the boundary is one edge. The segment
-- before it ends with a break.
buildTimeline :: TimelineInput -> [Segment]
buildTimeline input =
  zipWith (buildSegment input breakTracksAt) edges (drop 1 edges)
  where
    inWindow t = tiFrom input <= t && t <= tiTo input
    boundaries = map biBoundary (tiBreaks input)
    slotStarts = map siStart (tiSlots input)
    edges = Set.toAscList (Set.fromList (filter inWindow (boundaries <> slotStarts)))
    breakTracksAt = Map.fromList [(biBoundary b, biTracks b) | b <- tiBreaks input]

-- | Build the segment from one edge to the next.
--
-- A segment ends with a break when its end is a break boundary. The break
-- region is then the last 'breakSeconds' of the segment, and the content
-- region is the time before it. Without a break, the content region is the
-- whole segment.
--
-- An item of zero length never appears. A region of 0 seconds has no items.
buildSegment :: TimelineInput -> Map.Map UTCTime [BreakTrack] -> UTCTime -> UTCTime -> Segment
buildSegment input breakTracksAt start end =
  Segment
    { segStart = start,
      segEnd = end,
      segItems = openLastFiller (filter (\item -> itemSeconds item > 0) (contentItems <> breakItems))
    }
  where
    segmentSeconds = secondsBetween start end
    breakTracks = Map.lookup end breakTracksAt

    -- A segment shorter than a break gives all of its time to the break.
    breakLength =
      case breakTracks of
        Nothing -> 0
        Just _ -> min breakSeconds segmentSeconds
    contentLength = segmentSeconds - breakLength
    breakStart = addSeconds contentLength start

    contentItems = fillContent input start contentLength
    breakItems =
      case breakTracks of
        Nothing -> []
        Just tracks -> fillBreak breakStart breakLength tracks

-- | Remove the cue out of a filler item at the end of a segment.
--
-- Liquidsoap plays the items one after another, not at their planned starts.
-- The stored lengths are rounded up to whole seconds, so the items before the
-- filler usually end a little early. With no cue out, the filler plays on
-- until the swap at the boundary stops it. When the items run long, the swap
-- cuts the filler and not a break track. The length and the planned start of
-- the item do not change.
openLastFiller :: [Item] -> [Item]
openLastFiller items =
  case reverse items of
    lastItem@Item {itemContent = FillerContent} : earlierItems ->
      reverse (lastItem {itemCueOut = Nothing} : earlierItems)
    _ -> items

-- | Fill the content region of a segment.
--
-- The segment belongs to the slot whose [siStart, siEnd) holds the segment
-- start. With no such slot, the segment is unscheduled.
--
-- In a slot with an episode, the episode starts at the region start with cue
-- in 0. Let R be the region length and L the episode length.
--
-- * If L > R, the episode is cut at R, with a cue out of R.
-- * If L == R, the episode plays to its end, with no cue out.
-- * If R - L <= 'fillerSeconds', one filler item fills the gap.
-- * Otherwise, ephemeral fill covers the gap.
--
-- An unscheduled segment, or a slot with no episode, gets ephemeral fill for
-- the whole region.
fillContent :: TimelineInput -> UTCTime -> Int64 -> [Item]
fillContent input regionStart regionSeconds =
  case slotEpisode of
    Nothing -> fillEphemeral (tiEphemeral input) regionStart regionSeconds
    Just episode -> fillWithEpisode (tiEphemeral input) regionStart regionSeconds episode
  where
    holdsStart slot = siStart slot <= regionStart && regionStart < siEnd slot
    slotEpisode = find holdsStart (tiSlots input) >>= siEpisode

-- | Fill a content region that starts with an episode.
fillWithEpisode :: [EphemeralCandidate] -> UTCTime -> Int64 -> EpisodeCandidate -> [Item]
fillWithEpisode pool regionStart regionSeconds episode
  | episodeSeconds >= regionSeconds =
      [placeItem regionStart regionSeconds 0 (EpisodeContent episode) episodeSeconds]
  | gapSeconds <= fillerSeconds =
      [episodeItem, fillerItem gapStart gapSeconds]
  | otherwise =
      episodeItem : fillEphemeral pool gapStart gapSeconds
  where
    -- A stored length below 0 would move later items back in time.
    episodeSeconds = max 0 (ecSeconds episode)
    episodeItem = placeItem regionStart regionSeconds 0 (EpisodeContent episode) episodeSeconds
    gapSeconds = regionSeconds - episodeSeconds
    gapStart = addSeconds episodeSeconds regionStart

-- | Fill a break region with its planned tracks.
--
-- The tracks play in order at full length. The track that crosses the region
-- end is cut there, and later tracks are dropped. When the tracks end early,
-- one filler item fills the rest. A break with no tracks is all filler.
fillBreak :: UTCTime -> Int64 -> [BreakTrack] -> [Item]
fillBreak breakStart breakLength tracks =
  unfoldr step (0, filter (\track -> btSeconds track > 0) tracks)
  where
    step (offset, remainingTracks)
      | offset >= breakLength = Nothing
      | otherwise =
          case remainingTracks of
            [] ->
              Just (fillerItem (addSeconds offset breakStart) (breakLength - offset), (breakLength, []))
            track : laterTracks ->
              Just
                ( placeItem breakStart breakLength offset (BreakContent track) (btSeconds track),
                  (offset + btSeconds track, laterTracks)
                )

-- | The state of an ephemeral fill between two picks.
data EphemeralState = EphemeralState
  { esOffset :: Int64,
    esGen :: StdGen,
    -- | The position in the pool of the last pick.
    esPrevious :: Maybe Int
  }

-- | Fill a region with ephemeral uploads.
--
-- The picks use a 'StdGen' from 'mkStdGen', with the POSIX seconds of the
-- region start as the seed. Each pick is uniform over the pool. A pick that
-- equals the previous pick is drawn again, when the pool has more than one
-- upload. Each pick plays at full length, and the pick that crosses the
-- region end is cut there.
--
-- An upload with no length cannot fill time, so the pool leaves it out. An
-- empty pool gives repeated filler.
fillEphemeral :: [EphemeralCandidate] -> UTCTime -> Int64 -> [Item]
fillEphemeral candidates regionStart regionSeconds =
  case pool of
    [] -> repeatFiller regionStart regionSeconds
    _ -> unfoldr step (EphemeralState 0 (mkStdGen seed) Nothing)
  where
    pool = filter (\candidate -> ecdSeconds candidate > 0) candidates
    poolSize = length pool
    seed = floor (utcTimeToPOSIXSeconds regionStart)

    step state
      | esOffset state >= regionSeconds = Nothing
      | otherwise = Just (item, nextState)
      where
        (index, nextGen) = drawIndex poolSize (esPrevious state) (esGen state)
        candidate = pool !! index
        item = placeItem regionStart regionSeconds (esOffset state) (EphemeralContent candidate) (ecdSeconds candidate)
        nextState =
          EphemeralState
            { esOffset = esOffset state + ecdSeconds candidate,
              esGen = nextGen,
              esPrevious = Just index
            }

-- | Draw a position in the pool. Draw again while the pick equals the
-- previous pick and the pool has more than one upload.
drawIndex :: Int -> Maybe Int -> StdGen -> (Int, StdGen)
drawIndex poolSize previous gen =
  until isFresh drawAgain (draw gen)
  where
    draw = uniformR (0, poolSize - 1)
    drawAgain (_, usedGen) = draw usedGen
    isFresh (index, _) = poolSize <= 1 || Just index /= previous

-- | Fill a region with filler items of 'fillerSeconds' each. The last item is
-- cut at the region end.
repeatFiller :: UTCTime -> Int64 -> [Item]
repeatFiller regionStart regionSeconds =
  unfoldr step 0
  where
    step offset
      | offset >= regionSeconds = Nothing
      | otherwise =
          let itemLength = min fillerSeconds (regionSeconds - offset)
           in Just (fillerItem (addSeconds offset regionStart) itemLength, offset + itemLength)

-- | Place one item at an offset into a region, with cue in 0.
--
-- An item that runs past the region end is cut there, and its cue out is the
-- seconds it airs. An item that fits has no cue out.
placeItem ::
  -- | Region start
  UTCTime ->
  -- | Region length
  Int64 ->
  -- | Offset of the item into the region
  Int64 ->
  Content ->
  -- | Full length of the item
  Int64 ->
  Item
placeItem regionStart regionSeconds offset content fullSeconds
  | fullSeconds > secondsLeft = item secondsLeft (Just secondsLeft)
  | otherwise = item fullSeconds Nothing
  where
    secondsLeft = regionSeconds - offset
    itemStartTime = addSeconds offset regionStart
    item seconds cueOut =
      Item
        { itemContent = content,
          itemStart = itemStartTime,
          itemPlannedStart = itemStartTime,
          itemSeconds = seconds,
          itemCueIn = 0,
          itemCueOut = cueOut
        }

-- | One filler item.
--
-- A filler item has a cue out, because the file can be longer than
-- 'fillerSeconds'. 'openLastFiller' removes it from the last item of a
-- segment.
fillerItem :: UTCTime -> Int64 -> Item
fillerItem start seconds =
  Item
    { itemContent = FillerContent,
      itemStart = start,
      itemPlannedStart = start,
      itemSeconds = seconds,
      itemCueIn = 0,
      itemCueOut = Just seconds
    }

--------------------------------------------------------------------------------
-- Reading

-- | The items of a segment from an instant to the segment end.
--
-- The first item is the one that airs at the instant. It starts at the
-- instant, rounded down to a whole second. Its cue in grows by the elapsed
-- seconds, and its length shrinks by them. Its cue out stays the same.
-- Its planned start stays the original start of the item.
--
-- An instant outside the segment gives no items.
resumeAt :: UTCTime -> Segment -> [Item]
resumeAt instant segment
  | instant < segStart segment || instant >= segEnd segment = []
  | otherwise =
      case dropWhile endsByInstant (segItems segment) of
        [] -> []
        current : laterItems -> resumeItem current : laterItems
  where
    endsByInstant item = addSeconds (itemSeconds item) (itemStart item) <= instant
    resumeItem item =
      let elapsed = secondsBetween (itemStart item) instant
       in item
            { itemStart = addSeconds elapsed (itemStart item),
              itemSeconds = itemSeconds item - elapsed,
              itemCueIn = itemCueIn item + elapsed
            }

-- | The segment whose [segStart, segEnd) holds an instant.
segmentContaining :: UTCTime -> [Segment] -> Maybe Segment
segmentContaining instant =
  find (\segment -> segStart segment <= instant && instant < segEnd segment)

-- | The segment that starts at an instant.
segmentStartingAt :: UTCTime -> [Segment] -> Maybe Segment
segmentStartingAt instant =
  find (\segment -> segStart segment == instant)

--------------------------------------------------------------------------------
-- Time

-- | Add whole seconds to an instant.
addSeconds :: Int64 -> UTCTime -> UTCTime
addSeconds seconds = addUTCTime (fromIntegral seconds)

-- | The whole seconds from one instant to a later one, rounded down.
secondsBetween :: UTCTime -> UTCTime -> Int64
secondsBetween start end = floor (diffUTCTime end start)
