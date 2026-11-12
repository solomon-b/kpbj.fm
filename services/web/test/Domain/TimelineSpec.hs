-- | Tests for the pure playout timeline.
module Domain.TimelineSpec where

--------------------------------------------------------------------------------

import Control.Monad (when)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.List (nub, sort)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (Day, LocalTime (..), NominalDiffTime, TimeOfDay (..), UTCTime, addUTCTime, diffUTCTime, fromGregorian)
import Domain.BreakPlanner (breakSeconds)
import Domain.Timeline
import Domain.Types.Timezone (pacificToUtc)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.Episodes qualified as Episodes
import Hedgehog (Gen, PropertyT, annotateShow, assert, cover, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------
-- Fixtures

-- | January 10, 2025.
day :: Day
day = fromGregorian 2025 1 10

-- | An instant on the test day, in Pacific time.
pacificAt :: Int -> Int -> UTCTime
pacificAt hour minute = pacificToUtc (LocalTime day (TimeOfDay hour minute 0))

-- | Add whole seconds to an instant.
plusSeconds :: Int64 -> UTCTime -> UTCTime
plusSeconds seconds = addUTCTime (fromIntegral seconds)

-- | The whole seconds from one instant to a later one.
secondsFrom :: UTCTime -> UTCTime -> Int64
secondsFrom start end = floor (diffUTCTime end start)

-- | The instant an item ends.
itemEnd :: Item -> UTCTime
itemEnd item = plusSeconds (itemSeconds item) (itemStart item)

-- | An episode of the given length.
episodeOf :: Int64 -> EpisodeCandidate
episodeOf seconds =
  EpisodeCandidate
    { ecId = Episodes.Id 1,
      ecShowTitle = "Test Show",
      ecAudioKey = "audio/episodes/test.mp3",
      ecSeconds = seconds
    }

-- | An ephemeral upload of the given length.
ephemeralOf :: Int64 -> Int64 -> EphemeralCandidate
ephemeralOf uploadId seconds =
  EphemeralCandidate
    { ecdId = EphemeralUploads.Id uploadId,
      ecdTitle = "Ephemeral",
      ecdAudioKey = "audio/ephemeral/test.mp3",
      ecdSeconds = seconds
    }

-- | A station ID of the given length.
stationIdTrack :: Int64 -> BreakTrack
stationIdTrack seconds =
  BreakTrack
    { btSourceType = "station_id",
      btTitle = "Station ID",
      btAudioKey = "audio/station-ids/test.mp3",
      btSeconds = seconds,
      btBreakItemId = Nothing
    }

-- | A PSA of the given length.
psaTrack :: Int64 -> Int64 -> BreakTrack
psaTrack itemId seconds =
  BreakTrack
    { btSourceType = "psa",
      btTitle = "PSA",
      btAudioKey = "audio/break-items/test.mp3",
      btSeconds = seconds,
      btBreakItemId = Just itemId
    }

-- | One hour slot from 16:00 to 17:00 Pacific.
hourSlot :: Maybe EpisodeCandidate -> SlotInput
hourSlot episode =
  SlotInput
    { siStart = pacificAt 16 0,
      siEnd = pacificAt 17 0,
      siShowTitle = "Test Show",
      siEpisode = episode
    }

-- | The 16:00 slot and the break at 17:00, with the given ephemeral pool.
hourInput :: Maybe EpisodeCandidate -> [BreakTrack] -> [EphemeralCandidate] -> TimelineInput
hourInput episode breakTracks pool =
  TimelineInput
    { tiFrom = pacificAt 16 0,
      tiTo = pacificAt 17 0,
      tiSlots = [hourSlot episode],
      tiBreaks = [BreakInput (pacificAt 17 0) breakTracks],
      tiEphemeral = pool
    }

-- | The break tracks of the examples: a station ID and two PSAs.
exampleBreak :: [BreakTrack]
exampleBreak = [stationIdTrack 10, psaTrack 1 30, psaTrack 2 30]

-- | The one segment of an input, or a test failure.
onlySegment :: TimelineInput -> IO Segment
onlySegment timelineInput =
  case buildTimeline timelineInput of
    [segment] -> pure segment
    segments -> expectationFailure ("expected one segment, got " <> show segments) >> fail "no segment"

-- | A short name for the content of an item, to compare item lists.
contentName :: Item -> Text
contentName item =
  case itemContent item of
    EpisodeContent _ -> "episode"
    EphemeralContent _ -> "ephemeral"
    BreakContent track -> btSourceType track
    FillerContent -> "filler"

-- | The content names and lengths of a segment.
shape :: Segment -> [(Text, Int64)]
shape segment = [(contentName item, itemSeconds item) | item <- segItems segment]

-- | True when an item plays a break track.
isBreakItem :: Item -> Bool
isBreakItem item =
  case itemContent item of
    BreakContent _ -> True
    _ -> False

--------------------------------------------------------------------------------
-- Generators

-- | The start of the generated window, midnight Pacific on the test day.
windowStart :: UTCTime
windowStart = pacificAt 0 0

-- | The length of the generated window, in hours.
windowHours :: Int64
windowHours = 48

-- | Half an hour.
halfHour :: NominalDiffTime
halfHour = 1800

-- | Two days of slots, gaps, breaks, and ephemeral uploads.
genTimelineInput :: Gen TimelineInput
genTimelineInput = do
  slots <- genSlots windowStart
  breakTracks <- traverse (const genBreakTracks) (breakBoundaries slots)
  pool <- genPool
  pure
    TimelineInput
      { tiFrom = windowStart,
        tiTo = windowEnd,
        tiSlots = slots,
        tiBreaks = zipWith BreakInput (breakBoundaries slots) breakTracks,
        tiEphemeral = pool
      }
  where
    windowEnd = plusSeconds (windowHours * 3600) windowStart

-- | Slots of 30, 60, 90, or 120 minutes, with gaps, from the given instant to
-- the window end.
--
-- Every length is a whole number of half hours, so each slot starts at :00
-- or :30. Some slots cross midnight. The last slot can run past the window
-- end.
genSlots :: UTCTime -> Gen [SlotInput]
genSlots from
  | from >= plusSeconds (windowHours * 3600) windowStart = pure []
  | otherwise = do
      gapHalfHours <- Gen.int (Range.constant 0 4)
      slotHalfHours <- Gen.int (Range.constant 1 4)
      let slotStart = addUTCTime (fromIntegral gapHalfHours * halfHour) from
          slotEnd = addUTCTime (fromIntegral slotHalfHours * halfHour) slotStart
          slotSeconds = fromIntegral slotHalfHours * 1800
      episode <- genEpisode slotSeconds
      laterSlots <- genSlots slotEnd
      let slot =
            SlotInput
              { siStart = slotStart,
                siEnd = slotEnd,
                siShowTitle = "Generated Show",
                siEpisode = episode
              }
      pure (slot : laterSlots)

-- | An episode for a slot of the given length, 7 times in 10.
--
-- For 9 episodes in 10, the length runs from 5 minutes short to 1 minute
-- long. This covers episodes that were uploaded before the length rule.
--
-- For 1 episode in 10, the length runs from 0 to 10 minutes short of the
-- slot. Ephemeral fill then covers the gap after the episode.
genEpisode :: Int64 -> Gen (Maybe EpisodeCandidate)
genEpisode slotSeconds =
  Gen.frequency
    [ (3, pure Nothing),
      (7, Just . episodeOf <$> genEpisodeSeconds)
    ]
  where
    genEpisodeSeconds =
      Gen.frequency
        [ (9, Gen.int64 (Range.constant (slotSeconds - 300) (slotSeconds + 60))),
          (1, Gen.int64 (Range.constant 0 (slotSeconds - 600)))
        ]

-- | Every break boundary in the window, in time order.
--
-- A show break airs at each slot end. An automation break airs at each top
-- of the hour that no slot spans.
breakBoundaries :: [SlotInput] -> [UTCTime]
breakBoundaries slots =
  Set.toAscList (Set.fromList (filter inWindow (slotEnds <> openHours)))
  where
    windowEnd = plusSeconds (windowHours * 3600) windowStart
    inWindow t = windowStart <= t && t <= windowEnd
    slotEnds = map siEnd slots
    hours = [plusSeconds (h * 3600) windowStart | h <- [0 .. windowHours]]
    openHours = filter (\hour -> not (any (spans hour) slots)) hours
    spans t slot = siStart slot < t && t < siEnd slot

-- | The tracks of one break: 0 to 6 PSAs of 5 to 30 seconds. One break in 10
-- opens with a 150 second station ID.
genBreakTracks :: Gen [BreakTrack]
genBreakTracks = do
  longStationId <- Gen.frequency [(9, pure []), (1, pure [stationIdTrack 150])]
  psaCount <- Gen.int64 (Range.linear 0 6)
  psas <- traverse (\itemId -> psaTrack itemId <$> Gen.int64 (Range.constant 5 30)) [1 .. psaCount]
  pure (longStationId <> psas)

-- | The same input over a narrower window, from one whole hour to a later one
-- after the window start.
--
-- It keeps the breaks in the narrower window, so the list still holds every
-- break boundary there. It keeps the slots that overlap the narrower window.
narrowWindow :: Int64 -> Int64 -> TimelineInput -> TimelineInput
narrowWindow fromHour toHour timelineInput =
  timelineInput
    { tiFrom = narrowFrom,
      tiTo = narrowTo,
      tiSlots = filter overlaps (tiSlots timelineInput),
      tiBreaks = filter (inNarrowWindow . biBoundary) (tiBreaks timelineInput)
    }
  where
    narrowFrom = plusSeconds (fromHour * 3600) windowStart
    narrowTo = plusSeconds (toHour * 3600) windowStart
    inNarrowWindow t = narrowFrom <= t && t <= narrowTo
    overlaps slot = siStart slot <= narrowTo && narrowFrom < siEnd slot

-- | 0 to 5 ephemeral uploads of 1 to 7200 seconds.
genPool :: Gen [EphemeralCandidate]
genPool = do
  uploadCount <- Gen.int64 (Range.linear 0 5)
  traverse (\uploadId -> ephemeralOf uploadId <$> Gen.int64 (Range.constant 1 7200)) [1 .. uploadCount]

--------------------------------------------------------------------------------
-- Checks

-- | Check that a list of items starts at an instant, has no gaps, holds no
-- empty item, and ends at another instant.
checkContiguous :: UTCTime -> UTCTime -> [Item] -> PropertyT IO ()
checkContiguous start end items = do
  annotateShow items
  map itemStart (take 1 items) === [start]
  for_ items $ \item -> assert (itemSeconds item > 0)
  map itemEnd items === map itemStart (drop 1 items) <> [end]

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Domain.Timeline" $ do
  describe "invariants" $ do
    it "tiles the window with contiguous segments" $ hedgehog $ do
      timelineInput <- forAll genTimelineInput
      let segments = buildTimeline timelineInput
      assert (not (null segments))
      for_ (zip segments (drop 1 segments)) $ \(earlier, later) ->
        segEnd earlier === segStart later

    it "fills every segment exactly" $ hedgehog $ do
      timelineInput <- forAll genTimelineInput
      for_ (buildTimeline timelineInput) $ \segment ->
        checkContiguous (segStart segment) (segEnd segment) (segItems segment)

    it "plans every item at its start" $ hedgehog $ do
      timelineInput <- forAll genTimelineInput
      for_ (buildTimeline timelineInput) $ \segment ->
        for_ (segItems segment) $ \item ->
          itemPlannedStart item === itemStart item

    it "plays every break in the last 120 seconds before its boundary" $ hedgehog $ do
      timelineInput <- forAll genTimelineInput
      let boundaries = Set.fromList (map biBoundary (tiBreaks timelineInput))
      for_ (buildTimeline timelineInput) $ \segment ->
        when (Set.member (segEnd segment) boundaries) $ do
          let breakStart = plusSeconds (negate breakSeconds) (segEnd segment)
              breakItems = filter (\item -> itemStart item >= breakStart) (segItems segment)
              crosses item = itemStart item < breakStart && breakStart < itemEnd item
          annotateShow segment
          sum (map itemSeconds breakItems) === breakSeconds
          filter crosses (segItems segment) === []

    it "gives the same segment for any window that contains it" $ hedgehog $ do
      timelineInput <- forAll genTimelineInput
      fromHour <- forAll (Gen.int64 (Range.constant 0 (windowHours - 1)))
      toHour <- forAll (Gen.int64 (Range.constant (fromHour + 1) windowHours))
      let fullSegments = buildTimeline timelineInput
          narrowSegments = buildTimeline (narrowWindow fromHour toHour timelineInput)
          fullSegmentAt start = filter (\segment -> segStart segment == start) fullSegments
      -- A narrow window with no segment checks nothing.
      cover 30 "has segments" (not (null narrowSegments))
      for_ narrowSegments $ \segment ->
        fullSegmentAt (segStart segment) === [segment]

    it "resumes at any instant and still ends at the segment end" $ hedgehog $ do
      timelineInput <- forAll genTimelineInput
      let segments = buildTimeline timelineInput
      segmentIndex <- forAll (Gen.int (Range.constant 0 (length segments - 1)))
      let segment = segments !! segmentIndex
          segmentSeconds = secondsFrom (segStart segment) (segEnd segment)
      offset <- forAll (Gen.int64 (Range.constant 0 (segmentSeconds - 1)))
      let instant = plusSeconds offset (segStart segment)
      checkContiguous instant (segEnd segment) (resumeAt instant segment)

    it "has edges only at break boundaries and slot starts" $ hedgehog $ do
      timelineInput <- forAll genTimelineInput
      let edges = Set.fromList (map biBoundary (tiBreaks timelineInput) <> map siStart (tiSlots timelineInput))
          segments = buildTimeline timelineInput
      for_ segments $ \segment -> do
        assert (Set.member (segStart segment) edges)
        assert (Set.member (segEnd segment) edges)
      nub (sort (map segStart segments)) === map segStart segments

    it "lets only the filler at the end of a segment play until the swap" $ hedgehog $ do
      timelineInput <- forAll genTimelineInput
      for_ (buildTimeline timelineInput) $ \segment -> do
        let items = segItems segment
            isFiller item = case itemContent item of
              FillerContent -> True
              _ -> False
            earlierFillers = filter isFiller (take (length items - 1) items)
            lastFillers = filter isFiller (drop (length items - 1) items)
        annotateShow segment
        map itemCueOut earlierFillers === map (Just . itemSeconds) earlierFillers
        map itemCueOut lastFillers === map (const Nothing) lastFillers

    it "never plays the same upload twice in a row when the pool has more than one" $ hedgehog $ do
      timelineInput <- forAll genTimelineInput
      when (length (tiEphemeral timelineInput) > 1) $
        for_ (buildTimeline timelineInput) $ \segment -> do
          let uploadIds = [ecdId upload | EphemeralContent upload <- map itemContent (segItems segment)]
          annotateShow uploadIds
          filter (uncurry (==)) (zip uploadIds (drop 1 uploadIds)) === []

  describe "examples" $ do
    it "plays an episode, then filler, then the break" $ do
      segment <- onlySegment (hourInput (Just (episodeOf 3420)) exampleBreak [])
      shape segment
        `shouldBe` [ ("episode", 3420),
                     ("filler", 60),
                     ("station_id", 10),
                     ("psa", 30),
                     ("psa", 30),
                     ("filler", 50)
                   ]
      map itemCueOut (segItems segment)
        `shouldBe` [Nothing, Just 60, Nothing, Nothing, Nothing, Nothing]

    it "cuts an episode that runs past the break start" $ do
      segment <- onlySegment (hourInput (Just (episodeOf 3600)) exampleBreak [])
      map itemCueOut (take 1 (segItems segment)) `shouldBe` [Just 3480]
      map itemSeconds (take 1 (segItems segment)) `shouldBe` [3480]

    it "fills a long gap with ephemeral tracks" $ do
      segment <- onlySegment (hourInput (Just (episodeOf 1800)) exampleBreak [ephemeralOf 1 600])
      let afterEpisode = drop 1 (segItems segment)
          ephemeralItems = take 3 afterEpisode
      map contentName ephemeralItems `shouldBe` ["ephemeral", "ephemeral", "ephemeral"]
      map itemSeconds ephemeralItems `shouldBe` [600, 600, 480]
      map itemCueOut ephemeralItems `shouldBe` [Nothing, Nothing, Just 480]
      all isBreakItem (take 3 (drop 3 afterEpisode)) `shouldBe` True

    it "treats a slot with no episode as unscheduled time" $ do
      segment <- onlySegment (hourInput Nothing exampleBreak [ephemeralOf 1 600, ephemeralOf 2 900])
      let contentItems = filter (\item -> itemStart item < pacificAt 16 58) (segItems segment)
      null contentItems `shouldBe` False
      map contentName contentItems `shouldBe` map (const "ephemeral") contentItems
      sum (map itemSeconds contentItems) `shouldBe` 3480

    it "repeats filler when the pool is empty" $ do
      let timelineInput =
            TimelineInput
              { tiFrom = pacificAt 16 0,
                tiTo = pacificAt 17 0,
                tiSlots = [],
                tiBreaks = [BreakInput (pacificAt 16 0) [], BreakInput (pacificAt 17 0) []],
                tiEphemeral = []
              }
      segment <- onlySegment timelineInput
      shape segment `shouldBe` [("filler", seconds) | seconds <- replicate 19 180 <> [60, 120]]
      map itemCueOut (segItems segment) `shouldBe` map Just (replicate 19 180 <> [60]) <> [Nothing]

    it "cuts a break that runs past its boundary" $ do
      segment <- onlySegment (hourInput (Just (episodeOf 3480)) [stationIdTrack 150, psaTrack 1 30] [])
      let breakItems = filter isBreakItem (segItems segment)
      map itemSeconds breakItems `shouldBe` [120]
      map itemCueOut breakItems `shouldBe` [Just 120]

    it "makes a slot start at :30 an edge with no break" $ do
      let laterSlot =
            SlotInput
              { siStart = pacificAt 16 30,
                siEnd = pacificAt 17 30,
                siShowTitle = "Test Show",
                siEpisode = Just (episodeOf 3420)
              }
          timelineInput =
            TimelineInput
              { tiFrom = pacificAt 16 0,
                tiTo = pacificAt 18 0,
                tiSlots = [laterSlot],
                tiBreaks =
                  [ BreakInput (pacificAt 16 0) exampleBreak,
                    BreakInput (pacificAt 17 30) exampleBreak,
                    BreakInput (pacificAt 18 0) exampleBreak
                  ],
                tiEphemeral = []
              }
          segments = buildTimeline timelineInput
      map segStart segments `shouldBe` [pacificAt 16 0, pacificAt 16 30, pacificAt 17 30]
      map segEnd segments `shouldBe` [pacificAt 16 30, pacificAt 17 30, pacificAt 18 0]
      case segments of
        firstSegment : _ -> filter isBreakItem (segItems firstSegment) `shouldBe` []
        [] -> expectationFailure "expected segments"

    it "resumes an item with the elapsed seconds added to its cue in" $ do
      segment <- onlySegment (hourInput (Just (episodeOf 3420)) exampleBreak [])
      case resumeAt (pacificAt 16 20) segment of
        firstItem : _ -> do
          itemStart firstItem `shouldBe` pacificAt 16 20
          itemCueIn firstItem `shouldBe` 1200
          itemSeconds firstItem `shouldBe` 2220
          itemCueOut firstItem `shouldBe` Nothing
        [] -> expectationFailure "expected items"

    it "resumes at a fraction of a second and still ends at the segment end" $ do
      segment <- onlySegment (hourInput (Just (episodeOf 3420)) exampleBreak [])
      let instant = addUTCTime 0.5 (pacificAt 16 20)
          items = resumeAt instant segment
      map itemStart (take 1 items) `shouldBe` [pacificAt 16 20]
      map itemCueIn (take 1 items) `shouldBe` [1200]
      map itemEnd items `shouldBe` map itemStart (drop 1 items) <> [segEnd segment]
      segEnd segment `shouldBe` pacificAt 17 0

    it "places no item for a 0 second upload or a 0 second break track" $ do
      let pool = [ephemeralOf 1 0, ephemeralOf 2 600]
          breakTracks = [stationIdTrack 0, psaTrack 1 30]
      segment <- onlySegment (hourInput Nothing breakTracks pool)
      let uploadIds = [ecdId upload | EphemeralContent upload <- map itemContent (segItems segment)]
          breakShape = [(contentName item, itemSeconds item) | item <- segItems segment, isBreakItem item]
      filter (\item -> itemSeconds item <= 0) (segItems segment) `shouldBe` []
      filter (== EphemeralUploads.Id 1) uploadIds `shouldBe` []
      null uploadIds `shouldBe` False
      breakShape `shouldBe` [("psa", 30)]

    it "keeps the planned start of a resumed item" $ do
      segment <- onlySegment (hourInput (Just (episodeOf 3420)) exampleBreak [])
      case resumeAt (pacificAt 16 20) segment of
        firstItem : _ -> do
          itemStart firstItem `shouldBe` pacificAt 16 20
          itemPlannedStart firstItem `shouldBe` pacificAt 16 0
        [] -> expectationFailure "expected items"

    it "resumes nothing outside the segment" $ do
      segment <- onlySegment (hourInput (Just (episodeOf 3420)) exampleBreak [])
      resumeAt (pacificAt 17 0) segment `shouldBe` []
      resumeAt (pacificAt 15 59) segment `shouldBe` []

    it "finds the segment that holds an instant or starts at it" $ do
      let segments = buildTimeline (hourInput (Just (episodeOf 3420)) exampleBreak [])
      fmap segStart (segmentContaining (pacificAt 16 30) segments) `shouldBe` Just (pacificAt 16 0)
      fmap segStart (segmentContaining (pacificAt 17 0) segments) `shouldBe` Nothing
      fmap segStart (segmentStartingAt (pacificAt 16 0) segments) `shouldBe` Just (pacificAt 16 0)
      fmap segStart (segmentStartingAt (pacificAt 16 30) segments) `shouldBe` Nothing
