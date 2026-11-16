-- | Tests for the problems of the day timeline.
module Domain.DayTimelineSpec where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day, LocalTime (..), TimeOfDay (..), UTCTime, fromGregorian)
import Domain.DayTimeline
import Domain.Timeline
import Domain.Types.Slug (Slug (..))
import Domain.Types.Timezone (pacificToUtc)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.Episodes qualified as Episodes
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "Domain.DayTimeline" $ do
    it "does not count filler in a break" breakFillerIgnored
    it "flags long filler when the ephemeral pool is empty" longFillerFlagged
    it "does not flag the short filler after an episode" shortFillerIgnored
    it "gives one problem for a slot with no episode over two segments" oneProblemPerSlot
    it "flags a dropped slot on the segment where it opens" droppedSlotFlagged
    it "keeps only the scheduled segments" keepsScheduled

--------------------------------------------------------------------------------
-- Fixtures

-- | January 10, 2025.
day :: Day
day = fromGregorian 2025 1 10

-- | An instant on the test day, in Pacific time.
pacificAt :: Int -> Int -> UTCTime
pacificAt hour minute = pacificToUtc (LocalTime day (TimeOfDay hour minute 0))

-- | The hourly break boundaries from 12:00 to 15:00.
boundaries :: [UTCTime]
boundaries = [pacificAt hour 0 | hour <- [12 .. 15]]

-- | A slot window with no episode.
window :: Text -> UTCTime -> UTCTime -> Episodes.SlotWindow
window title start end =
  Episodes.SlotWindow
    { Episodes.swStart = start,
      Episodes.swEnd = end,
      Episodes.swShowTitle = title,
      Episodes.swShowSlug = Slug title,
      Episodes.swEpisode = Nothing
    }

-- | A slot window with an episode of the given length.
windowWithEpisode :: Text -> UTCTime -> UTCTime -> Int64 -> Episodes.SlotWindow
windowWithEpisode title start end seconds =
  (window title start end)
    { Episodes.swEpisode =
        Just
          Episodes.SlotEpisode
            { Episodes.seId = Episodes.Id 1,
              Episodes.seEpisodeNumber = Episodes.EpisodeNumber 1,
              Episodes.seAudioFilePath = "audio/episodes/test.mp3",
              Episodes.seDurationSeconds = seconds
            }
    }

-- | An ephemeral upload of ten minutes.
ephemeral :: EphemeralCandidate
ephemeral =
  EphemeralCandidate
    { ecdId = EphemeralUploads.Id 1,
      ecdTitle = "Ephemeral",
      ecdAudioKey = "audio/ephemeral/test.mp3",
      ecdSeconds = 600
    }

-- | The timeline from 12:00 to 15:00. Every break is empty, so each one is
-- all filler.
timelineOf :: [Episodes.SlotWindow] -> [Episodes.SlotWindow] -> [EphemeralCandidate] -> TimelineData
timelineOf kept dropped pool =
  TimelineData
    { tdSegments = buildTimeline input,
      tdSlots = kept,
      tdDroppedSlots = dropped,
      tdBreakBoundaries = boundaries
    }
  where
    input =
      TimelineInput
        { tiFrom = pacificAt 12 0,
          tiTo = pacificAt 15 0,
          tiSlots = map toSlotInput kept,
          tiBreaks = [BreakInput boundary [] | boundary <- boundaries],
          tiEphemeral = pool
        }
    toSlotInput w =
      SlotInput
        { siStart = Episodes.swStart w,
          siEnd = Episodes.swEnd w,
          siShowTitle = Episodes.swShowTitle w,
          siEpisode = toCandidate w <$> Episodes.swEpisode w
        }
    toCandidate w episode =
      EpisodeCandidate
        { ecId = Episodes.seId episode,
          ecShowTitle = Episodes.swShowTitle w,
          ecShowSlug = Episodes.swShowSlug w,
          ecEpisodeNumber = Episodes.seEpisodeNumber episode,
          ecAudioKey = Episodes.seAudioFilePath episode,
          ecSeconds = Episodes.seDurationSeconds episode
        }

-- | The flags of the problems of a timeline.
problemFlags :: TimelineData -> [Flag]
problemFlags = map problemFlag . problems . segmentViews

--------------------------------------------------------------------------------
-- Cases

breakFillerIgnored :: Expectation
breakFillerIgnored = do
  let timeline = timelineOf [] [] [ephemeral]
  map (contentFillerSeconds boundaries) (tdSegments timeline) `shouldBe` [0, 0, 0]
  problemFlags timeline `shouldBe` []

longFillerFlagged :: Expectation
longFillerFlagged =
  problemFlags (timelineOf [] [] []) `shouldBe` replicate 3 (LongFiller 3480)

shortFillerIgnored :: Expectation
shortFillerIgnored = do
  -- The content region is 3480 seconds, so the episode leaves 60 seconds.
  let slot = windowWithEpisode "Morning" (pacificAt 12 0) (pacificAt 13 0) 3420
      timeline = timelineOf [slot] [] [ephemeral]
  map (contentFillerSeconds boundaries) (take 1 (tdSegments timeline)) `shouldBe` [60]
  problemFlags timeline `shouldBe` []

oneProblemPerSlot :: Expectation
oneProblemPerSlot = do
  let slot = window "Dub Hour" (pacificAt 12 0) (pacificAt 14 0)
      views = segmentViews (timelineOf [slot] [] [ephemeral])
  map svFlags (take 2 views) `shouldBe` replicate 2 [NoPlayableEpisode slot]
  map (\p -> (problemSegmentStart p, problemFlag p)) (problems views)
    `shouldBe` [(pacificAt 12 0, NoPlayableEpisode slot)]

droppedSlotFlagged :: Expectation
droppedSlotFlagged = do
  let airing = windowWithEpisode "Early" (pacificAt 12 0) (pacificAt 13 0) 3480
      dropped = window "Late" (pacificAt 12 30) (pacificAt 13 30)
      timeline = timelineOf [airing] [dropped] [ephemeral]
  map (\p -> (problemSegmentStart p, problemFlag p)) (problems (segmentViews timeline))
    `shouldBe` [(pacificAt 12 0, DroppedSlot dropped (Just airing))]

keepsScheduled :: Expectation
keepsScheduled = do
  let slot = windowWithEpisode "Early" (pacificAt 13 0) (pacificAt 14 0) 3480
      views = scheduled (segmentViews (timelineOf [slot] [] []))
  map (segStart . svSegment) views `shouldBe` [pacificAt 13 0]
  map problemFlag (problems views) `shouldBe` []
