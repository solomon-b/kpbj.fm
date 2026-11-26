{-# LANGUAGE PackageImports #-}

-- | Tests for collecting the timeline inputs.
--
-- The schedule has one show, so the day has the automation breaks and no
-- others. The break items are one station ID and one PSA.
module Effects.TimelineSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time
  ( Day,
    LocalTime (..),
    TimeOfDay (..),
    UTCTime,
    addDays,
    fromGregorian,
  )
import Data.Time.Calendar (DayOfWeek (..))
import Domain.DayTimeline (TimelineData (..))
import Domain.Timeline
import Domain.Types.Slug (Slug (..))
import Domain.Types.Timezone (pacificToUtc, startOfPacificDay)
import Effects.BreakPlan (forecastDay)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.BreakPlans qualified as BreakPlans
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Timeline
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEpisode, insertTestShowWithSchedule, insertTestStationId, insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec
import "kpbj-web" App.Monad (AppM)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Timeline" $ do
      it "returns a segment for an episode slot with its stored break" storedBreak
      it "uses the forecast for a future day" futureForecast
      it "drops a slot window that overlaps an earlier one" . const $ dropsOverlap
      it "does not build a plan for a past day in the window" noPlanForPastDay
      it "gives the 00:00 break the stored tracks of the day before" midnightBreak
      it "gives a day the same segments as the window around an instant" dayMatchesAround
      it "keeps a segment that crosses midnight in both days" crossesMidnight
      it "covers the whole day with segments" (coversWholeDay (fromGregorian 2025 1 6))
      it "covers a 23 hour day with segments" (coversWholeDay (fromGregorian 2025 3 9))
      it "covers a 25 hour day with segments" (coversWholeDay (fromGregorian 2025 11 2))
      it "airs an earlier episode in a slot with no episode" earlierEpisodeRerun
      it "groups the rerun candidates by show and keeps the row order" . const $ candidatesKeepRowOrder
      it "gives a window with no episode a rerun from the candidates" . const $ rerunFromCandidates
      it "gives the primary and the replay window the same rerun" . const $ replaySharesRerun
      it "gives two slots of one show on one date their own reruns" . const $ secondSlotOwnRerun
      it "keeps the new episode when the window has one" . const $ newEpisodeWins
      it "leaves out a candidate on or after the air date of the slot" . const $ laterEpisodesLeftOut
      it "gives no rerun to a show with no candidates" . const $ noCandidatesNoRerun
      it "does not use the candidates of a different show" . const $ otherShowCandidatesUnused

--------------------------------------------------------------------------------
-- Fixtures

-- | Monday, January 6, 2025. Clear of both daylight saving changes.
day :: Day
day = fromGregorian 2025 1 6

-- | A Pacific clock time on 'day', or on a later day.
at :: Integer -> TimeOfDay -> UTCTime
at daysLater time = pacificToUtc (LocalTime (addDays daysLater day) time)

-- | Insert a staff user, one 10 second station ID, and one 30 second PSA.
-- Returns the user.
seedBreakItems :: AppM User.Id
seedBreakItems = do
  userInsert <- liftIO $ mkUserInsert "timeline" UserMetadata.Staff
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    _ <-
      insertTestStationId
        StationIds.Insert
          { siiTitle = "Station ID",
            siiAudioFilePath = "audio/station-ids/2025/01/06/id_2025-01-06_abc.mp3",
            siiMimeType = "audio/mpeg",
            siiFileSize = 512,
            siiDurationSeconds = 10,
            siiCreatorId = userId
          }
    _ <-
      unwrapInsert $
        BreakItems.insertBreakItem
          BreakItems.Insert
            { biiTitle = "Library Hours",
              biiCategory = BreakItems.Psa,
              biiUnderwriterId = Nothing,
              biiShowId = Nothing,
              biiReviewStatus = BreakItems.Approved,
              biiAudioFilePath = "audio/break-items/2025/01/01/library_2025-01-01_def.mp3",
              biiMimeType = "audio/mpeg",
              biiFileSize = 1024,
              biiDurationSeconds = 30,
              biiStartsOn = fromGregorian 2025 1 1,
              biiEndsOn = Nothing,
              biiCreatorId = userId
            }
    pure userId
  case result of
    Left err -> error ("Setup failed: " <> show err)
    Right userId -> pure userId

-- | Insert a show with a Monday 14:00 to 15:00 slot and one episode that airs
-- in it. Returns the episode id.
seedEpisodeSlot :: User.Id -> AppM Episodes.Id
seedEpisodeSlot = seedEpisodeSlotOn day

-- | Insert the show and the slot of 'seedEpisodeSlot', with one episode for
-- the given air date.
seedEpisodeSlotOn :: Day -> User.Id -> AppM Episodes.Id
seedEpisodeSlotOn airDate userId = do
  let showInsert = Shows.Insert "Timeline Show" (Slug "timeline-show") Nothing Nothing Shows.Active
      template =
        ShowSchedule.ScheduleTemplateInsert
          { stiShowId = Shows.Id 0,
            stiDayOfWeek = Monday,
            stiWeeksOfMonth = [1, 2, 3, 4, 5],
            stiStartTime = TimeOfDay 14 0 0,
            stiEndTime = TimeOfDay 15 0 0,
            stiTimezone = "America/Los_Angeles",
            stiReplayStartTime = Nothing
          }
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (showId, templateId) <- insertTestShowWithSchedule showInsert template
    _ <-
      unwrapInsert $
        ShowSchedule.insertValidity $
          ShowSchedule.ValidityInsert templateId (fromGregorian 2024 12 1) Nothing
    insertTestEpisode
      Episodes.Insert
        { Episodes.eiId = showId,
          Episodes.eiDescription = Just "Timeline test episode",
          Episodes.eiAudioFilePath = Just "audio/episodes/2025/01/06/timeline_2025-01-06_abc.mp3",
          Episodes.eiAudioFileSize = Just 1024,
          Episodes.eiAudioMimeType = Just "audio/mpeg",
          Episodes.eiDurationSeconds = Just 3500,
          Episodes.eiArtworkUrl = Nothing,
          Episodes.eiScheduleTemplateId = Just templateId,
          Episodes.eiAirDate = Just airDate,
          Episodes.eiCreatedBy = userId
        }
  case result of
    Left err -> error ("Setup failed: " <> show err)
    Right episodeId -> pure episodeId

-- | Insert a show with a Monday 23:00 to 01:00 slot and no episode. Midnight
-- is inside the slot, so it has no break.
seedOvernightSlot :: AppM ()
seedOvernightSlot = do
  let showInsert = Shows.Insert "Night Show" (Slug "night-show") Nothing Nothing Shows.Active
      template =
        ShowSchedule.ScheduleTemplateInsert
          { stiShowId = Shows.Id 0,
            stiDayOfWeek = Monday,
            stiWeeksOfMonth = [1, 2, 3, 4, 5],
            stiStartTime = TimeOfDay 23 0 0,
            stiEndTime = TimeOfDay 1 0 0,
            stiTimezone = "America/Los_Angeles",
            stiReplayStartTime = Nothing
          }
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (_, templateId) <- insertTestShowWithSchedule showInsert template
    unwrapInsert $
      ShowSchedule.insertValidity $
        ShowSchedule.ValidityInsert templateId (fromGregorian 2024 12 1) Nothing
  case result of
    Left err -> error ("Setup failed: " <> show err)
    Right _ -> pure ()

-- | Build the timeline of a day, failing the test on a database error.
dayOf :: UTCTime -> Day -> AppM [Segment]
dayOf now d =
  dayTimeline now d >>= \case
    Left err -> error ("Timeline failed: " <> show err)
    Right dayData -> pure (tdSegments dayData)

-- | Build the timeline, failing the test on a database error.
timeline :: UTCTime -> UTCTime -> AppM [Segment]
timeline now centre =
  timelineAround now centre >>= \case
    Left err -> error ("Timeline failed: " <> show err)
    Right segments -> pure segments

-- | The planned tracks as title and break item id.
plannedPairs :: [BreakPlans.PlannedTrack] -> [(Text, Maybe Int64)]
plannedPairs tracks = [(t.plTitle, t.plBreakItemId) | t <- tracks]

-- | The break tracks of a segment, as title and break item id.
breakTracksOf :: Segment -> [(Text, Maybe Int64)]
breakTracksOf segment = [(btTitle t, btBreakItemId t) | Item {itemContent = BreakContent t} <- segItems segment]

--------------------------------------------------------------------------------
-- Cases

storedBreak :: TestDBConfig -> IO ()
storedBreak cfg =
  bracketAppM cfg $ do
    userId <- seedBreakItems
    episodeId <- seedEpisodeSlot userId
    segments <- timeline (at 0 (TimeOfDay 20 0 0)) (at 0 (TimeOfDay 14 40 0))
    let boundary = at 0 (TimeOfDay 15 0 0)
    stored <-
      execQuery (BreakPlans.tracksBetween boundary boundary) >>= \case
        Left err -> error ("Read failed: " <> show err)
        Right tracks -> pure tracks
    liftIO $ case segmentContaining (at 0 (TimeOfDay 14 40 0)) segments of
      Nothing -> expectationFailure "No segment holds 14:40"
      Just segment -> do
        let firstContent = fmap itemContent (take 1 (segItems segment))
            ids = [ecId e | EpisodeContent e <- firstContent]
        ids `shouldBe` [episodeId]
        stored `shouldNotBe` []
        breakTracksOf segment `shouldBe` plannedPairs stored

futureForecast :: TestDBConfig -> IO ()
futureForecast cfg =
  bracketAppM cfg $ do
    _ <- seedBreakItems
    let now = at 0 (TimeOfDay 12 0 0)
        centre = at 1 (TimeOfDay 12 30 0)
        boundary = at 1 (TimeOfDay 13 0 0)
    segments <- timeline now centre
    forecast <-
      forecastDay now (addDays 1 day) >>= \case
        Left err -> error ("Forecast failed: " <> show err)
        Right tracks -> pure tracks
    let expected = plannedPairs [t | t <- forecast, t.plBoundary == boundary]
    liftIO $ case segmentContaining centre segments of
      Nothing -> expectationFailure "No segment holds the centre"
      Just segment -> do
        expected `shouldNotBe` []
        breakTracksOf segment `shouldBe` expected

noPlanForPastDay :: TestDBConfig -> IO ()
noPlanForPastDay cfg =
  bracketAppM cfg $ do
    _ <- seedBreakItems
    -- The window starts 12 hours before 08:00, at 20:00 the day before.
    let now = at 0 (TimeOfDay 8 0 0)
    _ <- timeline now now
    -- True means this call created the day row, so the timeline did not.
    yesterdayCreated <- execQuery (BreakPlans.insertPlanDay (addDays (-1) day))
    todayCreated <- execQuery (BreakPlans.insertPlanDay day)
    liftIO $ do
      yesterdayCreated `shouldBe` Right True
      todayCreated `shouldBe` Right False

midnightBreak :: TestDBConfig -> IO ()
midnightBreak cfg =
  bracketAppM cfg $ do
    _ <- seedBreakItems
    -- The break at 00:00 starts at 23:58, so it belongs to today.
    let now = at 0 (TimeOfDay 23 55 0)
        boundary = at 1 (TimeOfDay 0 0 0)
    segments <- timeline now now
    stored <-
      execQuery (BreakPlans.tracksBetween boundary boundary) >>= \case
        Left err -> error ("Read failed: " <> show err)
        Right tracks -> pure tracks
    liftIO $ case segmentContaining now segments of
      Nothing -> expectationFailure "No segment holds 23:55"
      Just segment -> do
        segEnd segment `shouldBe` boundary
        stored `shouldNotBe` []
        breakTracksOf segment `shouldBe` plannedPairs stored

dayMatchesAround :: TestDBConfig -> IO ()
dayMatchesAround cfg =
  bracketAppM cfg $ do
    userId <- seedBreakItems
    _ <- seedEpisodeSlot userId
    let now = at 0 (TimeOfDay 12 0 0)
        instants = [at 0 (TimeOfDay hour 40 0) | hour <- [2, 14, 15, 22]]
    daySegments <- dayOf now day
    aroundSegments <- traverse (\instant -> segmentContaining instant <$> timeline now instant) instants
    liftIO $ do
      map (`segmentContaining` daySegments) instants `shouldBe` aroundSegments
      notElem Nothing aroundSegments `shouldBe` True

crossesMidnight :: TestDBConfig -> IO ()
crossesMidnight cfg =
  bracketAppM cfg $ do
    seedOvernightSlot
    let now = at 0 (TimeOfDay 12 0 0)
        start = at 0 (TimeOfDay 23 0 0)
        end = at 1 (TimeOfDay 1 0 0)
    monday <- dayOf now day
    tuesday <- dayOf now (addDays 1 day)
    let spans segments = [(segStart s, segEnd s) | s <- segments, segStart s == start]
    liftIO $ do
      spans monday `shouldBe` [(start, end)]
      spans tuesday `shouldBe` [(start, end)]

-- | With no shows, the automation breaks are the edges. The first segment
-- starts by midnight, the last ends by the next midnight, and no gap lies
-- between them.
coversWholeDay :: Day -> TestDBConfig -> IO ()
coversWholeDay today cfg = bracketAppM cfg $ do
  segments <- dayOf (pacificToUtc (LocalTime today (TimeOfDay 12 0 0))) today
  let starts = map segStart segments
      ends = map segEnd segments
  liftIO $ do
    fmap (<= startOfPacificDay today) (take 1 starts) `shouldBe` [True]
    fmap (>= startOfPacificDay (succ today)) (take 1 (reverse ends)) `shouldBe` [True]
    drop 1 starts `shouldBe` take (length ends - 1) ends

dropsOverlap :: Expectation
dropsOverlap = do
  let window start end title =
        Episodes.SlotWindow
          { Episodes.swStart = start,
            Episodes.swEnd = end,
            Episodes.swShowId = Shows.Id 1,
            Episodes.swTemplateId = ShowSchedule.TemplateId 1,
            Episodes.swShowTitle = title,
            Episodes.swShowSlug = Slug title,
            Episodes.swAirDate = day,
            Episodes.swEpisode = Nothing
          }
      early = window (at 0 (TimeOfDay 14 0 0)) (at 0 (TimeOfDay 15 0 0)) "Early"
      overlapping = window (at 0 (TimeOfDay 14 30 0)) (at 0 (TimeOfDay 15 30 0)) "Overlapping"
      touching = window (at 0 (TimeOfDay 15 0 0)) (at 0 (TimeOfDay 16 0 0)) "Touching"
      titles (slots, dropped) = (map siShowTitle slots, map Episodes.swShowTitle dropped)
  titles (slotInputs Map.empty [early, overlapping]) `shouldBe` (["Early"], ["Overlapping"])
  titles (slotInputs Map.empty [early, overlapping, touching]) `shouldBe` (["Early", "Touching"], ["Overlapping"])

--------------------------------------------------------------------------------
-- Reruns

-- | The 14:00 slot holds an episode of the week before and none for the day.
-- The timeline airs the earlier episode as a rerun, and the window still has
-- no episode.
earlierEpisodeRerun :: TestDBConfig -> IO ()
earlierEpisodeRerun cfg =
  bracketAppM cfg $ do
    userId <- seedBreakItems
    episodeId <- seedEpisodeSlotOn (addDays (-7) day) userId
    let now = at 0 (TimeOfDay 13 0 0)
        instant = at 0 (TimeOfDay 14 40 0)
    segments <- timeline now instant
    dayData <-
      dayTimeline now day >>= \case
        Left err -> error ("Timeline failed: " <> show err)
        Right timelineData -> pure timelineData
    liftIO $ do
      case segmentContaining instant segments of
        Nothing -> expectationFailure "No segment holds 14:40"
        Just segment ->
          [ecId e | RerunContent e <- map itemContent (take 1 (segItems segment))] `shouldBe` [episodeId]
      [w.swEpisode | w <- tdSlots dayData, w.swStart == at 0 (TimeOfDay 14 0 0)] `shouldBe` [Nothing]

-- | The show of the pure rerun tests.
rerunShow :: Shows.Id
rerunShow = Shows.Id 7

-- | A window of 'rerunShow' on the test day, with no episode.
emptyWindow :: TimeOfDay -> TimeOfDay -> Episodes.SlotWindow
emptyWindow start end =
  Episodes.SlotWindow
    { Episodes.swStart = at 0 start,
      Episodes.swEnd = at 0 end,
      Episodes.swShowId = rerunShow,
      Episodes.swTemplateId = ShowSchedule.TemplateId 1,
      Episodes.swShowTitle = "Rerun Show",
      Episodes.swShowSlug = Slug "rerun-show",
      Episodes.swAirDate = day,
      Episodes.swEpisode = Nothing
    }

-- | A slot episode with the given id.
slotEpisode :: Int64 -> Episodes.SlotEpisode
slotEpisode episodeId =
  Episodes.SlotEpisode
    { Episodes.seId = Episodes.Id episodeId,
      Episodes.seEpisodeNumber = Episodes.EpisodeNumber episodeId,
      Episodes.seAudioFilePath = "audio/episodes/rerun.mp3",
      Episodes.seDurationSeconds = 3500
    }

-- | The rerun candidates of one show: each episode id with the days from the
-- test day to its air date.
candidatesOf :: Shows.Id -> [(Int64, Integer)] -> Map.Map Shows.Id [Episodes.RerunCandidate]
candidatesOf showId episodes =
  Map.singleton
    showId
    [ Episodes.RerunCandidate showId (slotEpisode episodeId) (addDays daysLater day)
    | (episodeId, daysLater) <- episodes
    ]

-- | What airs in each kept window.
airings :: Map.Map Shows.Id [Episodes.RerunCandidate] -> [Episodes.SlotWindow] -> [SlotAiring]
airings candidates = map siAiring . fst . slotInputs candidates

-- | The episode id of a rerun.
rerunId :: SlotAiring -> Maybe Episodes.Id
rerunId = \case
  Rerun episode -> Just (ecId episode)
  _ -> Nothing

-- | The rows of two shows arrive mixed. The list of each show keeps the
-- order of its rows, because the rerun pick is a position in that list.
candidatesKeepRowOrder :: Expectation
candidatesKeepRowOrder = do
  let row showId episodeId = Episodes.RerunCandidate (Shows.Id showId) (slotEpisode episodeId) (addDays (-7) day)
      rows = [row 7 3, row 8 1, row 7 1, row 8 4, row 7 2]
      idsOf = map (\candidate -> candidate.rcEpisode.seId)
  Map.toList (Map.map idsOf (candidatesByShow rows))
    `shouldBe` [ (Shows.Id 7, [Episodes.Id 3, Episodes.Id 1, Episodes.Id 2]),
                 (Shows.Id 8, [Episodes.Id 1, Episodes.Id 4])
               ]

rerunFromCandidates :: Expectation
rerunFromCandidates = do
  let candidates = candidatesOf rerunShow [(1, -14), (2, -7)]
  case airings candidates [emptyWindow (TimeOfDay 14 0 0) (TimeOfDay 15 0 0)] of
    [Rerun episode] -> do
      [Episodes.Id 1, Episodes.Id 2] `shouldContain` [ecId episode]
      ecShowTitle episode `shouldBe` "Rerun Show"
      ecShowSlug episode `shouldBe` Slug "rerun-show"
    other -> expectationFailure ("Expected one rerun, got " <> show other)

-- | The check runs for 20 shows. A seed that used the window start would
-- give the two windows of some show different reruns.
replaySharesRerun :: Expectation
replaySharesRerun =
  for_ [1 .. 20] $ \n -> do
    let showId = Shows.Id n
        candidates = candidatesOf showId [(episodeId, negate (7 * toInteger episodeId)) | episodeId <- [1 .. 8]]
        primary = (emptyWindow (TimeOfDay 14 0 0) (TimeOfDay 15 0 0)) {Episodes.swShowId = showId}
        replay = (emptyWindow (TimeOfDay 22 0 0) (TimeOfDay 23 0 0)) {Episodes.swShowId = showId}
    case map rerunId (airings candidates [primary, replay]) of
      [Just first, Just second] -> first `shouldBe` second
      other -> expectationFailure ("Expected two reruns, got " <> show other)

-- | A show with two slots on one date. Each slot has its own schedule
-- template, so each gets its own pick. With 8 earlier episodes, the two picks
-- differ for some of the 20 shows.
secondSlotOwnRerun :: Expectation
secondSlotOwnRerun = do
  let picksOf n =
        let showId = Shows.Id n
            candidates = candidatesOf showId [(episodeId, negate (7 * toInteger episodeId)) | episodeId <- [1 .. 8]]
            firstSlot = (emptyWindow (TimeOfDay 10 0 0) (TimeOfDay 11 0 0)) {Episodes.swShowId = showId}
            secondSlot =
              (emptyWindow (TimeOfDay 18 0 0) (TimeOfDay 19 0 0))
                { Episodes.swShowId = showId,
                  Episodes.swTemplateId = ShowSchedule.TemplateId 2
                }
         in map rerunId (airings candidates [firstSlot, secondSlot])
      differs = \case
        [Just first, Just second] -> first /= second
        _ -> False
  any (differs . picksOf) [1 .. 20] `shouldBe` True

newEpisodeWins :: Expectation
newEpisodeWins = do
  let candidates = candidatesOf rerunShow [(1, -14), (2, -7)]
      window = (emptyWindow (TimeOfDay 14 0 0) (TimeOfDay 15 0 0)) {Episodes.swEpisode = Just (slotEpisode 9)}
  case airings candidates [window] of
    [NewEpisode episode] -> ecId episode `shouldBe` Episodes.Id 9
    other -> expectationFailure ("Expected the new episode, got " <> show other)

laterEpisodesLeftOut :: Expectation
laterEpisodesLeftOut =
  airings (candidatesOf rerunShow [(1, 0), (2, 1)]) [emptyWindow (TimeOfDay 14 0 0) (TimeOfDay 15 0 0)]
    `shouldBe` [NoEpisode]

noCandidatesNoRerun :: Expectation
noCandidatesNoRerun =
  airings Map.empty [emptyWindow (TimeOfDay 14 0 0) (TimeOfDay 15 0 0)] `shouldBe` [NoEpisode]

otherShowCandidatesUnused :: Expectation
otherShowCandidatesUnused =
  airings (candidatesOf (Shows.Id 8) [(1, -7)]) [emptyWindow (TimeOfDay 14 0 0) (TimeOfDay 15 0 0)]
    `shouldBe` [NoEpisode]
