{-# LANGUAGE QuasiQuotes #-}

-- | Tests for the getCurrentlyAiringEpisodes query.
--
-- Liquidsoap polls this to decide what to broadcast, so a wrong answer is dead
-- air or the wrong show. The query pairs each episode with one or two airing
-- windows, each a pair of @timestamptz@ values, and asks whether the given
-- instant falls inside one. These tests cover:
--
-- 1. The window built from the template's start_time and end_time
-- 2. An overnight window, where end_time <= start_time and the window wraps
-- 3. A replay window, which opens at replay_start_time and wraps the same way
-- 4. The stop at the end of the audio, from duration_seconds
-- 5. Both daylight saving transitions, where a span of clock times and a span
--    of instants have different lengths
-- 6. Schedule validity periods, effective_from and effective_until
-- 7. The recurrence test over day_of_week and weeks_of_month
-- 8. Audio file presence, episode deletion, and show status
-- 9. The order of the rows when more than one claims the same instant
module Effects.Database.Tables.CurrentlyAiringSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Data.Password.Argon2 (Argon2, PasswordHash, hashPassword, mkPassword)
import Data.Text (Text)
import Data.Int (Int64)
import Data.Time
  ( Day,
    DayOfWeek (..),
    LocalTime (..),
    TimeOfDay (..),
    UTCTime (..),
    addDays,
    dayOfWeek,
    fromGregorian,
    timeOfDayToTime,
  )
import Domain.Types.DisplayName (mkDisplayNameUnsafe)
import Domain.Types.EmailAddress (mkEmailAddress)
import Domain.Types.FullName (mkFullNameUnsafe)
import Domain.Types.Slug (mkSlug)
import Domain.Types.Timezone (pacificToUtc, utcToPacific)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Interpolate (OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.Episodes.getCurrentlyAiringEpisode" $ do
      -- Basic cases
      describe "basic cases" $ do
        it "returns Nothing when no episodes exist" basicNoEpisodes
        it "returns Nothing when episode has no audio file" basicNoAudio
        it "returns Nothing when episode is deleted" basicDeletedEpisode
        it "returns Nothing when the show is soft-deleted" basicDeletedShow
        it "returns Nothing when the show is inactive" basicInactiveShow
        it "returns Nothing when episode is scheduled for different day" basicDifferentDay
        it "recurring: airs when the episode date matches the template" recurringDateMatches
        it "recurring: returns Nothing when the weekday does not match the template" recurringWrongWeekday
        it "recurring: returns Nothing when the week of the month does not match" recurringWrongWeekOfMonth
        it "returns the episode when it is currently airing" basicCurrentlyAiring

      -- Standard show time slot tests
      describe "standard show (end > start)" $ do
        it "returns Nothing when current time is before start_time" standardBeforeStart
        it "returns the episode when current time equals start_time" standardAtStart
        it "returns the episode when current time is mid-show" standardMidShow
        it "returns Nothing when current time equals end_time" standardAtEnd
        it "returns Nothing when current time is after end_time" standardAfterEnd

      -- Overnight show tests (e.g., 11 PM - 2 AM)
      describe "overnight show (end <= start)" $ do
        it "returns Nothing when current time is before start (same day)" overnightBeforeStart
        it "returns the episode when current time is after start (before midnight)" overnightAfterStartSameDay
        it "returns the episode when current time is after midnight but before end" overnightAfterMidnight
        it "returns Nothing when current time is after end (next day)" overnightAfterEnd

      -- Replay airing tests (replay_start_time)
      describe "replay_start_time replay" $ do
        it "returns the episode during primary airing" replayPrimaryAiring
        it "returns the episode during replay airing (+12 hours)" replaySecondAiring
        it "returns the episode during non-+12h replay (custom replay time)" replayCustomTime
        it "returns Nothing between primary and replay" replayBetweenAirings
        it "returns Nothing after replay ends" replayAfterBothEnd

      -- Schedule validity tests
      describe "schedule validity periods" $ do
        it "returns Nothing when validity period hasn't started" validityNotStarted
        it "returns Nothing when validity period has ended" validityEnded
        it "returns the episode when validity is active (no end date)" validityActiveNoEnd
        it "returns the episode when validity is active (future end date)" validityActiveFutureEnd
        it "returns Nothing when effective_until equals today (exclusive)" validityEndsToday
        it "returns the episode when effective_until equals tomorrow" validityEndsTomorrow

      -- Duration-based airing tests (prevent replay bleeding)
      describe "duration-based airing" $ do
        it "returns Nothing when episode duration has ended even if slot continues" durationEndedSlotContinues
        it "returns the episode when queried mid-duration" durationMidway
        it "falls back to slot end when duration_seconds is NULL" durationNullFallback

        -- Overnight shows with duration
        it "overnight: returns episode before midnight when within duration" overnightDurationBeforeMidnightWithin
        it "overnight: returns Nothing before midnight when past duration" overnightDurationBeforeMidnightPast
        it "overnight: returns episode after midnight when duration extends past midnight" overnightDurationAfterMidnightWithin
        it "overnight: returns Nothing after midnight when duration ended before midnight" overnightDurationEndedBeforeMidnight

      -- Schedule template transitions (slot-level diffing correctness)
      describe "schedule template transitions" $ do
        it "replaced slot orphans episode" transitionReplacedSlot
        it "removed slot correctly hides episode" transitionRemovedSlot

      -- Two shows airing at different times of the same day. A show holds one
      -- slot, so the second window belongs to a second show.
      describe "consecutive timeslots" $ do
        it "returns the episode whose window covers the query" multiSlotFirstSlot
        it "returns the later episode once its window opens" multiSlotSecondSlot
        it "returns Nothing between the two windows" multiSlotBetween
        it "returns Nothing before both windows" multiSlotBeforeAll
        it "returns Nothing after both windows" multiSlotAfterAll

      -- A span of `time` values and a span of `timestamptz` values differ in
      -- length on these two dates.
      describe "daylight saving transitions" $ do
        it "fall back: airs while its audio is still running" fallBackWithinDuration
        it "fall back: stops when the audio ends, not when the clock agrees" fallBackStopsWhenAudioEnds
        it "fall back: a NULL duration fills the longer slot" fallBackNullDurationFillsSlot
        it "fall back: a slot opens the first time the clock reads its start" fallBackOpensAtFirstReading
        it "fall back: the silence lands at the end of the repeated hour" fallBackSilenceAtSlotEnd
        it "fall back: a slot closes the first time the clock reads its end" fallBackClosesAtFirstReading
        it "spring forward: airs inside the shortened slot" springForwardWithinSlot
        it "spring forward: the slot end cuts the episode short" springForwardCutAtSlotEnd
        it "spring forward: a slot inside the gap never airs" springForwardGapSlotNeverAirs

      -- Two shows claim the same time
      describe "overlapping claims" $ do
        it "returns both and picks the same one every time" overlapIsDeterministic
        it "orders a primary airing ahead of a replay" primaryBeatsReplay
        it "returns both when two different shows claim one time" twoShowsOverlap

      -- The replay window wraps by the same rule the primary does.
      describe "replay across midnight" $ do
        it "airs before midnight" replayCrossesMidnightBefore
        it "airs after midnight, on the following date" replayCrossesMidnightAfter
        it "stops at the replay end on the following date" replayCrossesMidnightEnds

      -- Behaviour the Haddock states. These pin it so a change is deliberate.
      describe "documented behaviour" $ do
        it "a duration of 0 never airs" zeroDurationNeverAirs
        it "an episode with no published_at still airs" unpublishedEpisodeStillAirs
        it "a detached episode never airs" detachedEpisodeNeverAirs
        it "equal start and end times give a 24-hour window" equalTimesGiveFullDay
        it "ignores the template timezone and uses Pacific" templateTimezoneIsIgnored
        it "an episode from two dates ago never airs" oldEpisodeNeverAirs

--------------------------------------------------------------------------------
-- Test Helpers

-- | A test date: Monday, January 6, 2025
testDay :: Day
testDay = fromGregorian 2025 1 6

-- | Create a UTC time from a Pacific time on the test day
mkTestTime :: TimeOfDay -> UTCTime
mkTestTime tod = pacificToUtc (LocalTime testDay tod)

-- | Create a UTC time from a Pacific time on the day after test day (for overnight shows)
mkTestTimeNextDay :: TimeOfDay -> UTCTime
mkTestTimeNextDay tod = pacificToUtc (LocalTime (addDays 1 testDay) tod)

-- | Helper to unwrap Maybe in IO, failing the test if Nothing
assertJustIO :: Maybe a -> IO a
assertJustIO Nothing = expectationFailure "Expected Just but got Nothing" >> error "unreachable"
assertJustIO (Just a) = pure a

-- | Setup test data: user, show, schedule template, validity, and episode.
-- Returns the episode ID for verification.
--
-- Note: Password hash must be created outside the transaction (in IO) and passed in.
setupTestData ::
  -- | Password hash (created in IO before transaction)
  PasswordHash Argon2 ->
  -- | Schedule start time
  TimeOfDay ->
  -- | Schedule end time
  TimeOfDay ->
  -- | Replay start time (Nothing = no replay)
  Maybe TimeOfDay ->
  -- | Episode scheduled_at (UTC)
  UTCTime ->
  -- | Audio file path (Nothing = no audio)
  Maybe Text ->
  -- | Validity effective_from
  Day ->
  -- | Validity effective_until
  Maybe Day ->
  TRX.Transaction (Episodes.Id, Shows.Id)
setupTestData passHash startTime endTime replayStartTime scheduledAt mAudioPath effectiveFrom effectiveUntil =
  -- Calculate slot duration in seconds and delegate to setupTestDataWithDuration
  -- For standard shows: end - start
  -- For overnight shows: (24h - start) + end
  let slotDuration =
        if endTime > startTime
          then truncate (timeOfDayToTime endTime - timeOfDayToTime startTime)
          else truncate ((24 * 3600) - timeOfDayToTime startTime + timeOfDayToTime endTime)
   in setupTestDataWithDuration passHash startTime endTime replayStartTime scheduledAt mAudioPath effectiveFrom effectiveUntil (Just slotDuration)

-- | Setup test data with custom duration, returning user ID as well.
--
-- Like setupTestData but allows specifying the episode duration explicitly
-- and returns the user ID for use in multi-timeslot tests.
--------------------------------------------------------------------------------

-- | A template covering every week, on the weekday @airDate@ falls on.
--
-- 'Episodes.getCurrentlyAiringEpisode' drops an episode whose air date its template
-- does not hold. Deriving the weekday from the date, and taking every week, means the
-- fixture always satisfies that test, so each case below exercises the window
-- arithmetic rather than the recurrence.
recurringOn ::
  -- | The Pacific date the episode airs on.
  Day ->
  Shows.Id ->
  TimeOfDay ->
  TimeOfDay ->
  Maybe TimeOfDay ->
  ShowSchedule.ScheduleTemplateInsert
recurringOn airDate showId startTime endTime replayStartTime =
  ShowSchedule.ScheduleTemplateInsert
    { stiShowId = showId,
      stiDayOfWeek = dayOfWeek airDate,
      stiWeeksOfMonth = [1, 2, 3, 4, 5],
      stiStartTime = startTime,
      stiEndTime = endTime,
      stiTimezone = "America/Los_Angeles",
      stiReplayStartTime = replayStartTime
    }

-- | The Pacific date an instant falls on.
pacificDayOf :: UTCTime -> Day
pacificDayOf = localDay . utcToPacific

setupTestDataFull ::
  -- | Password hash (created in IO before transaction)
  PasswordHash Argon2 ->
  -- | Schedule start time
  TimeOfDay ->
  -- | Schedule end time
  TimeOfDay ->
  -- | Replay start time (Nothing = no replay)
  Maybe TimeOfDay ->
  -- | Episode scheduled_at (UTC)
  UTCTime ->
  -- | Audio file path (Nothing = no audio)
  Maybe Text ->
  -- | Validity effective_from
  Day ->
  -- | Validity effective_until
  Maybe Day ->
  -- | Episode duration in seconds (Nothing = NULL)
  Maybe Int ->
  TRX.Transaction (Episodes.Id, Shows.Id, User.Id)
setupTestDataFull passHash startTime endTime replayStartTime scheduledAt mAudioPath effectiveFrom effectiveUntil mDuration = do
  -- Create user
  (OneRow userId) <-
    TRX.statement () $
      User.insertUser $
        User.ModelInsert (mkEmailAddress "test@example.com") passHash

  _ <-
    TRX.statement () $
      UserMetadata.insertUserMetadata $
        UserMetadata.Insert
          userId
          (mkDisplayNameUnsafe "Test User")
          (mkFullNameUnsafe "Test User")
          Nothing
          UserMetadata.Staff
          UserMetadata.Automatic
          UserMetadata.DefaultTheme

  -- Create show
  showId <-
    unwrapInsert $
      Shows.insertShow
        Shows.Insert
          { siTitle = "Test Show",
            siSlug = mkSlug "test-show",
            siDescription = Nothing,
            siLogoUrl = Nothing,
            siStatus = Shows.Active
          }

  -- Create schedule template
  templateId <-
    TRX.statement () $
      ShowSchedule.insertScheduleTemplate
        (recurringOn (pacificDayOf scheduledAt) showId (startTime) (endTime) (replayStartTime))

  -- Create validity period
  _ <-
    unwrapInsert $
      ShowSchedule.insertValidity
        ShowSchedule.ValidityInsert
          { viTemplateId = templateId,
            viEffectiveFrom = effectiveFrom,
            viEffectiveUntil = effectiveUntil
          }

  -- Create episode with custom duration
  episodeId <-
    unwrapInsert $
      Episodes.insertEpisode
        Episodes.Insert
          { eiId = showId,
            eiDescription = Just "Test Episode",
            eiAudioFilePath = mAudioPath,
            eiAudioFileSize = if isJust mAudioPath then Just 1000000 else Nothing,
            eiAudioMimeType = if isJust mAudioPath then Just "audio/mpeg" else Nothing,
            eiDurationSeconds = fromIntegral <$> mDuration,
            eiArtworkUrl = Nothing,
            eiScheduleTemplateId = Just templateId,
            eiScheduledAt = Just scheduledAt,
            eiCreatedBy = userId
          }

  pure (episodeId, showId, userId)

-- | Setup a show on a recurring template, with an episode on a chosen date.
--
-- The airing query takes the time of day from the template and the date from the
-- episode. These tests check that it also requires the two to agree, which the
-- one-time templates the other fixtures use are exempt from.
setupRecurringTestData ::
  PasswordHash Argon2 ->
  -- | The day of the week the template airs on
  DayOfWeek ->
  -- | The weeks of the month the template airs in
  [Int64] ->
  -- | The date the episode claims to air on
  Day ->
  TRX.Transaction Episodes.Id
setupRecurringTestData passHash dayOfWeek weeksOfMonth episodeDate = do
  (OneRow userId) <-
    TRX.statement () $
      User.insertUser $
        User.ModelInsert (mkEmailAddress "recurring@example.com") passHash
  _ <-
    TRX.statement () $
      UserMetadata.insertUserMetadata $
        UserMetadata.Insert
          userId
          (mkDisplayNameUnsafe "Recurring Host")
          (mkFullNameUnsafe "Recurring Host")
          Nothing
          UserMetadata.Staff
          UserMetadata.Automatic
          UserMetadata.DefaultTheme

  showId <-
    unwrapInsert $
      Shows.insertShow
        Shows.Insert
          { siTitle = "Recurring Show",
            siSlug = mkSlug "recurring-show",
            siDescription = Nothing,
            siLogoUrl = Nothing,
            siStatus = Shows.Active
          }

  templateId <-
    TRX.statement () $
      ShowSchedule.insertScheduleTemplate
        ShowSchedule.ScheduleTemplateInsert
          { stiShowId = showId,
            stiDayOfWeek = dayOfWeek,
            stiWeeksOfMonth = weeksOfMonth,
            stiStartTime = TimeOfDay 14 0 0,
            stiEndTime = TimeOfDay 16 0 0,
            stiTimezone = "America/Los_Angeles",
            stiReplayStartTime = Nothing
          }

  _ <-
    unwrapInsert $
      ShowSchedule.insertValidity
        ShowSchedule.ValidityInsert
          { viTemplateId = templateId,
            viEffectiveFrom = addDays (-30) episodeDate,
            viEffectiveUntil = Nothing
          }

  unwrapInsert $
    Episodes.insertEpisode
      Episodes.Insert
        { eiId = showId,
          eiDescription = Just "Recurring Episode",
          eiAudioFilePath = Just "audio/recurring.mp3",
          eiAudioFileSize = Just 1000000,
          eiAudioMimeType = Just "audio/mpeg",
          eiDurationSeconds = Just 7200,
          eiArtworkUrl = Nothing,
          eiScheduleTemplateId = Just templateId,
          eiScheduledAt = Just (pacificToUtc (LocalTime episodeDate (TimeOfDay 14 0 0))),
          eiCreatedBy = userId
        }

-- | testDay is a Monday in week 1 of the month.
recurringDateMatches :: TestDBConfig -> IO ()
recurringDateMatches cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let queryTime = pacificToUtc (LocalTime testDay (TimeOfDay 15 0 0))
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    episodeId <- setupRecurringTestData passHash Monday [1, 2, 3, 4, 5] testDay
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure (episodeId, mEpisode)
  case result of
    Left err -> error $ "DB error: " <> show err
    Right (episodeId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      episode.id `shouldBe` episodeId

-- | A Wednesday episode on a Monday template must not air, even at the
-- template's own hours on that Wednesday.
recurringWrongWeekday :: TestDBConfig -> IO ()
recurringWrongWeekday cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let wednesday = addDays 2 testDay
      queryTime = pacificToUtc (LocalTime wednesday (TimeOfDay 15 0 0))
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupRecurringTestData passHash Monday [1, 2, 3, 4, 5] wednesday
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure mEpisode
  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

-- | The weekday alone is not enough. A third-Monday template must not air an
-- episode on a first Monday.
recurringWrongWeekOfMonth :: TestDBConfig -> IO ()
recurringWrongWeekOfMonth cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let queryTime = pacificToUtc (LocalTime testDay (TimeOfDay 15 0 0))
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupRecurringTestData passHash Monday [3] testDay
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure mEpisode
  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

-- | Setup test data with custom duration (discards user ID).
setupTestDataWithDuration ::
  PasswordHash Argon2 ->
  TimeOfDay ->
  TimeOfDay ->
  Maybe TimeOfDay ->
  UTCTime ->
  Maybe Text ->
  Day ->
  Maybe Day ->
  Maybe Int ->
  TRX.Transaction (Episodes.Id, Shows.Id)
setupTestDataWithDuration passHash startTime endTime replayStartTime scheduledAt mAudioPath effectiveFrom effectiveUntil mDuration = do
  (episodeId, showId, _userId) <- setupTestDataFull passHash startTime endTime replayStartTime scheduledAt mAudioPath effectiveFrom effectiveUntil mDuration
  pure (episodeId, showId)

-- | Add a second concurrent airing, as its own show.
--
-- @one_active_slot_per_show@ allows a show one slot at a time, so a window overlapping
-- another belongs to a second show. Which episode
-- 'Episodes.getCurrentlyAiringEpisode' picks does not depend on whose slot each window
-- is, so these cases read the same either way.
addTimeslot ::
  -- | Slug suffix, so each show in a test is distinct
  Text ->
  -- | Existing user (for episode creator)
  User.Id ->
  -- | Start time
  TimeOfDay ->
  -- | End time
  TimeOfDay ->
  -- | Replay start time (Nothing = no replay)
  Maybe TimeOfDay ->
  -- | Episode scheduled_at (UTC)
  UTCTime ->
  -- | Audio file path
  Maybe Text ->
  -- | Validity effective_from
  Day ->
  -- | Validity effective_until
  Maybe Day ->
  TRX.Transaction Episodes.Id
addTimeslot slugSuffix userId startTime endTime replayStartTime scheduledAt mAudioPath effectiveFrom effectiveUntil = do
  let slotDuration :: Integer
      slotDuration =
        if endTime > startTime
          then truncate (timeOfDayToTime endTime - timeOfDayToTime startTime)
          else truncate ((24 * 3600) - timeOfDayToTime startTime + timeOfDayToTime endTime)

  showId <-
    unwrapInsert $
      Shows.insertShow
        Shows.Insert
          { siTitle = "Test Show " <> slugSuffix,
            siSlug = mkSlug ("test-show-" <> slugSuffix),
            siDescription = Nothing,
            siLogoUrl = Nothing,
            siStatus = Shows.Active
          }

  templateId <-
    TRX.statement () $
      ShowSchedule.insertScheduleTemplate
        (recurringOn (pacificDayOf scheduledAt) showId (startTime) (endTime) (replayStartTime))

  _ <-
    unwrapInsert $
      ShowSchedule.insertValidity
        ShowSchedule.ValidityInsert
          { viTemplateId = templateId,
            viEffectiveFrom = effectiveFrom,
            viEffectiveUntil = effectiveUntil
          }

  unwrapInsert $
    Episodes.insertEpisode
      Episodes.Insert
        { eiId = showId,
          eiDescription = Just "Test Episode (second show)",
          eiAudioFilePath = mAudioPath,
          eiAudioFileSize = if isJust mAudioPath then Just 1000000 else Nothing,
          eiAudioMimeType = if isJust mAudioPath then Just "audio/mpeg" else Nothing,
          eiDurationSeconds = Just (fromIntegral slotDuration),
          eiArtworkUrl = Nothing,
          eiScheduleTemplateId = Just templateId,
          eiScheduledAt = Just scheduledAt,
          eiCreatedBy = userId
        }

--------------------------------------------------------------------------------
-- Basic Cases

basicNoEpisodes :: TestDBConfig -> IO ()
basicNoEpisodes cfg = bracketConn cfg $ do
  let queryTime = mkTestTime (TimeOfDay 14 0 0) -- 2 PM Pacific
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ do
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

basicNoAudio :: TestDBConfig -> IO ()
basicNoAudio cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0 -- 2 PM
      endTime = TimeOfDay 16 0 0 -- 4 PM
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 0 0) -- 3 PM (mid-show)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash startTime endTime Nothing scheduledAt Nothing testDay Nothing
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

basicDeletedEpisode :: TestDBConfig -> IO ()
basicDeletedEpisode cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 0 0)

  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    -- Soft delete the episode
    _ <- TRX.statement () $ Episodes.deleteEpisode episodeId
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

basicDeletedShow :: TestDBConfig -> IO ()
basicDeletedShow cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 0 0)

  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (_, showId) <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    -- Soft delete the show
    _ <- TRX.statement () $ Shows.softDeleteShow showId
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

basicInactiveShow :: TestDBConfig -> IO ()
basicInactiveShow cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 0 0)
      slotDuration = 7200 -- 2 hours in seconds
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    -- Inline setup to insert the show as Inactive (setupTestData hardcodes Active)
    (OneRow userId) <-
      TRX.statement () $
        User.insertUser $
          User.ModelInsert (mkEmailAddress "test@example.com") passHash
    _ <-
      TRX.statement () $
        UserMetadata.insertUserMetadata $
          UserMetadata.Insert userId (mkDisplayNameUnsafe "Test User") (mkFullNameUnsafe "Test User") Nothing UserMetadata.Staff UserMetadata.Automatic UserMetadata.DefaultTheme

    showId <-
      unwrapInsert $
        Shows.insertShow
          Shows.Insert {siTitle = "Test Show", siSlug = mkSlug "test-show", siDescription = Nothing, siLogoUrl = Nothing, siStatus = Shows.Inactive}

    templateId <-
      TRX.statement () $
        ShowSchedule.insertScheduleTemplate
          (recurringOn (pacificDayOf scheduledAt) showId (startTime) (endTime) (Nothing))

    _ <-
      unwrapInsert $
        ShowSchedule.insertValidity
          ShowSchedule.ValidityInsert {viTemplateId = templateId, viEffectiveFrom = testDay, viEffectiveUntil = Nothing}

    _ <-
      unwrapInsert $
        Episodes.insertEpisode
          Episodes.Insert
            { eiId = showId,
              eiDescription = Just "Test Episode",
              eiAudioFilePath = Just "audio/test.mp3",
              eiAudioFileSize = Just 1000000,
              eiAudioMimeType = Just "audio/mpeg",
              eiDurationSeconds = Just slotDuration,
              eiArtworkUrl = Nothing,
              eiScheduleTemplateId = Just templateId,
              eiScheduledAt = Just scheduledAt,
              eiCreatedBy = userId
            }

    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

basicDifferentDay :: TestDBConfig -> IO ()
basicDifferentDay cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      -- Episode scheduled for tomorrow, not today
      scheduledAt = mkTestTimeNextDay startTime
      -- Query time is today
      queryTime = mkTestTime (TimeOfDay 15 0 0)

  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

basicCurrentlyAiring :: TestDBConfig -> IO ()
basicCurrentlyAiring cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 0 0)

  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

--------------------------------------------------------------------------------
-- Standard Show Tests (end > start)

standardBeforeStart :: TestDBConfig -> IO ()
standardBeforeStart cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0 -- 2 PM
      endTime = TimeOfDay 16 0 0 -- 4 PM
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 13 59 59) -- 1:59:59 PM (1 second before)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

standardAtStart :: TestDBConfig -> IO ()
standardAtStart cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime startTime -- Exactly at start
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

standardMidShow :: TestDBConfig -> IO ()
standardMidShow cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 0 0) -- 3 PM (middle)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

standardAtEnd :: TestDBConfig -> IO ()
standardAtEnd cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime endTime -- Exactly at end (exclusive)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

standardAfterEnd :: TestDBConfig -> IO ()
standardAfterEnd cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 16 0 1) -- 1 second after end
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Overnight Show Tests (end <= start, e.g., 11 PM - 2 AM)

overnightBeforeStart :: TestDBConfig -> IO ()
overnightBeforeStart cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 23 0 0 -- 11 PM
      endTime = TimeOfDay 2 0 0 -- 2 AM (next day)
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 22 59 59) -- 10:59:59 PM (before start)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

overnightAfterStartSameDay :: TestDBConfig -> IO ()
overnightAfterStartSameDay cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 23 0 0 -- 11 PM
      endTime = TimeOfDay 2 0 0 -- 2 AM
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 23 30 0) -- 11:30 PM (same day, during show)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

overnightAfterMidnight :: TestDBConfig -> IO ()
overnightAfterMidnight cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 23 0 0 -- 11 PM
      endTime = TimeOfDay 2 0 0 -- 2 AM
      scheduledAt = mkTestTime startTime
      -- Query at 1 AM next day - still during the show that started yesterday
      queryTime = mkTestTimeNextDay (TimeOfDay 1 0 0)

  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

overnightAfterEnd :: TestDBConfig -> IO ()
overnightAfterEnd cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 23 0 0 -- 11 PM
      endTime = TimeOfDay 2 0 0 -- 2 AM
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTimeNextDay (TimeOfDay 2 0 1) -- 2:00:01 AM (after end)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Replay Airing Tests (replay_start_time)

replayPrimaryAiring :: TestDBConfig -> IO ()
replayPrimaryAiring cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 6 0 0 -- 6 AM
      endTime = TimeOfDay 8 0 0 -- 8 AM
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 7 0 0) -- 7 AM (during primary)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash startTime endTime (Just (TimeOfDay 18 0 0)) scheduledAt (Just "audio/test.mp3") testDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

replaySecondAiring :: TestDBConfig -> IO ()
replaySecondAiring cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 6 0 0 -- 6 AM
      endTime = TimeOfDay 8 0 0 -- 8 AM
      -- Replay is at 6 PM - 8 PM (+12 hours)
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 19 0 0) -- 7 PM (during replay)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash startTime endTime (Just (TimeOfDay 18 0 0)) scheduledAt (Just "audio/test.mp3") testDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

-- | Test a non-+12h replay: 10 AM - 11 AM primary, replay at 9 PM.
-- Replay runs 9 PM - 10 PM (1 hour duration matches primary).
-- +12h would put replay at 10 PM, so 9 PM proves configurable replay works.
replayCustomTime :: TestDBConfig -> IO ()
replayCustomTime cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 10 0 0 -- 10 AM
      endTime = TimeOfDay 11 0 0 -- 11 AM (1 hour show)
      replayStart = TimeOfDay 21 0 0 -- 9 PM (not +12h which would be 10 PM)
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 21 30 0) -- 9:30 PM (during replay 9 PM - 10 PM)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash startTime endTime (Just replayStart) scheduledAt (Just "audio/test.mp3") testDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

replayBetweenAirings :: TestDBConfig -> IO ()
replayBetweenAirings cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 6 0 0 -- 6 AM
      endTime = TimeOfDay 8 0 0 -- 8 AM
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 12 0 0) -- Noon (between 8 AM and 6 PM)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash startTime endTime (Just (TimeOfDay 18 0 0)) scheduledAt (Just "audio/test.mp3") testDay Nothing
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

replayAfterBothEnd :: TestDBConfig -> IO ()
replayAfterBothEnd cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 6 0 0 -- 6 AM
      endTime = TimeOfDay 8 0 0 -- 8 AM
      -- Replay ends at 8 PM
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 20 0 1) -- 8:00:01 PM (after replay)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash startTime endTime (Just (TimeOfDay 18 0 0)) scheduledAt (Just "audio/test.mp3") testDay Nothing
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Schedule Validity Tests

validityNotStarted :: TestDBConfig -> IO ()
validityNotStarted cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 0 0)
      -- Validity starts tomorrow
      effectiveFrom = addDays 1 testDay

  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") effectiveFrom Nothing
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

validityEnded :: TestDBConfig -> IO ()
validityEnded cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 0 0)
      -- Validity ended yesterday
      effectiveFrom = addDays (-30) testDay
      effectiveUntil = Just $ addDays (-1) testDay

  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") effectiveFrom effectiveUntil
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

validityActiveNoEnd :: TestDBConfig -> IO ()
validityActiveNoEnd cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 0 0)
      -- Validity started in the past, no end date
      effectiveFrom = addDays (-30) testDay

  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") effectiveFrom Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

validityActiveFutureEnd :: TestDBConfig -> IO ()
validityActiveFutureEnd cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 0 0)
      -- Validity active with future end date
      effectiveFrom = addDays (-30) testDay
      effectiveUntil = Just $ addDays 30 testDay

  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") effectiveFrom effectiveUntil
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

-- | effective_until is exclusive: if effective_until = today, today is invalid
validityEndsToday :: TestDBConfig -> IO ()
validityEndsToday cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 0 0)
      -- Validity ends today (exclusive), so today is the first INVALID day
      effectiveFrom = addDays (-30) testDay
      effectiveUntil = Just testDay

  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") effectiveFrom effectiveUntil
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

-- | effective_until is exclusive: if effective_until = tomorrow, today is still valid
validityEndsTomorrow :: TestDBConfig -> IO ()
validityEndsTomorrow cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0
      endTime = TimeOfDay 16 0 0
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 0 0)
      -- Validity ends tomorrow (exclusive), so today is the last VALID day
      effectiveFrom = addDays (-30) testDay
      effectiveUntil = Just $ addDays 1 testDay

  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") effectiveFrom effectiveUntil
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

--------------------------------------------------------------------------------
-- Duration-Based Airing Tests

-- | When episode duration has ended but the time slot continues,
-- the query should return Nothing to prevent replay bleeding.
--
-- Scenario: 2 PM - 4 PM slot, 30-minute episode, query at 2:35 PM
-- Expected: Nothing (duration ended at 2:30 PM even though slot runs until 4 PM)
durationEndedSlotContinues :: TestDBConfig -> IO ()
durationEndedSlotContinues cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0 -- 2 PM
      endTime = TimeOfDay 16 0 0 -- 4 PM (2 hour slot)
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 14 35 0) -- 2:35 PM (past 30-min duration)
      duration = Just 1800 -- 30 minutes in seconds
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestDataWithDuration passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing duration
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

-- | When queried mid-duration, the episode should still be returned.
--
-- Scenario: 2 PM - 4 PM slot, 30-minute episode, query at 2:15 PM
-- Expected: Returns the episode (15 minutes into 30-minute duration)
durationMidway :: TestDBConfig -> IO ()
durationMidway cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0 -- 2 PM
      endTime = TimeOfDay 16 0 0 -- 4 PM
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 14 15 0) -- 2:15 PM (within 30-min duration)
      duration = Just 1800 -- 30 minutes
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestDataWithDuration passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing duration
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

-- | When duration_seconds is NULL, fall back to slot end time (backward compatibility).
--
-- Scenario: 2 PM - 4 PM slot, NULL duration, query at 3:30 PM
-- Expected: Returns the episode (still within slot bounds)
durationNullFallback :: TestDBConfig -> IO ()
durationNullFallback cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0 -- 2 PM
      endTime = TimeOfDay 16 0 0 -- 4 PM
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 30 0) -- 3:30 PM
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestDataWithDuration passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

--------------------------------------------------------------------------------
-- Overnight Duration Tests

-- | Overnight show, query before midnight, within duration.
--
-- Scenario: 11 PM - 2 AM slot, 30-minute duration, query at 11:15 PM
-- Expected: Returns episode (15 minutes into 30-minute duration)
overnightDurationBeforeMidnightWithin :: TestDBConfig -> IO ()
overnightDurationBeforeMidnightWithin cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 23 0 0 -- 11 PM
      endTime = TimeOfDay 2 0 0 -- 2 AM (next day)
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 23 15 0) -- 11:15 PM (within 30-min duration)
      duration = Just 1800 -- 30 minutes
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestDataWithDuration passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing duration
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

-- | Overnight show, query before midnight, past duration.
--
-- Scenario: 11 PM - 2 AM slot, 30-minute duration, query at 11:45 PM
-- Expected: Nothing (past 30-minute duration that ended at 11:30 PM)
overnightDurationBeforeMidnightPast :: TestDBConfig -> IO ()
overnightDurationBeforeMidnightPast cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 23 0 0 -- 11 PM
      endTime = TimeOfDay 2 0 0 -- 2 AM
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 23 45 0) -- 11:45 PM (past 30-min duration)
      duration = Just 1800 -- 30 minutes
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestDataWithDuration passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing duration
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

-- | Overnight show, query after midnight, duration extends past midnight.
--
-- Scenario: 11 PM - 2 AM slot, 2-hour duration, query at 12:30 AM next day
-- Expected: Returns episode (1.5 hours into 2-hour duration)
overnightDurationAfterMidnightWithin :: TestDBConfig -> IO ()
overnightDurationAfterMidnightWithin cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 23 0 0 -- 11 PM
      endTime = TimeOfDay 2 0 0 -- 2 AM
      scheduledAt = mkTestTime startTime
      -- Query at 12:30 AM next day (1.5 hours into show, within 2-hour duration)
      queryTime = mkTestTimeNextDay (TimeOfDay 0 30 0)
      duration = Just 7200 -- 2 hours
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestDataWithDuration passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing duration
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId

-- | Overnight show, query after midnight, but duration ended before midnight.
--
-- Scenario: 11 PM - 2 AM slot, 30-minute duration, query at 1 AM next day
-- Expected: Nothing (30-minute duration ended at 11:30 PM, before midnight)
overnightDurationEndedBeforeMidnight :: TestDBConfig -> IO ()
overnightDurationEndedBeforeMidnight cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 23 0 0 -- 11 PM
      endTime = TimeOfDay 2 0 0 -- 2 AM
      scheduledAt = mkTestTime startTime
      -- Query at 1 AM next day - but duration ended at 11:30 PM
      queryTime = mkTestTimeNextDay (TimeOfDay 1 0 0)
      duration = Just 1800 -- 30 minutes (ends at 11:30 PM)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestDataWithDuration passHash startTime endTime Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing duration
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Schedule Template Transition Tests
--
-- These tests document the behavior difference between slot-level diffing
-- (preserving unchanged templates) and nuke-and-rebuild (terminating all templates).

-- | Replaced slot orphans episode.
--
-- When nuke-and-rebuild terminates an existing template and creates a new one
-- with identical times, episodes linked to the old template become orphaned
-- because the old template's validity has ended.
transitionReplacedSlot :: TestDBConfig -> IO ()
transitionReplacedSlot cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0 -- 2 PM
      endTime = TimeOfDay 16 0 0 -- 4 PM
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 0 0) -- 3 PM (mid-show)
      effectiveFrom = addDays (-30) testDay
      slotDuration = 7200 -- 2 hours in seconds
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    -- Inline setup to capture the validity ID
    (OneRow userId) <-
      TRX.statement () $
        User.insertUser $
          User.ModelInsert (mkEmailAddress "test@example.com") passHash
    _ <-
      TRX.statement () $
        UserMetadata.insertUserMetadata $
          UserMetadata.Insert userId (mkDisplayNameUnsafe "Test User") (mkFullNameUnsafe "Test User") Nothing UserMetadata.Staff UserMetadata.Automatic UserMetadata.DefaultTheme

    showId <-
      unwrapInsert $
        Shows.insertShow
          Shows.Insert {siTitle = "Test Show", siSlug = mkSlug "test-show", siDescription = Nothing, siLogoUrl = Nothing, siStatus = Shows.Active}

    templateId1 <-
      TRX.statement () $
        ShowSchedule.insertScheduleTemplate
          (recurringOn (pacificDayOf scheduledAt) showId (startTime) (endTime) (Nothing))

    validityId1 <-
      unwrapInsert $
        ShowSchedule.insertValidity
          ShowSchedule.ValidityInsert {viTemplateId = templateId1, viEffectiveFrom = effectiveFrom, viEffectiveUntil = Nothing}

    _ <-
      unwrapInsert $
        Episodes.insertEpisode
          Episodes.Insert
            { eiId = showId,
              eiDescription = Just "Test Episode",
              eiAudioFilePath = Just "audio/test.mp3",
              eiAudioFileSize = Just 1000000,
              eiAudioMimeType = Just "audio/mpeg",
              eiDurationSeconds = Just slotDuration,
              eiArtworkUrl = Nothing,
              eiScheduleTemplateId = Just templateId1,
              eiScheduledAt = Just scheduledAt,
              eiCreatedBy = userId
            }

    -- End T1's validity — simulating nuke-and-rebuild
    _ <- TRX.statement () $ ShowSchedule.endValidity validityId1 testDay

    -- Create T2 with identical times + new validity — simulating recreation
    templateId2 <-
      TRX.statement () $
        ShowSchedule.insertScheduleTemplate
          (recurringOn (pacificDayOf scheduledAt) showId (startTime) (endTime) (Nothing))

    _ <-
      unwrapInsert $
        ShowSchedule.insertValidity
          ShowSchedule.ValidityInsert {viTemplateId = templateId2, viEffectiveFrom = testDay, viEffectiveUntil = Nothing}

    -- Episode is linked to T1, but T1's validity ended → orphaned
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

-- | Removed slot correctly hides episode.
--
-- When a slot is genuinely removed, its template's validity is ended.
-- Episodes linked to that template should no longer be visible.
transitionRemovedSlot :: TestDBConfig -> IO ()
transitionRemovedSlot cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 14 0 0 -- 2 PM
      endTime = TimeOfDay 16 0 0 -- 4 PM
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 15 0 0) -- 3 PM (mid-show)
      effectiveFrom = addDays (-30) testDay
      slotDuration = 7200 -- 2 hours in seconds
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    -- Inline setup to capture the validity ID
    (OneRow userId) <-
      TRX.statement () $
        User.insertUser $
          User.ModelInsert (mkEmailAddress "test@example.com") passHash
    _ <-
      TRX.statement () $
        UserMetadata.insertUserMetadata $
          UserMetadata.Insert userId (mkDisplayNameUnsafe "Test User") (mkFullNameUnsafe "Test User") Nothing UserMetadata.Staff UserMetadata.Automatic UserMetadata.DefaultTheme

    showId <-
      unwrapInsert $
        Shows.insertShow
          Shows.Insert {siTitle = "Test Show", siSlug = mkSlug "test-show", siDescription = Nothing, siLogoUrl = Nothing, siStatus = Shows.Active}

    templateId1 <-
      TRX.statement () $
        ShowSchedule.insertScheduleTemplate
          (recurringOn (pacificDayOf scheduledAt) showId (startTime) (endTime) (Nothing))

    validityId1 <-
      unwrapInsert $
        ShowSchedule.insertValidity
          ShowSchedule.ValidityInsert {viTemplateId = templateId1, viEffectiveFrom = effectiveFrom, viEffectiveUntil = Nothing}

    _ <-
      unwrapInsert $
        Episodes.insertEpisode
          Episodes.Insert
            { eiId = showId,
              eiDescription = Just "Test Episode",
              eiAudioFilePath = Just "audio/test.mp3",
              eiAudioFileSize = Just 1000000,
              eiAudioMimeType = Just "audio/mpeg",
              eiDurationSeconds = Just slotDuration,
              eiArtworkUrl = Nothing,
              eiScheduleTemplateId = Just templateId1,
              eiScheduledAt = Just scheduledAt,
              eiCreatedBy = userId
            }

    -- End T1's validity — simulating genuine slot removal
    _ <- TRX.statement () $ ShowSchedule.endValidity validityId1 testDay

    -- No replacement template created — the slot was truly removed
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Multiple Timeslot Tests
--
-- These tests verify that when a show has multiple timeslots (e.g., 9-11 AM
-- and 2-4 PM), the correct episode is returned for each time window.

-- | Returns correct episode for first timeslot.
--
-- Show has slots 9-11 AM and 2-4 PM. Query at 10 AM should return
-- the episode from the first slot.
multiSlotFirstSlot :: TestDBConfig -> IO ()
multiSlotFirstSlot cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let slot1Start = TimeOfDay 9 0 0 -- 9 AM
      slot1End = TimeOfDay 11 0 0 -- 11 AM
      slot2Start = TimeOfDay 14 0 0 -- 2 PM
      slot2End = TimeOfDay 16 0 0 -- 4 PM
      scheduledAt1 = mkTestTime slot1Start
      scheduledAt2 = mkTestTime slot2Start
      queryTime = mkTestTime (TimeOfDay 10 0 0) -- 10 AM (during first slot)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (ep1, showId, userId) <- setupTestDataFull passHash slot1Start slot1End Nothing scheduledAt1 (Just "audio/slot1.mp3") testDay Nothing Nothing
    _ep2 <- addTimeslot "slot2" userId slot2Start slot2End Nothing scheduledAt2 (Just "audio/slot2.mp3") testDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (ep1, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId
      Episodes.audioFilePath episode `shouldBe` Just "audio/slot1.mp3"

-- | Returns correct episode for second timeslot.
--
-- Show has slots 9-11 AM and 2-4 PM. Query at 3 PM should return
-- the episode from the second slot.
multiSlotSecondSlot :: TestDBConfig -> IO ()
multiSlotSecondSlot cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let slot1Start = TimeOfDay 9 0 0
      slot1End = TimeOfDay 11 0 0
      slot2Start = TimeOfDay 14 0 0
      slot2End = TimeOfDay 16 0 0
      scheduledAt1 = mkTestTime slot1Start
      scheduledAt2 = mkTestTime slot2Start
      queryTime = mkTestTime (TimeOfDay 15 0 0) -- 3 PM (during second slot)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (_ep1, showId, userId) <- setupTestDataFull passHash slot1Start slot1End Nothing scheduledAt1 (Just "audio/slot1.mp3") testDay Nothing Nothing
    ep2 <- addTimeslot "slot2" userId slot2Start slot2End Nothing scheduledAt2 (Just "audio/slot2.mp3") testDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (ep2, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId
      Episodes.audioFilePath episode `shouldBe` Just "audio/slot2.mp3"

-- | Returns Nothing between timeslots.
--
-- Show has slots 9-11 AM and 2-4 PM. Query at 12 PM (between slots)
-- should return Nothing.
multiSlotBetween :: TestDBConfig -> IO ()
multiSlotBetween cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let slot1Start = TimeOfDay 9 0 0
      slot1End = TimeOfDay 11 0 0
      slot2Start = TimeOfDay 14 0 0
      slot2End = TimeOfDay 16 0 0
      scheduledAt1 = mkTestTime slot1Start
      scheduledAt2 = mkTestTime slot2Start
      queryTime = mkTestTime (TimeOfDay 12 0 0) -- Noon (between slots)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (_ep1, showId, userId) <- setupTestDataFull passHash slot1Start slot1End Nothing scheduledAt1 (Just "audio/slot1.mp3") testDay Nothing Nothing
    _ep2 <- addTimeslot "slot2" userId slot2Start slot2End Nothing scheduledAt2 (Just "audio/slot2.mp3") testDay Nothing
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

-- | Returns Nothing before all timeslots.
--
-- Show has slots 9-11 AM and 2-4 PM. Query at 8 AM (before both)
-- should return Nothing.
multiSlotBeforeAll :: TestDBConfig -> IO ()
multiSlotBeforeAll cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let slot1Start = TimeOfDay 9 0 0
      slot1End = TimeOfDay 11 0 0
      slot2Start = TimeOfDay 14 0 0
      slot2End = TimeOfDay 16 0 0
      scheduledAt1 = mkTestTime slot1Start
      scheduledAt2 = mkTestTime slot2Start
      queryTime = mkTestTime (TimeOfDay 8 0 0) -- 8 AM (before both)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (_ep1, showId, userId) <- setupTestDataFull passHash slot1Start slot1End Nothing scheduledAt1 (Just "audio/slot1.mp3") testDay Nothing Nothing
    _ep2 <- addTimeslot "slot2" userId slot2Start slot2End Nothing scheduledAt2 (Just "audio/slot2.mp3") testDay Nothing
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

-- | Returns Nothing after all timeslots.
--
-- Show has slots 9-11 AM and 2-4 PM. Query at 5 PM (after both)
-- should return Nothing.
multiSlotAfterAll :: TestDBConfig -> IO ()
multiSlotAfterAll cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let slot1Start = TimeOfDay 9 0 0
      slot1End = TimeOfDay 11 0 0
      slot2Start = TimeOfDay 14 0 0
      slot2End = TimeOfDay 16 0 0
      scheduledAt1 = mkTestTime slot1Start
      scheduledAt2 = mkTestTime slot2Start
      queryTime = mkTestTime (TimeOfDay 17 0 0) -- 5 PM (after both)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (_ep1, showId, userId) <- setupTestDataFull passHash slot1Start slot1End Nothing scheduledAt1 (Just "audio/slot1.mp3") testDay Nothing Nothing
    _ep2 <- addTimeslot "slot2" userId slot2Start slot2End Nothing scheduledAt2 (Just "audio/slot2.mp3") testDay Nothing
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Daylight saving transitions
--
-- These are the two dates each year on which subtracting two `time` values gives
-- a different answer from subtracting the matching `timestamptz` values. The
-- query compares `timestamptz` values, so it gets both dates right. Comparing
-- `time` values gets both wrong, in opposite directions.
--
-- The query times below are given as UTC, not as Pacific `time` values. On the
-- fall-back date the Pacific clock reads 01:30 twice, so a `time` cannot say
-- which of the two is meant.

-- | 2025-11-02. The clock repeats 01:00 to 02:00 Pacific.
--
-- A 00:00 to 02:00 slot therefore covers 3 elapsed hours.
fallBackDay :: Day
fallBackDay = fromGregorian 2025 11 2

-- | 2026-03-08. The clock skips 02:00 to 03:00 Pacific.
--
-- A 02:00 to 04:00 slot therefore covers 1 elapsed hour.
springForwardDay :: Day
springForwardDay = fromGregorian 2026 3 8

-- | A UTC time, given directly rather than through a Pacific `time`.
utcAt :: Day -> TimeOfDay -> UTCTime
utcAt day tod = UTCTime day (timeOfDayToTime tod)

-- | Fall back, 1.5 elapsed hours into a 2-hour episode. It is still playing.
--
-- 08:30 UTC is the first 01:30, which is still PDT.
fallBackWithinDuration :: TestDBConfig -> IO ()
fallBackWithinDuration cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = pacificToUtc (LocalTime fallBackDay (TimeOfDay 0 0 0))
      queryTime = utcAt fallBackDay (TimeOfDay 8 30 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _showId) <- setupTestData passHash (TimeOfDay 0 0 0) (TimeOfDay 2 0 0) Nothing scheduledAt (Just "audio/fallback.mp3") fallBackDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure (episodeId, mEpisode)
  case result of
    Left err -> error $ "DB error: " <> show err
    Right (episodeId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      episode.id `shouldBe` episodeId

-- | Fall back, 2.5 elapsed hours into a 2-hour episode. The audio has ended.
--
-- 09:30 UTC is the second 01:30, now PST. The Pacific clock still reads 01:30,
-- which is inside the 00:00 to 02:00 slot, so comparing `time` values keeps the
-- episode on air for about an hour after its audio ran out. Liquidsoap re-serves
-- the same file, which gives dead air or a stall.
fallBackStopsWhenAudioEnds :: TestDBConfig -> IO ()
fallBackStopsWhenAudioEnds cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = pacificToUtc (LocalTime fallBackDay (TimeOfDay 0 0 0))
      queryTime = utcAt fallBackDay (TimeOfDay 9 30 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash (TimeOfDay 0 0 0) (TimeOfDay 2 0 0) Nothing scheduledAt (Just "audio/fallback.mp3") fallBackDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure mEpisode
  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

-- | Fall back with no recorded duration. The episode fills the whole slot.
--
-- The slot covers 3 elapsed hours on this date, so the same 09:30 UTC that ends the
-- episode above is still inside the slot here. This pins the slot boundary as
-- a pair of `time` values, and not as a fixed 2 hours.
fallBackNullDurationFillsSlot :: TestDBConfig -> IO ()
fallBackNullDurationFillsSlot cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = pacificToUtc (LocalTime fallBackDay (TimeOfDay 0 0 0))
      insideSlot = utcAt fallBackDay (TimeOfDay 9 30 0)
      atSlotEnd = utcAt fallBackDay (TimeOfDay 10 0 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestDataWithDuration passHash (TimeOfDay 0 0 0) (TimeOfDay 2 0 0) Nothing scheduledAt (Just "audio/fallback.mp3") fallBackDay Nothing Nothing
    stillOn <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode insideSlot
    ended <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode atSlotEnd
    TRX.condemn
    pure (stillOn, ended)
  case result of
    Left err -> error $ "DB error: " <> show err
    Right (stillOn, ended) -> liftIO $ do
      isJust stillOn `shouldBe` True
      ended `shouldBe` Nothing

-- | Fall back. A 01:00 slot opens the first time the clock reads 01:00.
--
-- 01:00 to 01:59 happens twice on this date, so the local time 01:00 names two
-- instants, 08:00 UTC and 09:00 UTC. The query takes the first. Without that
-- correction the slot would open at 09:00 UTC and the hour before it would be
-- dead air, with nothing to explain the gap.
--
-- The slot is 01:00 to 02:00 and the episode carries 1 hour of audio.
fallBackOpensAtFirstReading :: TestDBConfig -> IO ()
fallBackOpensAtFirstReading cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = pacificToUtc (LocalTime fallBackDay (TimeOfDay 1 0 0))
      firstReading = utcAt fallBackDay (TimeOfDay 8 30 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _showId) <- setupTestDataWithDuration passHash (TimeOfDay 1 0 0) (TimeOfDay 2 0 0) Nothing scheduledAt (Just "audio/fallback.mp3") fallBackDay Nothing (Just 3600)
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode firstReading
    TRX.condemn
    pure (episodeId, mEpisode)
  case result of
    Left err -> error $ "DB error: " <> show err
    Right (episodeId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      episode.id `shouldBe` episodeId

-- | Fall back. The repeated hour is silent at the end of the slot, not the start.
--
-- The same 01:00 to 02:00 slot really covers 2 elapsed hours, and the episode
-- holds 1 hour of audio, so one hour has to be silent. It is the second one.
-- 09:30 UTC is the second 01:30, and by then the audio has ended.
fallBackSilenceAtSlotEnd :: TestDBConfig -> IO ()
fallBackSilenceAtSlotEnd cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = pacificToUtc (LocalTime fallBackDay (TimeOfDay 1 0 0))
      secondReading = utcAt fallBackDay (TimeOfDay 9 30 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestDataWithDuration passHash (TimeOfDay 1 0 0) (TimeOfDay 2 0 0) Nothing scheduledAt (Just "audio/fallback.mp3") fallBackDay Nothing (Just 3600)
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode secondReading
    TRX.condemn
    pure mEpisode
  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

-- | Fall back. A slot ends the first time the clock reads its end time.
--
-- Both ends of a window take the earlier of the two instants an ambiguous local
-- time names. The end has to follow the same rule as the start, or a slot would
-- close an hour after the next one opens and the two would air at once.
--
-- The slot runs 23:00 on 2025-11-01 to 01:30 on 2025-11-02, and 01:30 happens
-- twice. The episode carries no duration, so only the slot end can stop it.
-- 08:15 UTC is inside. 09:00 UTC is past the first 01:30 and before the second,
-- so it separates the two readings.
fallBackClosesAtFirstReading :: TestDBConfig -> IO ()
fallBackClosesAtFirstReading cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let airDate = addDays (-1) fallBackDay
      scheduledAt = pacificToUtc (LocalTime airDate (TimeOfDay 23 0 0))
      insideSlot = utcAt fallBackDay (TimeOfDay 8 15 0)
      pastFirstReading = utcAt fallBackDay (TimeOfDay 9 0 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestDataWithDuration passHash (TimeOfDay 23 0 0) (TimeOfDay 1 30 0) Nothing scheduledAt (Just "audio/fallback.mp3") airDate Nothing Nothing
    stillOn <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode insideSlot
    ended <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode pastFirstReading
    TRX.condemn
    pure (stillOn, ended)
  case result of
    Left err -> error $ "DB error: " <> show err
    Right (stillOn, ended) -> liftIO $ do
      isJust stillOn `shouldBe` True
      ended `shouldBe` Nothing

-- | Spring forward, 30 elapsed minutes into a slot that covers only 1 elapsed hour.
--
-- 10:30 UTC is 03:30 PDT. The slot opens at 02:00, which does not exist on this
-- date and normalizes forward to 03:00 PDT.
springForwardWithinSlot :: TestDBConfig -> IO ()
springForwardWithinSlot cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = pacificToUtc (LocalTime springForwardDay (TimeOfDay 2 0 0))
      queryTime = utcAt springForwardDay (TimeOfDay 10 30 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _showId) <- setupTestData passHash (TimeOfDay 2 0 0) (TimeOfDay 4 0 0) Nothing scheduledAt (Just "audio/spring.mp3") springForwardDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure (episodeId, mEpisode)
  case result of
    Left err -> error $ "DB error: " <> show err
    Right (episodeId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      episode.id `shouldBe` episodeId

-- | Spring forward. The slot ends while an hour of audio is still unplayed.
--
-- 11:00 UTC is 04:00 PDT, the slot's end. The episode carries a 2-hour duration
-- but only 1 elapsed hour of the slot exists on this date. The window boundary
-- wins, because the next show starts at 04:00.
springForwardCutAtSlotEnd :: TestDBConfig -> IO ()
springForwardCutAtSlotEnd cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = pacificToUtc (LocalTime springForwardDay (TimeOfDay 2 0 0))
      queryTime = utcAt springForwardDay (TimeOfDay 11 0 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash (TimeOfDay 2 0 0) (TimeOfDay 4 0 0) Nothing scheduledAt (Just "audio/spring.mp3") springForwardDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure mEpisode
  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

-- | Spring forward. A slot that opens inside the gap never airs.
--
-- No instant reads as a Pacific time from 02:00 to 02:59 on this date, and
-- PostgreSQL normalizes such a time forward by an hour. The two slots here cover
-- both shapes that produces:
--
-- * 02:00 to 03:00 collapses to an empty window, where start equals end
-- * 02:30 to 03:00 inverts, where the start lands after the end
--
-- Neither can air, because a row airs only at or after its start and before its
-- stop, and the stop is at most the end. Probed across 03:00 to 03:45 PDT, which
-- is where both windows sit.
springForwardGapSlotNeverAirs :: TestDBConfig -> IO ()
springForwardGapSlotNeverAirs cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let emptyAt = pacificToUtc (LocalTime springForwardDay (TimeOfDay 2 0 0))
      invertedAt = pacificToUtc (LocalTime springForwardDay (TimeOfDay 2 30 0))
      probes = map (utcAt springForwardDay) [TimeOfDay 10 0 0, TimeOfDay 10 15 0, TimeOfDay 10 30 0, TimeOfDay 10 45 0]
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (_, showId, userId) <-
      setupTestDataFull passHash (TimeOfDay 2 0 0) (TimeOfDay 3 0 0) Nothing emptyAt (Just "audio/empty.mp3") springForwardDay Nothing Nothing
    _ <- addTimeslot "inverted" userId (TimeOfDay 2 30 0) (TimeOfDay 3 0 0) Nothing invertedAt (Just "audio/inverted.mp3") springForwardDay Nothing
    airing <- traverse (TRX.statement () . Episodes.getCurrentlyAiringEpisodes) probes
    TRX.condemn
    pure airing
  case result of
    Left err -> error $ "DB error: " <> show err
    Right airing -> liftIO $ airing `shouldBe` map (const []) probes

--------------------------------------------------------------------------------
-- Two shows claiming the same time

-- | Two overlapping slots both hold 3:30 PM. The order must not vary.
--
-- An overlap is a data defect, and the conflict checks exist to stop it. When
-- one reaches the database anyway, the stream has to stay on one show rather
-- than swap between them on each poll. The later air time wins.
overlapIsDeterministic :: TestDBConfig -> IO ()
overlapIsDeterministic cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let earlyStart = TimeOfDay 14 0 0
      lateStart = TimeOfDay 15 0 0
      queryTime = mkTestTime (TimeOfDay 15 30 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (early, showId, userId) <- setupTestDataFull passHash earlyStart (TimeOfDay 16 0 0) Nothing (mkTestTime earlyStart) (Just "audio/early.mp3") testDay Nothing Nothing
    late <- addTimeslot "late" userId lateStart (TimeOfDay 17 0 0) Nothing (mkTestTime lateStart) (Just "audio/late.mp3") testDay Nothing
    both <- TRX.statement () $ Episodes.getCurrentlyAiringEpisodes queryTime
    picked <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure (early, late, both, picked)
  case result of
    Left err -> error $ "DB error: " <> show err
    Right (early, late, both, picked) -> liftIO $ do
      map (.id) both `shouldBe` [late, early]
      fmap (.id) picked `shouldBe` Just late

--------------------------------------------------------------------------------
-- Fixtures for the coverage tests below

-- | Clear @published_at@. 'Episodes.insertEpisode' always sets it to now.
unpublishEpisode :: Episodes.Id -> Hasql.Statement () ()
unpublishEpisode episodeId =
  interp False [sql| UPDATE episodes SET published_at = NULL WHERE id = #{episodeId} |]

-- | Detach an episode from its slot.
--
-- @episodes_schedule_consistency@ requires @schedule_template_id@ and
-- @scheduled_at@ to be NULL together, so this clears both.
detachEpisode :: Episodes.Id -> Hasql.Statement () ()
detachEpisode episodeId =
  interp
    False
    [sql| UPDATE episodes SET schedule_template_id = NULL, scheduled_at = NULL WHERE id = #{episodeId} |]

-- | Move a show's templates to another timezone.
setShowTimezone :: Shows.Id -> Text -> Hasql.Statement () ()
setShowTimezone showId zone =
  interp False [sql| UPDATE schedule_templates SET timezone = #{zone} WHERE show_id = #{showId} |]

-- | A second show, with its own host, template, validity and episode.
--
-- 'setupTestDataFull' hardcodes one email and one slug, so a cross-show test
-- needs a fixture of its own.
addSecondShow ::
  PasswordHash Argon2 ->
  -- | Start time
  TimeOfDay ->
  -- | End time
  TimeOfDay ->
  -- | Episode scheduled_at (UTC)
  UTCTime ->
  -- | Validity effective_from
  Day ->
  TRX.Transaction Episodes.Id
addSecondShow passHash startTime endTime scheduledAt effectiveFrom = do
  (OneRow userId) <-
    TRX.statement () $
      User.insertUser $
        User.ModelInsert (mkEmailAddress "second@example.com") passHash
  _ <-
    TRX.statement () $
      UserMetadata.insertUserMetadata $
        UserMetadata.Insert
          userId
          (mkDisplayNameUnsafe "Second Host")
          (mkFullNameUnsafe "Second Host")
          Nothing
          UserMetadata.Staff
          UserMetadata.Automatic
          UserMetadata.DefaultTheme
  showId <-
    unwrapInsert $
      Shows.insertShow
        Shows.Insert
          { siTitle = "Second Show",
            siSlug = mkSlug "second-show",
            siDescription = Nothing,
            siLogoUrl = Nothing,
            siStatus = Shows.Active
          }
  templateId <-
    TRX.statement () $
      ShowSchedule.insertScheduleTemplate
        (recurringOn (pacificDayOf scheduledAt) showId (startTime) (endTime) (Nothing))
  _ <-
    unwrapInsert $
      ShowSchedule.insertValidity
        ShowSchedule.ValidityInsert
          { viTemplateId = templateId,
            viEffectiveFrom = effectiveFrom,
            viEffectiveUntil = Nothing
          }
  unwrapInsert $
    Episodes.insertEpisode
      Episodes.Insert
        { eiId = showId,
          eiDescription = Just "Second Show Episode",
          eiAudioFilePath = Just "audio/second.mp3",
          eiAudioFileSize = Just 1000000,
          eiAudioMimeType = Just "audio/mpeg",
          eiDurationSeconds = Just (truncate (timeOfDayToTime endTime - timeOfDayToTime startTime)),
          eiArtworkUrl = Nothing,
          eiScheduleTemplateId = Just templateId,
          eiScheduledAt = Just scheduledAt,
          eiCreatedBy = userId
        }

--------------------------------------------------------------------------------
-- A replay window that crosses midnight
--
-- The query builds one window per (episode, primary or replay) pair and wraps
-- any window whose end is at or before its start. A replay wraps through the
-- same rule the primary does, and these three cases are the only coverage of
-- that branch for a replay.
--
-- Primary 22:00 to 23:00, so the slot is 1 hour and the replay at 23:30 runs to
-- 00:30 on the following date.

-- | The replay is on air before midnight.
replayCrossesMidnightBefore :: TestDBConfig -> IO ()
replayCrossesMidnightBefore cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = mkTestTime (TimeOfDay 22 0 0)
      queryTime = mkTestTime (TimeOfDay 23 45 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash (TimeOfDay 22 0 0) (TimeOfDay 23 0 0) (Just (TimeOfDay 23 30 0)) scheduledAt (Just "audio/test.mp3") testDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure (episodeId, mEpisode)
  case result of
    Left err -> error $ "DB error: " <> show err
    Right (episodeId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      episode.id `shouldBe` episodeId

-- | The replay is still on air after midnight, on the following date.
--
-- The episode's air date is the previous date. Only the date prune and the
-- window wrap keep it reachable.
replayCrossesMidnightAfter :: TestDBConfig -> IO ()
replayCrossesMidnightAfter cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = mkTestTime (TimeOfDay 22 0 0)
      queryTime = mkTestTimeNextDay (TimeOfDay 0 15 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash (TimeOfDay 22 0 0) (TimeOfDay 23 0 0) (Just (TimeOfDay 23 30 0)) scheduledAt (Just "audio/test.mp3") testDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure (episodeId, mEpisode)
  case result of
    Left err -> error $ "DB error: " <> show err
    Right (episodeId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      episode.id `shouldBe` episodeId

-- | The replay stops at its own end on the following date.
replayCrossesMidnightEnds :: TestDBConfig -> IO ()
replayCrossesMidnightEnds cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = mkTestTime (TimeOfDay 22 0 0)
      queryTime = mkTestTimeNextDay (TimeOfDay 0 30 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash (TimeOfDay 22 0 0) (TimeOfDay 23 0 0) (Just (TimeOfDay 23 30 0)) scheduledAt (Just "audio/test.mp3") testDay Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure mEpisode
  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Ordering and the row limit

-- | A primary airing sorts ahead of a replay that covers the same time.
--
-- @is_replay@ is the first ORDER BY key, and this is the only test that isolates
-- it. The replayed episode must carry the *later* @scheduled_at@, or the second
-- key alone would produce the same order and the test would prove nothing.
--
-- The live slot is 18:00 to 23:00 with its episode at 18:00. The replayed slot is
-- 20:00 to 21:00 with its episode at 20:00 and a replay from 22:00 to 23:00. At
-- 22:30 both windows are open, and the replayed episode has the later air time.
primaryBeatsReplay :: TestDBConfig -> IO ()
primaryBeatsReplay cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let queryTime = mkTestTime (TimeOfDay 22 30 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (live, showId, userId) <-
      setupTestDataFull passHash (TimeOfDay 18 0 0) (TimeOfDay 23 0 0) Nothing (mkTestTime (TimeOfDay 18 0 0)) (Just "audio/live.mp3") testDay Nothing Nothing
    replayed <- addTimeslot "replayed" userId (TimeOfDay 20 0 0) (TimeOfDay 21 0 0) (Just (TimeOfDay 22 0 0)) (mkTestTime (TimeOfDay 20 0 0)) (Just "audio/replayed.mp3") testDay Nothing
    both <- TRX.statement () $ Episodes.getCurrentlyAiringEpisodes queryTime
    picked <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure (replayed, live, both, picked)
  case result of
    Left err -> error $ "DB error: " <> show err
    Right (replayed, live, both, picked) -> liftIO $ do
      -- Without the is_replay key, scheduled_at DESC would put replayed first.
      map (.id) both `shouldBe` [live, replayed]
      fmap (.id) picked `shouldBe` Just live

-- | Two different shows claiming one time both come back, in a stable order.
--
-- This is the shape the cross-show conflict checks exist to prevent. When one
-- reaches the database anyway, the later air time wins and the caller sees both.
twoShowsOverlap :: TestDBConfig -> IO ()
twoShowsOverlap cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let queryTime = mkTestTime (TimeOfDay 15 30 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (first, _showId) <-
      setupTestData passHash (TimeOfDay 14 0 0) (TimeOfDay 16 0 0) Nothing (mkTestTime (TimeOfDay 14 0 0)) (Just "audio/first.mp3") testDay Nothing
    second <- addSecondShow passHash (TimeOfDay 15 0 0) (TimeOfDay 17 0 0) (mkTestTime (TimeOfDay 15 0 0)) testDay
    both <- TRX.statement () $ Episodes.getCurrentlyAiringEpisodes queryTime
    picked <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure (first, second, both, picked)
  case result of
    Left err -> error $ "DB error: " <> show err
    Right (first, second, both, picked) -> liftIO $ do
      map (.id) both `shouldBe` [second, first]
      fmap (.id) picked `shouldBe` Just second

--------------------------------------------------------------------------------
-- Documented behaviour that must not drift

-- | A duration of 0 makes the window empty, so the episode never airs.
--
-- @window_stop@ is @LEAST(window_end, window_start + 0)@, which is
-- @window_start@, and the test is @currentTime < window_stop@. Nothing validates
-- @duration_seconds@ on the way in.
zeroDurationNeverAirs :: TestDBConfig -> IO ()
zeroDurationNeverAirs cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = mkTestTime (TimeOfDay 14 0 0)
      queryTime = mkTestTime (TimeOfDay 15 0 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestDataWithDuration passHash (TimeOfDay 14 0 0) (TimeOfDay 16 0 0) Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing (Just 0)
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure mEpisode
  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

-- | An episode with no @published_at@ still airs.
--
-- The query has no @published_at@ filter. Whether it should is a separate
-- question. This test states what it does, so a change is deliberate.
unpublishedEpisodeStillAirs :: TestDBConfig -> IO ()
unpublishedEpisodeStillAirs cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = mkTestTime (TimeOfDay 14 0 0)
      queryTime = mkTestTime (TimeOfDay 15 0 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash (TimeOfDay 14 0 0) (TimeOfDay 16 0 0) Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    TRX.statement () $ unpublishEpisode episodeId
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure (episodeId, mEpisode)
  case result of
    Left err -> error $ "DB error: " <> show err
    Right (episodeId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      episode.id `shouldBe` episodeId

-- | A detached episode never airs.
--
-- The join to @schedule_templates@ drops a NULL @schedule_template_id@, and the
-- air date of a NULL @scheduled_at@ is NULL, which fails the date test. This is
-- the UNSCHEDULED state a removed slot leaves behind.
detachedEpisodeNeverAirs :: TestDBConfig -> IO ()
detachedEpisodeNeverAirs cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = mkTestTime (TimeOfDay 14 0 0)
      queryTime = mkTestTime (TimeOfDay 15 0 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash (TimeOfDay 14 0 0) (TimeOfDay 16 0 0) Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    TRX.statement () $ detachEpisode episodeId
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure mEpisode
  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

-- | Equal start and end times describe a 24-hour window.
--
-- @end_time <= start_time@ takes the wrapping branch, and the slot length is
-- @24 hours - 0@. The window therefore closes at the same clock time on the
-- following date.
equalTimesGiveFullDay :: TestDBConfig -> IO ()
equalTimesGiveFullDay cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = mkTestTime (TimeOfDay 14 0 0)
      lateNextDay = mkTestTimeNextDay (TimeOfDay 13 59 0)
      atWrap = mkTestTimeNextDay (TimeOfDay 14 0 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash (TimeOfDay 14 0 0) (TimeOfDay 14 0 0) Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    stillOn <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode lateNextDay
    ended <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode atWrap
    TRX.condemn
    pure (stillOn, ended)
  case result of
    Left err -> error $ "DB error: " <> show err
    Right (stillOn, ended) -> liftIO $ do
      isJust stillOn `shouldBe` True
      ended `shouldBe` Nothing

-- | The query ignores @schedule_templates.timezone@ and always uses Pacific.
--
-- The template below says New York, and the episode still airs on its Pacific
-- hours. @getUpcomingShowDates@ reads @timezone@, so the two disagree. Every
-- template is Pacific today, which is why nothing has noticed.
templateTimezoneIsIgnored :: TestDBConfig -> IO ()
templateTimezoneIsIgnored cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let scheduledAt = mkTestTime (TimeOfDay 14 0 0)
      pacificMidShow = mkTestTime (TimeOfDay 15 0 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, showId) <- setupTestData passHash (TimeOfDay 14 0 0) (TimeOfDay 16 0 0) Nothing scheduledAt (Just "audio/test.mp3") testDay Nothing
    TRX.statement () $ setShowTimezone showId "America/New_York"
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode pacificMidShow
    TRX.condemn
    pure (episodeId, mEpisode)
  case result of
    Left err -> error $ "DB error: " <> show err
    Right (episodeId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      episode.id `shouldBe` episodeId

-- | An episode from two dates ago never airs.
--
-- The date prune keeps only today and yesterday. A window opens on the air date
-- and closes at most one date later, so the prune can never change the answer.
-- It bounds the scan. This test guards the outcome, not the prune.
oldEpisodeNeverAirs :: TestDBConfig -> IO ()
oldEpisodeNeverAirs cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let twoDaysAgo = addDays (-2) testDay
      scheduledAt = pacificToUtc (LocalTime twoDaysAgo (TimeOfDay 14 0 0))
      queryTime = mkTestTime (TimeOfDay 15 0 0)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- setupTestData passHash (TimeOfDay 14 0 0) (TimeOfDay 16 0 0) Nothing scheduledAt (Just "audio/test.mp3") (addDays (-30) testDay) Nothing
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    TRX.condemn
    pure mEpisode
  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing
