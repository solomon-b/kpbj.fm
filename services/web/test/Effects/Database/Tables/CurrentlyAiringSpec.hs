-- | Tests for getCurrentlyAiringEpisode query.
--
-- This is critical functionality for Liquidsoap integration - the query must
-- correctly determine what episode is currently airing based on:
--
-- 1. Schedule template time slots (start_time, end_time)
-- 2. Schedule validity periods (effective_from, effective_until)
-- 3. Episode scheduled_at matching today's date
-- 4. Overnight shows (end_time <= start_time, e.g., 11 PM - 2 AM)
-- 5. Replay airings (airs_twice_daily = TRUE, +12 hours offset)
-- 6. Audio file presence and episode deletion status
module Effects.Database.Tables.CurrentlyAiringSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Data.Password.Argon2 (Argon2, PasswordHash, hashPassword, mkPassword)
import Data.Text (Text)
import Data.Time
  ( Day,
    LocalTime (..),
    TimeOfDay (..),
    TimeZone,
    UTCTime (..),
    addDays,
    fromGregorian,
    localTimeToUTC,
  )
import Data.Time.LocalTime (hoursToTimeZone)
import Domain.Types.DisplayName (mkDisplayNameUnsafe)
import Domain.Types.EmailAddress (mkEmailAddress)
import Domain.Types.FullName (mkFullNameUnsafe)
import Domain.Types.Slug (mkSlug)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Interpolate (OneRow (..))
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
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
        it "returns Nothing when episode is scheduled for different day" basicDifferentDay
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

      -- Replay airing tests (airs_twice_daily)
      describe "airs_twice_daily replay" $ do
        it "returns the episode during primary airing" replayPrimaryAiring
        it "returns the episode during replay airing (+12 hours)" replaySecondAiring
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

--------------------------------------------------------------------------------
-- Test Helpers

-- | Pacific timezone (UTC-8, ignoring DST for simplicity in tests)
pacificTZ :: TimeZone
pacificTZ = hoursToTimeZone (-8)

-- | Convert Pacific local time to UTC
pacificToUTC :: LocalTime -> UTCTime
pacificToUTC = localTimeToUTC pacificTZ

-- | A test date: Monday, January 6, 2025
testDay :: Day
testDay = fromGregorian 2025 1 6

-- | Create a UTC time from a Pacific time on the test day
mkTestTime :: TimeOfDay -> UTCTime
mkTestTime tod = pacificToUTC (LocalTime testDay tod)

-- | Create a UTC time from a Pacific time on the day after test day (for overnight shows)
mkTestTimeNextDay :: TimeOfDay -> UTCTime
mkTestTimeNextDay tod = pacificToUTC (LocalTime (addDays 1 testDay) tod)

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
  -- | Airs twice daily?
  Bool ->
  -- | Episode scheduled_at (UTC)
  UTCTime ->
  -- | Audio file path (Nothing = no audio)
  Maybe Text ->
  -- | Validity effective_from
  Day ->
  -- | Validity effective_until
  Maybe Day ->
  TRX.Transaction (Episodes.Id, Shows.Id)
setupTestData passHash startTime endTime airsTwiceDaily scheduledAt mAudioPath effectiveFrom effectiveUntil = do
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
    TRX.statement () $
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
        ShowSchedule.ScheduleTemplateInsert
          { stiShowId = showId,
            stiDayOfWeek = Nothing, -- One-time show for simplicity
            stiWeeksOfMonth = Nothing,
            stiStartTime = startTime,
            stiEndTime = endTime,
            stiTimezone = "America/Los_Angeles",
            stiAirsTwiceDaily = airsTwiceDaily
          }

  -- Create validity period
  _ <-
    TRX.statement () $
      ShowSchedule.insertValidity
        ShowSchedule.ValidityInsert
          { viTemplateId = templateId,
            viEffectiveFrom = effectiveFrom,
            viEffectiveUntil = effectiveUntil
          }

  -- Create episode
  episodeId <-
    TRX.statement () $
      Episodes.insertEpisode
        Episodes.Insert
          { eiId = showId,
            eiDescription = Just "Test Episode",
            eiAudioFilePath = mAudioPath,
            eiAudioFileSize = if isJust mAudioPath then Just 1000000 else Nothing,
            eiAudioMimeType = if isJust mAudioPath then Just "audio/mpeg" else Nothing,
            eiDurationSeconds = Just 3600,
            eiArtworkUrl = Nothing,
            eiScheduleTemplateId = templateId,
            eiScheduledAt = scheduledAt,
            eiCreatedBy = userId
          }

  pure (episodeId, showId)

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
    _ <- setupTestData passHash startTime endTime False scheduledAt Nothing testDay Nothing
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
    (episodeId, _) <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") testDay Nothing
    -- Soft delete the episode
    _ <- TRX.statement () $ Episodes.deleteEpisode episodeId
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
    _ <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") testDay Nothing
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
    (episodeId, _) <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") testDay Nothing
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
    _ <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") testDay Nothing
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
    (episodeId, _) <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") testDay Nothing
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
    (episodeId, _) <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") testDay Nothing
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
    _ <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") testDay Nothing
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
    _ <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") testDay Nothing
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
    _ <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") testDay Nothing
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
    (episodeId, _) <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") testDay Nothing
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
    (episodeId, _) <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") testDay Nothing
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
    _ <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") testDay Nothing
    TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime

  case result of
    Left err -> error $ "DB error: " <> show err
    Right mEpisode -> liftIO $ mEpisode `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Replay Airing Tests (airs_twice_daily)

replayPrimaryAiring :: TestDBConfig -> IO ()
replayPrimaryAiring cfg = bracketConn cfg $ do
  passHash <- hashPassword $ mkPassword "testpass"
  let startTime = TimeOfDay 6 0 0 -- 6 AM
      endTime = TimeOfDay 8 0 0 -- 8 AM
      scheduledAt = mkTestTime startTime
      queryTime = mkTestTime (TimeOfDay 7 0 0) -- 7 AM (during primary)
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (episodeId, _) <- setupTestData passHash startTime endTime True scheduledAt (Just "audio/test.mp3") testDay Nothing
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
    (episodeId, _) <- setupTestData passHash startTime endTime True scheduledAt (Just "audio/test.mp3") testDay Nothing
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
    _ <- setupTestData passHash startTime endTime True scheduledAt (Just "audio/test.mp3") testDay Nothing
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
    _ <- setupTestData passHash startTime endTime True scheduledAt (Just "audio/test.mp3") testDay Nothing
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
    _ <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") effectiveFrom Nothing
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
    _ <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") effectiveFrom effectiveUntil
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
    (episodeId, _) <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") effectiveFrom Nothing
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
    (episodeId, _) <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") effectiveFrom effectiveUntil
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
    _ <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") effectiveFrom effectiveUntil
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
    (episodeId, _) <- setupTestData passHash startTime endTime False scheduledAt (Just "audio/test.mp3") effectiveFrom effectiveUntil
    mEpisode <- TRX.statement () $ Episodes.getCurrentlyAiringEpisode queryTime
    pure (episodeId, mEpisode)

  case result of
    Left err -> error $ "DB error: " <> show err
    Right (expectedId, mEpisode) -> liftIO $ do
      episode <- assertJustIO mEpisode
      Episodes.id episode `shouldBe` expectedId
