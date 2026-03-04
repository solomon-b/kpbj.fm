module Effects.Database.Tables.ShowScheduleMissingEpisodesSpec where

--------------------------------------------------------------------------------

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Display (display)
import Data.Time (addDays, utctDay)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (TimeOfDay (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog qualified
import Hedgehog.Internal.Property (forAllT)
import OrphanInstances.DayOfWeek (toDayOfWeek)
import Test.Database.Helpers (addTestShowHost, insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight)
import Test.Gen.Tables.ShowSchedule (allWeeksOfMonth, genTimezone)
import Test.Gen.Tables.Shows (showInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $ do
    describe "Effects.Database.Tables.ShowSchedule.getShowsMissingEpisodes" $ do
      runs 10 . it "show with schedule in next 7 days and no episode appears in results" $ hedgehog . prop_missingEpisodeAppears
      runs 10 . it "show with schedule and episode WITH audio does NOT appear" $ hedgehog . prop_episodeWithAudioNotMissing
      runs 10 . it "show with schedule and episode WITHOUT audio appears in results" $ hedgehog . prop_episodeWithoutAudioIsMissing
      runs 10 . it "show scheduled more than 7 days out does NOT appear" $ hedgehog . prop_beyondWindowNotShown
      runs 10 . it "deleted show does NOT appear" $ hedgehog . prop_deletedShowNotShown
      runs 10 . it "results sorted by scheduled date ascending" $ hedgehog . prop_sortedByDate
    describe "Effects.Database.Tables.ShowSchedule.getHostsMissingEpisodesOnDay" $ do
      runs 10 . it "returns per-host rows with email for missing episode on target day" $ hedgehog . prop_hostMissingEpisodeOnDay
      runs 10 . it "does not return hosts for shows with uploaded audio" $ hedgehog . prop_hostNotReturnedWithAudio
      runs 10 . it "does not return rows when no host is assigned" $ hedgehog . prop_noHostNoRow
      runs 10 . it "does not return shows scheduled on a different day" $ hedgehog . prop_wrongDayNotReturned
      runs 10 . it "does not return hosts for deleted shows" $ hedgehog . prop_hostDeletedShowNotReturned
      runs 10 . it "returns host when episode exists without audio" $ hedgehog . prop_hostReturnedWithoutAudio
      runs 10 . it "returns one row per host for multi-host show" $ hedgehog . prop_multipleHostsMultipleRows
      runs 10 . it "does not return hosts for expired schedule validity" $ hedgehog . prop_hostExpiredValidityNotReturned

--------------------------------------------------------------------------------

-- | Show with schedule in next 7 days and no episode appears in results.
prop_missingEpisodeAppears :: TestDBConfig -> PropertyT IO ()
prop_missingEpisodeAppears cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      -- Use today's day of week so the schedule matches within the 7-day window
      let targetDow = toDayOfWeek $ let (_, _, d) = toWeekDate today in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let show1 = showInsert {Shows.siStatus = Shows.Active}
        showId <- unwrapInsert (Shows.insertShow show1)

        let scheduleInsert = ShowSchedule.ScheduleTemplateInsert showId (Just targetDow) (Just allWeeksOfMonth) (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) timezone False
        templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleInsert)
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-7) today) Nothing))

        missing <- TRX.statement () ShowSchedule.getShowsMissingEpisodes
        TRX.condemn
        pure (show1, missing)

      assert $ do
        (show1, missing) <- assertRight result
        let matchingShows = filter (\s -> ShowSchedule.smeShowTitle s == Shows.siTitle show1) missing
        Hedgehog.assert (not $ null matchingShows)

-- | Show with schedule and episode WITH audio does NOT appear.
--
-- Uses UTC timezone so the scheduled_at timestamp computation matches:
-- (show_date || start_time)::TIMESTAMP AT TIME ZONE 'UTC' = show_date start_time in UTC
prop_episodeWithAudioNotMissing :: TestDBConfig -> PropertyT IO ()
prop_episodeWithAudioNotMissing cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    userWithMetadata <- forAllT userWithMetadataInsertGen
    -- Use UTC so scheduled_at computation is straightforward
    let timezone = "UTC"

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      let targetDow = toDayOfWeek $ let (_, _, d) = toWeekDate today in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let show1 = showInsert {Shows.siStatus = Shows.Active}
        showId <- unwrapInsert (Shows.insertShow show1)

        let scheduleInsert = ShowSchedule.ScheduleTemplateInsert showId (Just targetDow) (Just allWeeksOfMonth) (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) timezone False
        templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleInsert)
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-7) today) Nothing))

        -- Insert episode WITH audio, scheduled_at matches: (today 10:00:00) AT TIME ZONE 'UTC'
        let episodeInsert =
              Episodes.Insert
                { eiId = showId,
                  eiDescription = Nothing,
                  eiAudioFilePath = Just "/audio/test.mp3",
                  eiAudioFileSize = Just 1000,
                  eiAudioMimeType = Just "audio/mpeg",
                  eiDurationSeconds = Just 3600,
                  eiArtworkUrl = Nothing,
                  eiScheduleTemplateId = Just templateId,
                  eiScheduledAt = Just (read (show today ++ " 10:00:00 UTC")),
                  eiCreatedBy = userId
                }
        _ <- unwrapInsert (Episodes.insertEpisode episodeInsert)

        missing <- TRX.statement () ShowSchedule.getShowsMissingEpisodes
        TRX.condemn
        pure (show1, missing)

      assert $ do
        (show1, missing) <- assertRight result
        let matchingShows = filter (\s -> ShowSchedule.smeShowTitle s == Shows.siTitle show1) missing
        -- Should NOT appear since episode has audio
        length matchingShows === 0

-- | Show with schedule and episode WITHOUT audio appears in results.
--
-- Uses UTC timezone so the scheduled_at timestamp computation matches.
prop_episodeWithoutAudioIsMissing :: TestDBConfig -> PropertyT IO ()
prop_episodeWithoutAudioIsMissing cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    userWithMetadata <- forAllT userWithMetadataInsertGen
    let timezone = "UTC"

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      let targetDow = toDayOfWeek $ let (_, _, d) = toWeekDate today in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let show1 = showInsert {Shows.siStatus = Shows.Active}
        showId <- unwrapInsert (Shows.insertShow show1)

        let scheduleInsert = ShowSchedule.ScheduleTemplateInsert showId (Just targetDow) (Just allWeeksOfMonth) (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) timezone False
        templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleInsert)
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-7) today) Nothing))

        -- Insert episode WITHOUT audio
        let episodeInsert =
              Episodes.Insert
                { eiId = showId,
                  eiDescription = Nothing,
                  eiAudioFilePath = Nothing,
                  eiAudioFileSize = Nothing,
                  eiAudioMimeType = Nothing,
                  eiDurationSeconds = Nothing,
                  eiArtworkUrl = Nothing,
                  eiScheduleTemplateId = Just templateId,
                  eiScheduledAt = Just (read (show today ++ " 10:00:00 UTC")),
                  eiCreatedBy = userId
                }
        _ <- unwrapInsert (Episodes.insertEpisode episodeInsert)

        missing <- TRX.statement () ShowSchedule.getShowsMissingEpisodes
        TRX.condemn
        pure (show1, missing)

      assert $ do
        (show1, missing) <- assertRight result
        let matchingShows = filter (\s -> ShowSchedule.smeShowTitle s == Shows.siTitle show1) missing
        -- Should appear since episode has no audio
        Hedgehog.assert (not $ null matchingShows)

-- | Show scheduled more than 7 days out does NOT appear.
prop_beyondWindowNotShown :: TestDBConfig -> PropertyT IO ()
prop_beyondWindowNotShown cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      -- Use the day of week that is 10 days from now (won't be within 7-day window)
      let futureDate = addDays 10 today
          futureDow = toDayOfWeek $ let (_, _, d) = toWeekDate futureDate in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let show1 = showInsert {Shows.siStatus = Shows.Active}
        showId <- unwrapInsert (Shows.insertShow show1)

        let scheduleInsert = ShowSchedule.ScheduleTemplateInsert showId (Just futureDow) (Just allWeeksOfMonth) (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) timezone False
        templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleInsert)
        -- Validity starts from 8 days from now (outside 7-day window)
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays 8 today) Nothing))

        missing <- TRX.statement () ShowSchedule.getShowsMissingEpisodes
        TRX.condemn
        pure (show1, missing)

      assert $ do
        (show1, missing) <- assertRight result
        let matchingShows = filter (\s -> ShowSchedule.smeShowTitle s == Shows.siTitle show1) missing
        length matchingShows === 0

-- | Deleted show does NOT appear.
prop_deletedShowNotShown :: TestDBConfig -> PropertyT IO ()
prop_deletedShowNotShown cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      let targetDow = toDayOfWeek $ let (_, _, d) = toWeekDate today in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let show1 = showInsert {Shows.siStatus = Shows.Active}
        showId <- unwrapInsert (Shows.insertShow show1)

        let scheduleInsert = ShowSchedule.ScheduleTemplateInsert showId (Just targetDow) (Just allWeeksOfMonth) (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) timezone False
        templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleInsert)
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-7) today) Nothing))

        -- Soft delete the show
        _ <- TRX.statement () (Shows.softDeleteShow showId)

        missing <- TRX.statement () ShowSchedule.getShowsMissingEpisodes
        TRX.condemn
        pure (show1, missing)

      assert $ do
        (show1, missing) <- assertRight result
        let matchingShows = filter (\s -> ShowSchedule.smeShowTitle s == Shows.siTitle show1) missing
        length matchingShows === 0

-- | Results sorted by scheduled date ascending.
prop_sortedByDate :: TestDBConfig -> PropertyT IO ()
prop_sortedByDate cfg = do
  arrange (bracketConn cfg) $ do
    showInsert1 <- forAllT showInsertGen
    showInsert2 <- forAllT showInsertGen
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      let targetDow = toDayOfWeek $ let (_, _, d) = toWeekDate today in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Create two shows on the same day but different times
        let show1 = showInsert1 {Shows.siStatus = Shows.Active, Shows.siSlug = Shows.siSlug showInsert1 <> "sort1"}
        showId1 <- unwrapInsert (Shows.insertShow show1)
        let sched1 = ShowSchedule.ScheduleTemplateInsert showId1 (Just targetDow) (Just allWeeksOfMonth) (TimeOfDay 14 0 0) (TimeOfDay 16 0 0) timezone False
        tid1 <- TRX.statement () (ShowSchedule.insertScheduleTemplate sched1)
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert tid1 (addDays (-7) today) Nothing))

        let show2 = showInsert2 {Shows.siStatus = Shows.Active, Shows.siSlug = Shows.siSlug showInsert2 <> "sort2"}
        showId2 <- unwrapInsert (Shows.insertShow show2)
        let sched2 = ShowSchedule.ScheduleTemplateInsert showId2 (Just targetDow) (Just allWeeksOfMonth) (TimeOfDay 8 0 0) (TimeOfDay 10 0 0) timezone False
        tid2 <- TRX.statement () (ShowSchedule.insertScheduleTemplate sched2)
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert tid2 (addDays (-7) today) Nothing))

        missing <- TRX.statement () ShowSchedule.getShowsMissingEpisodes
        TRX.condemn
        pure (show1, show2, missing)

      assert $ do
        (_show1, _show2, missing) <- assertRight result
        -- Results should be sorted by date ASC, then start_time ASC
        let dates = map (\m -> (ShowSchedule.smeShowDate m, ShowSchedule.smeStartTime m)) missing
        forM_ (zip dates (drop 1 dates)) $ \((d1, t1), (d2, t2)) ->
          Hedgehog.assert (d1 < d2 || (d1 == d2 && t1 <= t2))

--------------------------------------------------------------------------------
-- getHostsMissingEpisodesOnDay properties

-- | Show with host and missing episode on target day returns per-host row.
prop_hostMissingEpisodeOnDay :: TestDBConfig -> PropertyT IO ()
prop_hostMissingEpisodeOnDay cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      let targetDate = addDays 5 today
          targetDow = toDayOfWeek $ let (_, _, d) = toWeekDate targetDate in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let show1 = showInsert {Shows.siStatus = Shows.Active}
        showId <- unwrapInsert (Shows.insertShow show1)
        addTestShowHost showId userId

        let scheduleInsert = ShowSchedule.ScheduleTemplateInsert showId (Just targetDow) (Just allWeeksOfMonth) (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) "UTC" False
        templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleInsert)
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-7) today) Nothing))

        missing <- TRX.statement () (ShowSchedule.getHostsMissingEpisodesOnDay 5)
        TRX.condemn
        pure (show1, userWithMetadata, missing)

      assert $ do
        (show1, userMeta, missing) <- assertRight result
        let matchingRows = filter (\h -> ShowSchedule.hmeShowTitle h == Shows.siTitle show1) missing
        Hedgehog.assert (not $ null matchingRows)
        let emails = map ShowSchedule.hmeHostEmail matchingRows
        Hedgehog.assert (display (UserMetadata.uwmiEmail userMeta) `elem` emails)


-- | Show with host and episode WITH audio does not return host row.
prop_hostNotReturnedWithAudio :: TestDBConfig -> PropertyT IO ()
prop_hostNotReturnedWithAudio cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    userWithMetadata <- forAllT userWithMetadataInsertGen
    let timezone = "UTC"

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      let targetDate = addDays 5 today
          targetDow = toDayOfWeek $ let (_, _, d) = toWeekDate targetDate in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let show1 = showInsert {Shows.siStatus = Shows.Active}
        showId <- unwrapInsert (Shows.insertShow show1)
        addTestShowHost showId userId

        let scheduleInsert = ShowSchedule.ScheduleTemplateInsert showId (Just targetDow) (Just allWeeksOfMonth) (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) timezone False
        templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleInsert)
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-7) today) Nothing))

        let episodeInsert =
              Episodes.Insert
                { eiId = showId,
                  eiDescription = Nothing,
                  eiAudioFilePath = Just "/audio/test.mp3",
                  eiAudioFileSize = Just 1000,
                  eiAudioMimeType = Just "audio/mpeg",
                  eiDurationSeconds = Just 3600,
                  eiArtworkUrl = Nothing,
                  eiScheduleTemplateId = Just templateId,
                  eiScheduledAt = Just (read (show targetDate ++ " 10:00:00 UTC")),
                  eiCreatedBy = userId
                }
        _ <- unwrapInsert (Episodes.insertEpisode episodeInsert)

        missing <- TRX.statement () (ShowSchedule.getHostsMissingEpisodesOnDay 5)
        TRX.condemn
        pure (show1, missing)

      assert $ do
        (show1, missing) <- assertRight result
        let matchingRows = filter (\h -> ShowSchedule.hmeShowTitle h == Shows.siTitle show1) missing
        length matchingRows === 0


-- | Show with missing episode but no host assigned returns no rows.
prop_noHostNoRow :: TestDBConfig -> PropertyT IO ()
prop_noHostNoRow cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      let targetDate = addDays 5 today
          targetDow = toDayOfWeek $ let (_, _, d) = toWeekDate targetDate in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let show1 = showInsert {Shows.siStatus = Shows.Active}
        showId <- unwrapInsert (Shows.insertShow show1)

        let scheduleInsert = ShowSchedule.ScheduleTemplateInsert showId (Just targetDow) (Just allWeeksOfMonth) (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) "UTC" False
        templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleInsert)
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-7) today) Nothing))

        missing <- TRX.statement () (ShowSchedule.getHostsMissingEpisodesOnDay 5)
        TRX.condemn
        pure (show1, missing)

      assert $ do
        (show1, missing) <- assertRight result
        let matchingRows = filter (\h -> ShowSchedule.hmeShowTitle h == Shows.siTitle show1) missing
        length matchingRows === 0


-- | Show scheduled one day before the queried day does not appear.
--
-- Schedules a show for day +4 but queries with day +5. The query checks
-- exactly CURRENT_DATE + N, not a range, so a show one day off should
-- not be returned.
prop_wrongDayNotReturned :: TestDBConfig -> PropertyT IO ()
prop_wrongDayNotReturned cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      -- Schedule on day +4, but query for day +5
      let nearbyDate = addDays 4 today
          nearbyDow = toDayOfWeek $ let (_, _, d) = toWeekDate nearbyDate in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let show1 = showInsert {Shows.siStatus = Shows.Active}
        showId <- unwrapInsert (Shows.insertShow show1)
        addTestShowHost showId userId

        let scheduleInsert = ShowSchedule.ScheduleTemplateInsert showId (Just nearbyDow) (Just allWeeksOfMonth) (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) "UTC" False
        templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleInsert)
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-7) today) Nothing))

        -- Query for day +5, but show is scheduled for day +4
        missing <- TRX.statement () (ShowSchedule.getHostsMissingEpisodesOnDay 5)
        TRX.condemn
        pure (show1, missing)

      assert $ do
        (show1, missing) <- assertRight result
        let matchingRows = filter (\h -> ShowSchedule.hmeShowTitle h == Shows.siTitle show1) missing
        length matchingRows === 0


-- | Soft-deleted show with host does not appear.
prop_hostDeletedShowNotReturned :: TestDBConfig -> PropertyT IO ()
prop_hostDeletedShowNotReturned cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      let targetDate = addDays 5 today
          targetDow = toDayOfWeek $ let (_, _, d) = toWeekDate targetDate in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let show1 = showInsert {Shows.siStatus = Shows.Active}
        showId <- unwrapInsert (Shows.insertShow show1)
        addTestShowHost showId userId

        let scheduleInsert = ShowSchedule.ScheduleTemplateInsert showId (Just targetDow) (Just allWeeksOfMonth) (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) "UTC" False
        templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleInsert)
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-7) today) Nothing))

        _ <- TRX.statement () (Shows.softDeleteShow showId)

        missing <- TRX.statement () (ShowSchedule.getHostsMissingEpisodesOnDay 5)
        TRX.condemn
        pure (show1, missing)

      assert $ do
        (show1, missing) <- assertRight result
        let matchingRows = filter (\h -> ShowSchedule.hmeShowTitle h == Shows.siTitle show1) missing
        length matchingRows === 0


-- | Episode exists but has no audio file — host should still be returned.
prop_hostReturnedWithoutAudio :: TestDBConfig -> PropertyT IO ()
prop_hostReturnedWithoutAudio cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    userWithMetadata <- forAllT userWithMetadataInsertGen
    let timezone = "UTC"

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      let targetDate = addDays 5 today
          targetDow = toDayOfWeek $ let (_, _, d) = toWeekDate targetDate in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let show1 = showInsert {Shows.siStatus = Shows.Active}
        showId <- unwrapInsert (Shows.insertShow show1)
        addTestShowHost showId userId

        let scheduleInsert = ShowSchedule.ScheduleTemplateInsert showId (Just targetDow) (Just allWeeksOfMonth) (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) timezone False
        templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleInsert)
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-7) today) Nothing))

        let episodeInsert =
              Episodes.Insert
                { eiId = showId,
                  eiDescription = Nothing,
                  eiAudioFilePath = Nothing,
                  eiAudioFileSize = Nothing,
                  eiAudioMimeType = Nothing,
                  eiDurationSeconds = Nothing,
                  eiArtworkUrl = Nothing,
                  eiScheduleTemplateId = Just templateId,
                  eiScheduledAt = Just (read (show targetDate ++ " 10:00:00 UTC")),
                  eiCreatedBy = userId
                }
        _ <- unwrapInsert (Episodes.insertEpisode episodeInsert)

        missing <- TRX.statement () (ShowSchedule.getHostsMissingEpisodesOnDay 5)
        TRX.condemn
        pure (show1, missing)

      assert $ do
        (show1, missing) <- assertRight result
        let matchingRows = filter (\h -> ShowSchedule.hmeShowTitle h == Shows.siTitle show1) missing
        Hedgehog.assert (not $ null matchingRows)


-- | Show with two hosts returns one row per host.
prop_multipleHostsMultipleRows :: TestDBConfig -> PropertyT IO ()
prop_multipleHostsMultipleRows cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    userWithMetadata1 <- forAllT userWithMetadataInsertGen
    userWithMetadata2 <- forAllT userWithMetadataInsertGen

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      let targetDate = addDays 5 today
          targetDow = toDayOfWeek $ let (_, _, d) = toWeekDate targetDate in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId1 <- insertTestUser userWithMetadata1
        userId2 <- insertTestUser userWithMetadata2
        let show1 = showInsert {Shows.siStatus = Shows.Active}
        showId <- unwrapInsert (Shows.insertShow show1)
        addTestShowHost showId userId1
        addTestShowHost showId userId2

        let scheduleInsert = ShowSchedule.ScheduleTemplateInsert showId (Just targetDow) (Just allWeeksOfMonth) (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) "UTC" False
        templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleInsert)
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-7) today) Nothing))

        missing <- TRX.statement () (ShowSchedule.getHostsMissingEpisodesOnDay 5)
        TRX.condemn
        pure (show1, userWithMetadata1, userWithMetadata2, missing)

      assert $ do
        (show1, meta1, meta2, missing) <- assertRight result
        let matchingRows = filter (\h -> ShowSchedule.hmeShowTitle h == Shows.siTitle show1) missing
        length matchingRows === 2
        let emails = map ShowSchedule.hmeHostEmail matchingRows
        Hedgehog.assert (display (UserMetadata.uwmiEmail meta1) `elem` emails)
        Hedgehog.assert (display (UserMetadata.uwmiEmail meta2) `elem` emails)


-- | Schedule with expired validity (effective_until in the past) does not appear.
prop_hostExpiredValidityNotReturned :: TestDBConfig -> PropertyT IO ()
prop_hostExpiredValidityNotReturned cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    userWithMetadata <- forAllT userWithMetadataInsertGen

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      let targetDate = addDays 5 today
          targetDow = toDayOfWeek $ let (_, _, d) = toWeekDate targetDate in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        let show1 = showInsert {Shows.siStatus = Shows.Active}
        showId <- unwrapInsert (Shows.insertShow show1)
        addTestShowHost showId userId

        let scheduleInsert = ShowSchedule.ScheduleTemplateInsert showId (Just targetDow) (Just allWeeksOfMonth) (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) "UTC" False
        templateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate scheduleInsert)
        -- Validity expired yesterday
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-30) today) (Just (addDays (-1) today))))

        missing <- TRX.statement () (ShowSchedule.getHostsMissingEpisodesOnDay 5)
        TRX.condemn
        pure (show1, missing)

      assert $ do
        (show1, missing) <- assertRight result
        let matchingRows = filter (\h -> ShowSchedule.hmeShowTitle h == Shows.siTitle show1) missing
        length matchingRows === 0
