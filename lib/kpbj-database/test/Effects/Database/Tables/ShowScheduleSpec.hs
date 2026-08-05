module Effects.Database.Tables.ShowScheduleSpec where

--------------------------------------------------------------------------------

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)
import Data.Time (DayOfWeek, addDays, diffDays, diffUTCTime, getCurrentTime, utctDay)
import Data.Time qualified as Time
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.LocalTime (TimeOfDay (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as UUT
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, annotate, failure, (===))
import Hedgehog qualified
import Hedgehog.Internal.Property (forAllT)
import OrphanInstances.DayOfWeek (toDayOfWeek)
import Test.Database.Helpers (insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, (<==))
import Test.Gen.Tables.ShowSchedule (allWeeksOfMonth, genDayOfWeek, genFutureDay, genTimeRange, genTimezone, genWeeksOfMonth, weekOfMonth)
import Test.Gen.Tables.Shows (showInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.ShowSchedule" $ do
      -- Template CRUD tests
      runs 20 . it "schema validation: insert and select schedule template" $ hedgehog . prop_insertSelectTemplate
      runs 20 . it "query validation: getScheduleTemplatesForShow" $ hedgehog . prop_getTemplatesForShow
      runs 10 . it "templateBelongsToShow: only its owning show" $ hedgehog . prop_templateBelongsToShow

      -- Active schedule queries
      runs 20 . it "query validation: getActiveScheduleTemplatesForShow" $ hedgehog . prop_getActiveTemplates

      -- Upcoming dates calculations
      runs 30 . it "upcoming dates are always in the future" $ hedgehog . prop_upcomingDatesInFuture
      runs 30 . it "weekly schedules repeat correctly" $ hedgehog . prop_weeklyScheduleRepeats
      runs 20 . it "upcoming dates match correct day of week" $ hedgehog . prop_upcomingDatesDayOfWeek
      runs 20 . it "respects validity periods" $ hedgehog . prop_respectsValidityPeriods
      runs 10 . it "handles year boundaries correctly" $ hedgehog . prop_handlesYearBoundaries

      -- Unscheduled dates
      runs 20 . it "getUpcomingUnscheduledShowDates excludes scheduled episodes" $ hedgehog . prop_unscheduledExcludesScheduled


      -- Timezone validation
      runs 20 . it "timezone is stored and retrieved correctly" $ hedgehog . prop_timezoneStorage

      -- weeks_of_month constraint
      runs 10 . it "weeks_of_month rejects an empty array" $ hedgehog . prop_rejectsEmptyWeeksOfMonth
      runs 10 . it "weeks_of_month accepts any subset of 1 to 5" $ hedgehog . prop_acceptsWeeksInRange
      runs 10 . it "upcoming dates use correct timezone for timestamp conversion" $ hedgehog . prop_timezoneConversion

      -- Conflict detection
      describe "checkTimeSlotConflict" $ do
        runs 10 . it "detects overlapping time slots" $ hedgehog . prop_checkTimeSlotConflict
        runs 10 . it "range-overlaps validity windows against the effective date" $ hedgehog . prop_checkTimeSlotConflictValidityWindows
        runs 10 . it "sees a window that crosses midnight from the next day" $ hedgehog . prop_checkTimeSlotConflictOvernight
        runs 10 . it "sees a replay that crosses midnight from the next day" $ hedgehog . prop_checkTimeSlotConflictReplayAcrossMidnight

      -- Validity management
      describe "Validity" $ do
        runs 10 . it "getActiveValidityPeriodsForTemplate: returns active validity" $ hedgehog . prop_getActiveValidityPeriodsForTemplate
        runs 10 . it "endValidity: sets effective_until" $ hedgehog . prop_endValidity

      -- Scheduled shows
      describe "getScheduledShowsForDate" $ do
        runs 10 . it "returns shows scheduled for a given date" $ hedgehog . prop_getScheduledShowsForDate

--------------------------------------------------------------------------------
-- Template CRUD Tests

prop_insertSelectTemplate :: TestDBConfig -> PropertyT IO ()
prop_insertSelectTemplate cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT genDayOfWeek
    weeksOfMonth <- forAllT genWeeksOfMonth -- Generate to test various week patterns
    timezone <- forAllT genTimezone

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)

        let scheduleInsert =
              UUT.ScheduleTemplateInsert
                { stiShowId = showId,
                  stiDayOfWeek = dayOfWeek,
                  stiWeeksOfMonth = weeksOfMonth,
                  stiStartTime = startTime,
                  stiEndTime = endTime,
                  stiTimezone = timezone,
                  stiReplayStartTime = Nothing
                }

        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)
        selected <- TRX.statement () (UUT.getScheduleTemplateById templateId)
        TRX.condemn
        pure (templateId, scheduleInsert, selected)

      assert $ do
        (templateId, scheduleInsert, mSelected) <- assertRight result
        selectedTemplate <- assertJust mSelected
        scheduleInsert.stiShowId === selectedTemplate.stShowId
        scheduleInsert.stiDayOfWeek === selectedTemplate.stDayOfWeek
        scheduleInsert.stiWeeksOfMonth === selectedTemplate.stWeeksOfMonth
        scheduleInsert.stiStartTime === selectedTemplate.stStartTime
        scheduleInsert.stiEndTime === selectedTemplate.stEndTime
        scheduleInsert.stiTimezone === selectedTemplate.stTimezone
        scheduleInsert.stiReplayStartTime === selectedTemplate.stReplayStartTime
        templateId === selectedTemplate.stId

prop_getTemplatesForShow :: TestDBConfig -> PropertyT IO ()
prop_getTemplatesForShow cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (start1, end1) <- forAllT genTimeRange
    (start2, end2) <- forAllT genTimeRange
    dow1 <- forAllT genDayOfWeek
    dow2 <- forAllT genDayOfWeek
    tz1 <- forAllT genTimezone
    tz2 <- forAllT genTimezone

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)

        let schedule1 = UUT.ScheduleTemplateInsert showId dow1 allWeeksOfMonth start1 end1 tz1 Nothing
            schedule2 = UUT.ScheduleTemplateInsert showId dow2 allWeeksOfMonth start2 end2 tz2 Nothing

        _ <- TRX.statement () (UUT.insertScheduleTemplate schedule1)
        _ <- TRX.statement () (UUT.insertScheduleTemplate schedule2)
        templates <- TRX.statement () (UUT.getScheduleTemplatesForShow showId)
        TRX.condemn
        pure (showId, templates)

      assert $ do
        (showId, templates) <- assertRight result
        Hedgehog.assert (length templates >= 2)
        forM_ templates $ \template -> do
          template.stShowId === showId

-- | templateBelongsToShow: a template is owned only by the show it was created for.
--
-- Both episode writers gate on this before touching
-- @episodes.schedule_template_id@, because the form field carries a raw template
-- id that a crafted POST can point at any show. A template id that does not exist
-- is not owned either.
prop_templateBelongsToShow :: TestDBConfig -> PropertyT IO ()
prop_templateBelongsToShow cfg = do
  arrange (bracketConn cfg) $ do
    showInsert1 <- forAllT showInsertGen
    showInsert2 <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dow <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let show1 = showInsert1 {Shows.siSlug = Shows.siSlug showInsert1 <> "owner1"}
            show2 = showInsert2 {Shows.siSlug = Shows.siSlug showInsert2 <> "owner2"}
        showId1 <- unwrapInsert (Shows.insertShow show1)
        showId2 <- unwrapInsert (Shows.insertShow show2)

        let schedule = UUT.ScheduleTemplateInsert showId1 dow allWeeksOfMonth startTime endTime timezone Nothing
        templateId <- TRX.statement () (UUT.insertScheduleTemplate schedule)

        owned <- TRX.statement () (UUT.templateBelongsToShow templateId showId1)
        borrowed <- TRX.statement () (UUT.templateBelongsToShow templateId showId2)
        missing <- TRX.statement () (UUT.templateBelongsToShow (UUT.TemplateId 0) showId1)

        TRX.condemn
        pure (owned, borrowed, missing)

      assert $ do
        (owned, borrowed, missing) <- assertRight result
        owned === True
        borrowed === False
        missing === False

--------------------------------------------------------------------------------
-- Active Schedule Tests

prop_getActiveTemplates :: TestDBConfig -> PropertyT IO ()
prop_getActiveTemplates cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek allWeeksOfMonth startTime endTime timezone Nothing
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        _ <- unwrapInsert (UUT.insertValidity validityInsert)
        activeTemplates <- TRX.statement () (UUT.getActiveScheduleTemplatesForShow showId)
        pure (showId, templateId, activeTemplates)

      assert $ do
        (showId, templateId, activeTemplates) <- assertRight result
        Hedgehog.assert (not $ null activeTemplates)
        let matchingTemplates = filter (\(UUT.ScheduleTemplate {stId = tid}) -> tid == templateId) activeTemplates
        Hedgehog.assert (not $ null matchingTemplates)
        forM_ activeTemplates $ \template -> do
          template.stShowId === showId

--------------------------------------------------------------------------------
-- Upcoming Dates Tests

prop_upcomingDatesInFuture :: TestDBConfig -> PropertyT IO ()
prop_upcomingDatesInFuture cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek allWeeksOfMonth startTime endTime timezone Nothing
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        _ <- unwrapInsert (UUT.insertValidity validityInsert)
        upcomingDates <- TRX.statement () (UUT.getUpcomingShowDates showId today 10)
        pure (today, upcomingDates)

      assert $ do
        (today', upcomingDates) <- assertRight result
        forM_ upcomingDates $ \date -> do
          Hedgehog.assert (date.usdShowDate >= today')

prop_weeklyScheduleRepeats :: TestDBConfig -> PropertyT IO ()
prop_weeklyScheduleRepeats cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dow <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)

        -- Weekly schedule: airs every occurrence of the weekday
        -- Using [1,2,3,4,5] for weeksOfMonth is functionally equivalent to NULL
        -- (both mean "every week"), but we use the explicit form in tests
        let scheduleInsert =
              UUT.ScheduleTemplateInsert
                { stiShowId = showId,
                  stiDayOfWeek = dow,
                  stiWeeksOfMonth = allWeeksOfMonth,
                  stiStartTime = startTime,
                  stiEndTime = endTime,
                  stiTimezone = timezone,
                  stiReplayStartTime = Nothing
                }

        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        -- Add active validity with fixed dates
        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        _ <- unwrapInsert (UUT.insertValidity validityInsert)

        TRX.statement () (UUT.getUpcomingShowDates showId today 4)

      assert $ do
        upcomingDates <- assertRight result
        case upcomingDates of
          (d1 : d2 : d3 : d4 : _) -> do
            -- Check 7-day intervals between consecutive dates
            let diff1 = diffDays d2.usdShowDate d1.usdShowDate
            let diff2 = diffDays d3.usdShowDate d2.usdShowDate
            let diff3 = diffDays d4.usdShowDate d3.usdShowDate
            diff1 === 7
            diff2 === 7
            diff3 === 7
          _ -> do
            annotate $ "Expected at least 4 dates but found " <> show (length upcomingDates)
            failure

prop_upcomingDatesDayOfWeek :: TestDBConfig -> PropertyT IO ()
prop_upcomingDatesDayOfWeek cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek allWeeksOfMonth startTime endTime timezone Nothing
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        _ <- unwrapInsert (UUT.insertValidity validityInsert)
        upcomingDates <- TRX.statement () (UUT.getUpcomingShowDates showId today 5)
        pure (dayOfWeek, upcomingDates)

      assert $ do
        (expectedDow, upcomingDates) <- assertRight result
        forM_ upcomingDates $ \date -> do
          -- Extract day of week from date
          let (_, _, actualDow) = toDayOfWeek <$> toWeekDate (UUT.usdShowDate date)
          actualDow === expectedDow

prop_respectsValidityPeriods :: TestDBConfig -> PropertyT IO ()
prop_respectsValidityPeriods cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek allWeeksOfMonth startTime endTime timezone Nothing
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        -- Add validity that ends soon
        let effectiveFrom = addDays (-30) today
            effectiveUntil = Just $ addDays 7 today -- Ends in 7 days
            validityInsert = UUT.ValidityInsert templateId effectiveFrom effectiveUntil
        _ <- unwrapInsert (UUT.insertValidity validityInsert)

        upcomingDates <- TRX.statement () (UUT.getUpcomingShowDates showId today 20)
        pure (effectiveUntil, upcomingDates)

      assert $ do
        (effectiveUntil, upcomingDates) <- assertRight result
        case effectiveUntil of
          Just untilDay -> do
            -- All dates should be before the effective_until date
            forM_ upcomingDates $ \date -> do
              Hedgehog.assert (date.usdShowDate < untilDay)
          Nothing -> pure ()

prop_handlesYearBoundaries :: TestDBConfig -> PropertyT IO ()
prop_handlesYearBoundaries cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      -- Use a fixed reference date (Dec 20, 2026) to make the test deterministic.
      -- This ensures we always get December dates regardless of when the test runs.
      let referenceDate = fromGregorian 2026 12 20
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek allWeeksOfMonth startTime endTime timezone Nothing
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        -- Set validity to start before our reference date
        let effectiveFrom = fromGregorian 2026 12 15
            validityInsert = UUT.ValidityInsert templateId effectiveFrom Nothing
        _ <- unwrapInsert (UUT.insertValidity validityInsert)

        TRX.statement () (UUT.getUpcomingShowDates showId referenceDate 10)

      assert $ do
        upcomingDates <- assertRight result
        -- Should get dates spanning year boundary (Dec 2026 -> Jan 2027)
        case upcomingDates of
          (d1 : rest) | not (null rest) -> do
            let (year1, _, _) = toGregorian (UUT.usdShowDate d1)
            let years = map (\d -> let (y, _, _) = toGregorian d.usdShowDate in y) rest
            let crossesBoundary = any (> year1) years
            Hedgehog.assert crossesBoundary
          _ -> do
            annotate $ "Expected at least 2 dates but found " <> show (length upcomingDates)
            failure

prop_unscheduledExcludesScheduled :: TestDBConfig -> PropertyT IO ()
prop_unscheduledExcludesScheduled cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek allWeeksOfMonth startTime endTime timezone Nothing
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        -- Add active validity with fixed dates
        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        _ <- unwrapInsert (UUT.insertValidity validityInsert)

        -- Get all upcoming dates
        allDates <- TRX.statement () (UUT.getUpcomingShowDates showId today 10)

        -- Get unscheduled dates (should be same as all dates since no episodes exist)
        unscheduledDates <- TRX.statement () (UUT.getUpcomingUnscheduledShowDates showId 10)

        pure (allDates, unscheduledDates)

      assert $ do
        (allDates, unscheduledDates) <- assertRight result
        -- Since no episodes exist, unscheduled should equal all dates
        length unscheduledDates === length allDates

--------------------------------------------------------------------------------
-- Timezone Validation Tests

prop_timezoneStorage :: TestDBConfig -> PropertyT IO ()
prop_timezoneStorage cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)

        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek allWeeksOfMonth startTime endTime timezone Nothing
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)
        selected <- TRX.statement () (UUT.getScheduleTemplateById templateId)
        TRX.condemn
        pure (timezone, selected)

      assert $ do
        (expectedTimezone, mSelected) <- assertRight result
        template <- assertJust mSelected
        -- Verify timezone is stored and retrieved correctly
        template.stTimezone === expectedTimezone

prop_timezoneConversion :: TestDBConfig -> PropertyT IO ()
prop_timezoneConversion cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    dayOfWeek <- forAllT genDayOfWeek
    -- Use a known timezone for predictable testing
    let timezone = "America/Los_Angeles" -- PST/PDT
        startTime = TimeOfDay 18 0 0 -- 6:00 PM local
        endTime = TimeOfDay 20 0 0 -- 8:00 PM local
    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek allWeeksOfMonth startTime endTime timezone Nothing
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        _ <- unwrapInsert (UUT.insertValidity validityInsert)

        TRX.statement () (UUT.getUpcomingShowDates showId today 1)

      assert $ do
        upcomingDates <- assertRight result
        case upcomingDates of
          (date : _) -> do
            -- Verify that start_time and end_time maintain correct duration
            -- The exact UTC time depends on whether it's PST (-8) or PDT (-7)
            -- but the duration should always be 2 hours (7200 seconds)
            let duration = diffUTCTime date.usdEndTime date.usdStartTime
            duration === 7200 -- 2 hours in seconds
          _ -> do
            annotate $ "Expected at least 1 date but found " <> show (length upcomingDates)
            failure

--------------------------------------------------------------------------------
-- Conflict Detection Tests

-- | checkTimeSlotConflict: detects overlapping time slots between shows.
prop_checkTimeSlotConflict :: TestDBConfig -> PropertyT IO ()
prop_checkTimeSlotConflict cfg = do
  arrange (bracketConn cfg) $ do
    showInsert1 <- forAllT showInsertGen
    showInsert2 <- forAllT showInsertGen
    dow <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        -- Create first show with a schedule 10:00-12:00
        let show1 = showInsert1 {Shows.siStatus = Shows.Active, Shows.siSlug = Shows.siSlug showInsert1 <> "conflict1"}
        showId1 <- unwrapInsert (Shows.insertShow show1)
        let schedule1 = UUT.ScheduleTemplateInsert showId1 dow allWeeksOfMonth (TimeOfDay 10 0 0) (TimeOfDay 12 0 0) timezone Nothing
        templateId1 <- TRX.statement () (UUT.insertScheduleTemplate schedule1)
        _ <- unwrapInsert (UUT.insertValidity (UUT.ValidityInsert templateId1 (addDays (-7) today) Nothing))

        -- Create second show (to check conflict against show1)
        let show2 = showInsert2 {Shows.siStatus = Shows.Active, Shows.siSlug = Shows.siSlug showInsert2 <> "conflict2"}
        showId2 <- unwrapInsert (Shows.insertShow show2)

        -- Check overlapping slot (11:00-13:00 overlaps with 10:00-12:00)
        mConflict <- TRX.statement () (UUT.checkTimeSlotConflict showId2 dow allWeeksOfMonth (TimeOfDay 11 0 0) (TimeOfDay 13 0 0) today)

        -- Check non-overlapping slot (13:00-15:00 doesn't overlap with 10:00-12:00)
        mNoConflict <- TRX.statement () (UUT.checkTimeSlotConflict showId2 dow allWeeksOfMonth (TimeOfDay 13 0 0) (TimeOfDay 15 0 0) today)

        TRX.condemn
        pure (show1, mConflict, mNoConflict)

      assert $ do
        (show1, mConflict, mNoConflict) <- assertRight result
        -- Overlapping slot returns the conflicting show title
        conflictTitle <- assertJust mConflict
        conflictTitle === Shows.siTitle show1
        -- Non-overlapping slot returns Nothing
        assertNothing mNoConflict

-- | checkTimeSlotConflict: validity windows are range-overlapped against the
-- date the proposed slot takes effect.
--
-- The proposed slot is the open-ended window @[fromDate, infinity)@, so this
-- covers the three cases the @fromDate@ parameter exists for:
--
--   * another show's pending (future, open-ended) booking is a conflict today,
--   * a cancelled booking, stored as an empty window, is not a conflict,
--   * a booking whose window closes on the effective date hands the slot off
--     cleanly and is not a conflict, though it still conflicts for a change
--     taking effect while that window is open.
prop_checkTimeSlotConflictValidityWindows :: TestDBConfig -> PropertyT IO ()
prop_checkTimeSlotConflictValidityWindows cfg = do
  arrange (bracketConn cfg) $ do
    pendingInsert <- forAllT showInsertGen
    cancelledInsert <- forAllT showInsertGen
    handoffInsert <- forAllT showInsertGen
    probeInsert <- forAllT showInsertGen
    dow <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      -- The date the handoff show vacates its slot.
      let handoffEnd = addDays 10 today
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let activeShow showInsert suffix =
              showInsert
                { Shows.siStatus = Shows.Active,
                  Shows.siSlug = Shows.siSlug showInsert <> suffix
                }
            template showId start end =
              UUT.ScheduleTemplateInsert showId dow allWeeksOfMonth start end timezone Nothing

        -- (a) Pending booking: 01:00-02:00 held open-ended from 30 days out.
        let pendingShow = activeShow pendingInsert "window-pending"
        pendingShowId <- unwrapInsert (Shows.insertShow pendingShow)
        pendingTemplateId <- TRX.statement () (UUT.insertScheduleTemplate (template pendingShowId (TimeOfDay 1 0 0) (TimeOfDay 2 0 0)))
        _ <- unwrapInsert (UUT.insertValidity (UUT.ValidityInsert pendingTemplateId (addDays 30 today) Nothing))

        -- (b) Cancelled booking: 03:00-04:00 stored as an empty window.
        let cancelledShow = activeShow cancelledInsert "window-cancelled"
        cancelledShowId <- unwrapInsert (Shows.insertShow cancelledShow)
        cancelledTemplateId <- TRX.statement () (UUT.insertScheduleTemplate (template cancelledShowId (TimeOfDay 3 0 0) (TimeOfDay 4 0 0)))
        _ <- unwrapInsert (UUT.insertValidity (UUT.ValidityInsert cancelledTemplateId (addDays 30 today) (Just (addDays 30 today))))

        -- (c) Clean handoff: 05:00-06:00 held until handoffEnd, then vacated.
        let handoffShow = activeShow handoffInsert "window-handoff"
        handoffShowId <- unwrapInsert (Shows.insertShow handoffShow)
        handoffTemplateId <- TRX.statement () (UUT.insertScheduleTemplate (template handoffShowId (TimeOfDay 5 0 0) (TimeOfDay 6 0 0)))
        _ <- unwrapInsert (UUT.insertValidity (UUT.ValidityInsert handoffTemplateId (addDays (-30) today) (Just handoffEnd)))

        -- The show doing the booking, excluded from every check below.
        probeShowId <- unwrapInsert (Shows.insertShow (activeShow probeInsert "window-probe"))

        mPending <- TRX.statement () (UUT.checkTimeSlotConflict probeShowId dow allWeeksOfMonth (TimeOfDay 1 0 0) (TimeOfDay 2 0 0) today)
        mCancelled <- TRX.statement () (UUT.checkTimeSlotConflict probeShowId dow allWeeksOfMonth (TimeOfDay 3 0 0) (TimeOfDay 4 0 0) today)
        mHandoffOnVacate <- TRX.statement () (UUT.checkTimeSlotConflict probeShowId dow allWeeksOfMonth (TimeOfDay 5 0 0) (TimeOfDay 6 0 0) handoffEnd)
        mHandoffToday <- TRX.statement () (UUT.checkTimeSlotConflict probeShowId dow allWeeksOfMonth (TimeOfDay 5 0 0) (TimeOfDay 6 0 0) today)

        TRX.condemn
        pure (pendingShow, handoffShow, mPending, mCancelled, mHandoffOnVacate, mHandoffToday)

      assert $ do
        (pendingShow, handoffShow, mPending, mCancelled, mHandoffOnVacate, mHandoffToday) <- assertRight result
        -- (a) The pending booking is visible to a check made today
        pendingTitle <- assertJust mPending
        pendingTitle === Shows.siTitle pendingShow
        -- (b) The cancelled empty window never airs, so it is not a conflict
        assertNothing mCancelled
        -- (c) The slot is free from the date its holder vacates it
        assertNothing mHandoffOnVacate
        -- ... but is taken for a change that would take effect today
        handoffTitle <- assertJust mHandoffToday
        handoffTitle === Shows.siTitle handoffShow

-- | Move a day of the week forward. Saturday wraps to Sunday.
shiftDow :: Int -> DayOfWeek -> DayOfWeek
shiftDow n d = toEnum (fromEnum d + n)

-- | checkTimeSlotConflict: a window that crosses midnight is visible from the next day.
--
-- The proposed slot recurs on one day of the week, so the check compares against
-- three days: the same day, the day before, and the day after. A window that
-- stops at midnight does not cross, and stays invisible from the next day.
--
-- Each booking below sits on its own day of the week, and no probe reaches past
-- one day, so the bookings do not mask each other.
prop_checkTimeSlotConflictOvernight :: TestDBConfig -> PropertyT IO ()
prop_checkTimeSlotConflictOvernight cfg = do
  arrange (bracketConn cfg) $ do
    lateInsert <- forAllT showInsertGen
    earlyInsert <- forAllT showInsertGen
    midnightInsert <- forAllT showInsertGen
    weekOneInsert <- forAllT showInsertGen
    probeInsert <- forAllT showInsertGen
    dow <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let activeShow showInsert suffix =
              showInsert
                { Shows.siStatus = Shows.Active,
                  Shows.siSlug = Shows.siSlug showInsert <> suffix
                }
            book showInsert suffix day weeks start end = do
              let entry = activeShow showInsert suffix
              showId <- unwrapInsert (Shows.insertShow entry)
              templateId <-
                TRX.statement () $
                  UUT.insertScheduleTemplate (UUT.ScheduleTemplateInsert showId day weeks start end timezone Nothing)
              _ <- unwrapInsert (UUT.insertValidity (UUT.ValidityInsert templateId (addDays (-7) today) Nothing))
              pure (Shows.siTitle entry)

        -- Runs 23:00 to 01:00, so it takes the first hour of the next day.
        lateTitle <- book lateInsert "overnight-late" dow allWeeksOfMonth (TimeOfDay 23 0 0) (TimeOfDay 1 0 0)
        -- Runs 00:00 to 01:00, two days on, and receives a tail from the day before it.
        earlyTitle <- book earlyInsert "overnight-early" (shiftDow 2 dow) allWeeksOfMonth (TimeOfDay 0 0 0) (TimeOfDay 1 0 0)
        -- Stops at midnight, so it takes nothing from the next day.
        _ <- book midnightInsert "overnight-midnight" (shiftDow 3 dow) allWeeksOfMonth (TimeOfDay 22 0 0) (TimeOfDay 0 0 0)
        -- Crosses midnight on the first week of the month only.
        weekOneTitle <- book weekOneInsert "overnight-weekone" (shiftDow 5 dow) [1] (TimeOfDay 23 0 0) (TimeOfDay 1 0 0)

        probeShowId <- unwrapInsert (Shows.insertShow (activeShow probeInsert "overnight-probe"))
        let probe day weeks start end = TRX.statement () (UUT.checkTimeSlotConflict probeShowId day weeks start end today)

        -- (a) The late show's tail takes the first hour of the next day.
        mTailHit <- probe (shiftDow 1 dow) allWeeksOfMonth (TimeOfDay 0 0 0) (TimeOfDay 1 0 0)
        -- (b) The hour after that tail ends is free.
        mAfterTail <- probe (shiftDow 1 dow) allWeeksOfMonth (TimeOfDay 1 0 0) (TimeOfDay 2 0 0)
        -- (c) A proposed slot that crosses midnight reaches the next day's show.
        mProposedTail <- probe (shiftDow 1 dow) allWeeksOfMonth (TimeOfDay 23 0 0) (TimeOfDay 1 0 0)
        -- (d) A show that stops at midnight leaves the next day free.
        mMidnightStop <- probe (shiftDow 4 dow) allWeeksOfMonth (TimeOfDay 0 0 0) (TimeOfDay 1 0 0)
        -- (e) A week-1 date can be the 7th, so its tail can land in week 2.
        mWeekTwo <- probe (shiftDow 6 dow) [2] (TimeOfDay 0 0 0) (TimeOfDay 1 0 0)
        -- (f) A week-1 tail can never land in week 4.
        mWeekFour <- probe (shiftDow 6 dow) [4] (TimeOfDay 0 0 0) (TimeOfDay 1 0 0)

        TRX.condemn
        pure (lateTitle, earlyTitle, weekOneTitle, mTailHit, mAfterTail, mProposedTail, mMidnightStop, mWeekTwo, mWeekFour)

      assert $ do
        (lateTitle, earlyTitle, weekOneTitle, mTailHit, mAfterTail, mProposedTail, mMidnightStop, mWeekTwo, mWeekFour) <- assertRight result
        tailTitle <- assertJust mTailHit
        tailTitle === lateTitle
        assertNothing mAfterTail
        proposedTitle <- assertJust mProposedTail
        proposedTitle === earlyTitle
        assertNothing mMidnightStop
        weekTwoTitle <- assertJust mWeekTwo
        weekTwoTitle === weekOneTitle
        assertNothing mWeekFour

-- | checkTimeSlotConflict: a replay that crosses midnight reaches the next day.
--
-- A replay runs for the same number of minutes as the primary, and it is anchored
-- to the same day of the week. A primary can stop before midnight while its
-- replay crosses. The check has to see the replay on both days.
--
-- The schedule editor pushes toward this. Its replay picker offers only times at
-- or after the primary ends, so a show that ends at 22:00 gets a picker that
-- starts there, and a 2-hour replay from 23:00 crosses midnight.
prop_checkTimeSlotConflictReplayAcrossMidnight :: TestDBConfig -> PropertyT IO ()
prop_checkTimeSlotConflictReplayAcrossMidnight cfg = do
  arrange (bracketConn cfg) $ do
    everyWeekInsert <- forAllT showInsertGen
    weekOneInsert <- forAllT showInsertGen
    probeInsert <- forAllT showInsertGen
    dow <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        let activeShow showInsert suffix =
              showInsert
                { Shows.siStatus = Shows.Active,
                  Shows.siSlug = Shows.siSlug showInsert <> suffix
                }
            -- Primary 20:00 to 22:00, replay 23:00 to 01:00. The primary stops
            -- before midnight. The replay crosses it.
            book showInsert suffix day weeks = do
              let entry = activeShow showInsert suffix
              showId <- unwrapInsert (Shows.insertShow entry)
              templateId <-
                TRX.statement () $
                  UUT.insertScheduleTemplate
                    ( UUT.ScheduleTemplateInsert
                        showId
                        day weeks
                        (TimeOfDay 20 0 0)
                        (TimeOfDay 22 0 0)
                        timezone
                        (Just (TimeOfDay 23 0 0))
                    )
              _ <- unwrapInsert (UUT.insertValidity (UUT.ValidityInsert templateId (addDays (-7) today) Nothing))
              pure (Shows.siTitle entry)

        everyWeekTitle <- book everyWeekInsert "replay-every-week" dow allWeeksOfMonth
        weekOneTitle <- book weekOneInsert "replay-week-one" (shiftDow 2 dow) [1]

        probeShowId <- unwrapInsert (Shows.insertShow (activeShow probeInsert "replay-probe"))
        let probe day weeks start end = TRX.statement () (UUT.checkTimeSlotConflict probeShowId day weeks start end today)

        -- (a) The replay's tail takes the first hour of the next day.
        mTailHit <- probe (shiftDow 1 dow) allWeeksOfMonth (TimeOfDay 0 0 0) (TimeOfDay 1 0 0)
        -- (b) The hour after that tail ends is free.
        mAfterTail <- probe (shiftDow 1 dow) allWeeksOfMonth (TimeOfDay 1 0 0) (TimeOfDay 2 0 0)
        -- (c) The replay's own-day part is taken as well.
        mReplayDay <- probe dow allWeeksOfMonth (TimeOfDay 23 0 0) (TimeOfDay 0 0 0)
        -- (d) The gap between the primary and its replay is free.
        mGap <- probe dow allWeeksOfMonth (TimeOfDay 22 0 0) (TimeOfDay 23 0 0)
        -- (e) A week-1 replay tail can land in week 2.
        mWeekTwo <- probe (shiftDow 3 dow) [2] (TimeOfDay 0 0 0) (TimeOfDay 1 0 0)
        -- (f) A week-1 replay tail can never land in week 4.
        mWeekFour <- probe (shiftDow 3 dow) [4] (TimeOfDay 0 0 0) (TimeOfDay 1 0 0)

        TRX.condemn
        pure (everyWeekTitle, weekOneTitle, mTailHit, mAfterTail, mReplayDay, mGap, mWeekTwo, mWeekFour)

      assert $ do
        (everyWeekTitle, weekOneTitle, mTailHit, mAfterTail, mReplayDay, mGap, mWeekTwo, mWeekFour) <- assertRight result
        tailTitle <- assertJust mTailHit
        tailTitle === everyWeekTitle
        assertNothing mAfterTail
        replayDayTitle <- assertJust mReplayDay
        replayDayTitle === everyWeekTitle
        assertNothing mGap
        weekTwoTitle <- assertJust mWeekTwo
        weekTwoTitle === weekOneTitle
        assertNothing mWeekFour

--------------------------------------------------------------------------------
-- Validity Management Tests

-- | getActiveValidityPeriodsForTemplate: returns currently active validity.
prop_getActiveValidityPeriodsForTemplate :: TestDBConfig -> PropertyT IO ()
prop_getActiveValidityPeriodsForTemplate cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek allWeeksOfMonth startTime endTime timezone Nothing
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        -- Active validity (started 7 days ago, no end)
        validityId1 <- unwrapInsert (UUT.insertValidity (UUT.ValidityInsert templateId (addDays (-7) today) Nothing))

        -- Expired validity, ending where the active one begins. A schedule edit
        -- closes the outgoing window on the replacement's start date, so the two are
        -- adjacent; one_active_slot_per_show rejects any pair that overlaps.
        _ <- unwrapInsert (UUT.insertValidity (UUT.ValidityInsert templateId (addDays (-30) today) (Just (addDays (-7) today))))

        activeValidities <- TRX.statement () (UUT.getActiveValidityPeriodsForTemplate templateId)
        TRX.condemn
        pure (validityId1, activeValidities)

      assert $ do
        (activeValidityId, activeValidities) <- assertRight result
        -- Only the active validity should be returned
        Hedgehog.assert (not $ null activeValidities)
        let activeIds = map UUT.stvId activeValidities
        elem activeValidityId activeIds === True

-- | endValidity: sets effective_until on a validity period.
prop_endValidity :: TestDBConfig -> PropertyT IO ()
prop_endValidity cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek allWeeksOfMonth startTime endTime timezone Nothing
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        -- Create active validity
        validityId <- unwrapInsert (UUT.insertValidity (UUT.ValidityInsert templateId (addDays (-7) today) Nothing))

        -- Verify active before ending
        activeBefore <- TRX.statement () (UUT.getActiveScheduleTemplatesForShow showId)

        -- End the validity
        endResult <- TRX.statement () (UUT.endValidity validityId today)

        -- Verify no longer active
        activeAfter <- TRX.statement () (UUT.getActiveScheduleTemplatesForShow showId)

        TRX.condemn
        pure (validityId, activeBefore, endResult, activeAfter)

      assert $ do
        (validityId, activeBefore, endResult, activeAfter) <- assertRight result
        -- Was active before
        Hedgehog.assert (not $ null activeBefore)
        -- End returned the validity ID
        endedId <- assertJust endResult
        endedId === validityId
        -- No longer active after ending
        length activeAfter === 0

--------------------------------------------------------------------------------
-- Scheduled Shows Tests

-- | getScheduledShowsForDate: returns shows with host info for a specific date.
prop_getScheduledShowsForDate :: TestDBConfig -> PropertyT IO ()
prop_getScheduledShowsForDate cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      -- Find the next occurrence of the target day (use today's DOW)
      let targetDay = today
      let targetDow = toDayOfWeek $ let (_, _, d) = toWeekDate today in d
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let show1 = showInsert {Shows.siStatus = Shows.Active, Shows.siSlug = Shows.siSlug showInsert <> "sched"}
        showId <- unwrapInsert (Shows.insertShow show1)

        -- Add host so show appears in results
        TRX.statement () $ ShowHost.insertShowHost $ ShowHost.Insert showId userId ShowHost.Host

        -- Create a recurring schedule for today's day of week
        let scheduleInsert = UUT.ScheduleTemplateInsert showId targetDow allWeeksOfMonth (TimeOfDay 14 0 0) (TimeOfDay 16 0 0) timezone Nothing
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        -- Add active validity
        _ <- unwrapInsert (UUT.insertValidity (UUT.ValidityInsert templateId (addDays (-7) today) Nothing))

        scheduled <- TRX.statement () (UUT.getScheduledShowsForDate targetDay)
        TRX.condemn
        pure (show1, scheduled)

      assert $ do
        (show1, scheduled) <- assertRight result
        -- Should find at least our show
        Hedgehog.assert (not $ null scheduled)
        let matchingShows = filter (\s -> UUT.sswdShowTitle s == Shows.siTitle show1) scheduled
        Hedgehog.assert (not $ null matchingShows)

-- | The weeks_of_month CHECK rejects an empty array.
--
-- It used to accept one. The constraint read
-- @array_length(weeks_of_month, 1) > 0@, and array_length of an empty array is
-- NULL rather than 0, so the whole expression was NULL and Postgres accepts a
-- CHECK that evaluates to NULL. Migration 20260805061553 swapped it for
-- cardinality, which returns 0.
--
-- An empty array describes a slot that never airs, because recurrence_airs_on
-- tests the week against ANY(weeks_of_month), and one that never conflicts,
-- because both conflict checks test week membership the same way.
prop_rejectsEmptyWeeksOfMonth :: TestDBConfig -> PropertyT IO ()
prop_rejectsEmptyWeeksOfMonth cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    dow <- forAllT genDayOfWeek
    (startTime, endTime) <- forAllT genTimeRange
    timezone <- forAllT genTimezone

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dow [] startTime endTime timezone Nothing
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)
        TRX.condemn
        pure templateId

      assert $ do
        result <== isLeft
        pure ()

-- | The same CHECK still accepts every week from 1 to 5, and still rejects a
-- week outside that range.
prop_acceptsWeeksInRange :: TestDBConfig -> PropertyT IO ()
prop_acceptsWeeksInRange cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    dow <- forAllT genDayOfWeek
    weeks <- forAllT genWeeksOfMonth
    (startTime, endTime) <- forAllT genTimeRange
    timezone <- forAllT genTimezone

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- unwrapInsert (Shows.insertShow showInsert)
        let mkInsert ws = UUT.ScheduleTemplateInsert showId dow ws startTime endTime timezone Nothing
        valid <- TRX.statement () (UUT.insertScheduleTemplate (mkInsert weeks))
        TRX.condemn
        pure valid

      assert $ do
        _ <- assertRight result
        pure ()
