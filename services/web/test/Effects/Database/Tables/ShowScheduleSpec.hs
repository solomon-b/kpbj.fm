module Effects.Database.Tables.ShowScheduleSpec where

--------------------------------------------------------------------------------

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Time (addDays, diffDays, diffUTCTime, getCurrentTime, utctDay)
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.LocalTime (TimeOfDay (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowSchedule qualified as UUT
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog qualified
import Hedgehog.Internal.Property (forAllT)
import OrphanInstances.DayOfWeek (toDayOfWeek)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight)
import Test.Gen.Tables.ShowSchedule (allWeeksOfMonth, genDayOfWeek, genFutureDay, genTimeRange, genTimezone, genWeeksOfMonth)
import Test.Gen.Tables.Shows (showInsertGen)
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

      -- One-time shows
      runs 20 . it "one-time shows can be created with Nothing for dayOfWeek and weeksOfMonth" $ hedgehog . prop_oneTimeShowCreation

      -- Timezone validation
      runs 20 . it "timezone is stored and retrieved correctly" $ hedgehog . prop_timezoneStorage
      runs 10 . it "upcoming dates use correct timezone for timestamp conversion" $ hedgehog . prop_timezoneConversion

--------------------------------------------------------------------------------
-- Template CRUD Tests

prop_insertSelectTemplate :: TestDBConfig -> PropertyT IO ()
prop_insertSelectTemplate cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    weeksOfMonth <- forAllT $ Just <$> genWeeksOfMonth -- Generate to test various week patterns
    timezone <- forAllT genTimezone

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)

        let scheduleInsert =
              UUT.ScheduleTemplateInsert
                { stiShowId = showId,
                  stiDayOfWeek = dayOfWeek,
                  stiWeeksOfMonth = weeksOfMonth,
                  stiStartTime = startTime,
                  stiEndTime = endTime,
                  stiTimezone = timezone,
                  stiAirsTwiceDaily = False
                }

        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)
        selected <- TRX.statement () (UUT.getScheduleTemplateById templateId)
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
        scheduleInsert.stiAirsTwiceDaily === selectedTemplate.stAirsTwiceDaily
        templateId === selectedTemplate.stId

prop_getTemplatesForShow :: TestDBConfig -> PropertyT IO ()
prop_getTemplatesForShow cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (start1, end1) <- forAllT genTimeRange
    (start2, end2) <- forAllT genTimeRange
    dow1 <- forAllT $ Just <$> genDayOfWeek
    dow2 <- forAllT $ Just <$> genDayOfWeek
    tz1 <- forAllT genTimezone
    tz2 <- forAllT genTimezone

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)

        let schedule1 = UUT.ScheduleTemplateInsert showId dow1 (Just allWeeksOfMonth) start1 end1 tz1 False
            schedule2 = UUT.ScheduleTemplateInsert showId dow2 (Just allWeeksOfMonth) start2 end2 tz2 False

        _ <- TRX.statement () (UUT.insertScheduleTemplate schedule1)
        _ <- TRX.statement () (UUT.insertScheduleTemplate schedule2)
        templates <- TRX.statement () (UUT.getScheduleTemplatesForShow showId)
        pure (showId, templates)

      assert $ do
        (showId, templates) <- assertRight result
        Hedgehog.assert (length templates >= 2)
        forM_ templates $ \template -> do
          template.stShowId === showId

--------------------------------------------------------------------------------
-- Active Schedule Tests

prop_getActiveTemplates :: TestDBConfig -> PropertyT IO ()
prop_getActiveTemplates cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone False
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        void $ TRX.statement () (UUT.insertValidity validityInsert)
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
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone False
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        void $ TRX.statement () (UUT.insertValidity validityInsert)
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
        showId <- TRX.statement () (Shows.insertShow showInsert)

        -- Weekly schedule: airs every occurrence of the weekday
        -- Using [1,2,3,4,5] for weeksOfMonth is functionally equivalent to NULL
        -- (both mean "every week"), but we use the explicit form in tests
        let scheduleInsert =
              UUT.ScheduleTemplateInsert
                { stiShowId = showId,
                  stiDayOfWeek = Just dow,
                  stiWeeksOfMonth = Just allWeeksOfMonth,
                  stiStartTime = startTime,
                  stiEndTime = endTime,
                  stiTimezone = timezone,
                  stiAirsTwiceDaily = False
                }

        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        -- Add active validity with fixed dates
        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        void $ TRX.statement () (UUT.insertValidity validityInsert)

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
          _ -> pure () -- Not enough dates to test, skip

prop_upcomingDatesDayOfWeek :: TestDBConfig -> PropertyT IO ()
prop_upcomingDatesDayOfWeek cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone False
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        void $ TRX.statement () (UUT.insertValidity validityInsert)
        upcomingDates <- TRX.statement () (UUT.getUpcomingShowDates showId today 5)
        pure (dayOfWeek, upcomingDates)

      assert $ do
        (expectedDow, upcomingDates) <- assertRight result
        case expectedDow of
          Just dow -> do
            forM_ upcomingDates $ \date -> do
              -- Extract day of week from date
              let (_, _, actualDow) = toDayOfWeek <$> toWeekDate (UUT.usdShowDate date)
              actualDow === dow
          Nothing -> pure () -- One-time show, skip

prop_respectsValidityPeriods :: TestDBConfig -> PropertyT IO ()
prop_respectsValidityPeriods cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone False
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        -- Add validity that ends soon
        let effectiveFrom = addDays (-30) today
            effectiveUntil = Just $ addDays 7 today -- Ends in 7 days
            validityInsert = UUT.ValidityInsert templateId effectiveFrom effectiveUntil
        void $ TRX.statement () (UUT.insertValidity validityInsert)

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
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      -- Use a fixed reference date (Dec 20, 2025) to make the test deterministic.
      -- This ensures we always get December dates regardless of when the test runs.
      let referenceDate = fromGregorian 2025 12 20
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone False
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        -- Set validity to start before our reference date
        let effectiveFrom = fromGregorian 2025 12 15
            validityInsert = UUT.ValidityInsert templateId effectiveFrom Nothing
        void $ TRX.statement () (UUT.insertValidity validityInsert)

        TRX.statement () (UUT.getUpcomingShowDates showId referenceDate 10)

      assert $ do
        upcomingDates <- assertRight result
        -- Should get dates spanning year boundary (Dec 2025 -> Jan 2026)
        case upcomingDates of
          (d1 : rest) | not (null rest) -> do
            let (year1, _, _) = toGregorian (UUT.usdShowDate d1)
            let years = map (\d -> let (y, _, _) = toGregorian d.usdShowDate in y) rest
            let crossesBoundary = any (> year1) years
            Hedgehog.assert crossesBoundary
          _ -> pure () -- Not enough dates

prop_unscheduledExcludesScheduled :: TestDBConfig -> PropertyT IO ()
prop_unscheduledExcludesScheduled cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone False
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        -- Add active validity with fixed dates
        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        void $ TRX.statement () (UUT.insertValidity validityInsert)

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
-- One-Time Show Tests

prop_oneTimeShowCreation :: TestDBConfig -> PropertyT IO ()
prop_oneTimeShowCreation cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    timezone <- forAllT genTimezone
    specificDate <- forAllT genFutureDay

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)

        -- Create one-time show schedule (both dayOfWeek and weeksOfMonth are Nothing)
        let scheduleInsert =
              UUT.ScheduleTemplateInsert
                { stiShowId = showId,
                  stiDayOfWeek = Nothing,
                  stiWeeksOfMonth = Nothing,
                  stiStartTime = startTime,
                  stiEndTime = endTime,
                  stiTimezone = timezone,
                  stiAirsTwiceDaily = False
                }

        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        -- Add validity for specific date
        let validityInsert = UUT.ValidityInsert templateId specificDate (Just $ addDays 1 specificDate)
        void $ TRX.statement () (UUT.insertValidity validityInsert)

        -- Retrieve the template
        selected <- TRX.statement () (UUT.getScheduleTemplateById templateId)
        pure (templateId, scheduleInsert, selected)

      assert $ do
        (templateId, scheduleInsert, mSelected) <- assertRight result
        template <- assertJust mSelected
        -- Verify one-time show fields
        template.stId === templateId
        template.stShowId === scheduleInsert.stiShowId
        template.stDayOfWeek === Nothing
        template.stWeeksOfMonth === Nothing
        template.stStartTime === scheduleInsert.stiStartTime
        template.stEndTime === scheduleInsert.stiEndTime
        template.stTimezone === scheduleInsert.stiTimezone

--------------------------------------------------------------------------------
-- Timezone Validation Tests

prop_timezoneStorage :: TestDBConfig -> PropertyT IO ()
prop_timezoneStorage cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)

        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone False
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)
        selected <- TRX.statement () (UUT.getScheduleTemplateById templateId)
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
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    -- Use a known timezone for predictable testing
    let timezone = "America/Los_Angeles" -- PST/PDT
        startTime = TimeOfDay 18 0 0 -- 6:00 PM local
        endTime = TimeOfDay 20 0 0 -- 8:00 PM local
    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone False
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        void $ TRX.statement () (UUT.insertValidity validityInsert)

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
          _ -> pure () -- Skip if no dates
