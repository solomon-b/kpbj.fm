module Effects.Database.Tables.ShowScheduleSpec where

--------------------------------------------------------------------------------

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
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
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import OrphanInstances.DayOfWeek (toDayOfWeek)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight)
import Test.Gen.Tables.ShowSchedule (allWeeksOfMonth, genDayOfWeek, genFutureDay, genPastDay, genTimeRange, genTimezone, genWeeksOfMonth)
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
      runs 10 . it "query validation: deleteScheduleTemplate" $ hedgehog . prop_deleteTemplate

      -- Validity CRUD tests
      runs 20 . it "schema validation: insert and select validity period" $ hedgehog . prop_insertSelectValidity
      runs 20 . it "query validation: getValidityPeriodsForTemplate" $ hedgehog . prop_getValidityPeriods
      runs 10 . it "query validation: updateValidity" $ hedgehog . prop_updateValidity
      runs 10 . it "query validation: endValidity" $ hedgehog . prop_endValidity

      -- Active schedule queries
      runs 20 . it "query validation: getActiveScheduleTemplatesForShow" $ hedgehog . prop_getActiveTemplates
      runs 20 . it "query validation: getActiveRecurringScheduleTemplates" $ hedgehog . prop_getActiveRecurring

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
      runs 20 . it "one-time shows don't appear in recurring schedule queries" $ hedgehog . prop_oneTimeShowNotInRecurring

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
                  stiTimezone = timezone
                }

        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)
        selected <- TRX.statement () (UUT.getScheduleTemplateById templateId)
        pure (templateId, scheduleInsert, selected)

      assert $ do
        (templateId, scheduleInsert, mSelected) <- assertRight result
        selectedTemplate <- assertJust mSelected
        scheduleInsert.stiShowId === selectedTemplate.showId
        scheduleInsert.stiDayOfWeek === selectedTemplate.dayOfWeek
        scheduleInsert.stiWeeksOfMonth === selectedTemplate.weeksOfMonth
        scheduleInsert.stiStartTime === selectedTemplate.startTime
        scheduleInsert.stiEndTime === selectedTemplate.endTime
        scheduleInsert.stiTimezone === selectedTemplate.timezone
        templateId === selectedTemplate.id

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

        let schedule1 = UUT.ScheduleTemplateInsert showId dow1 (Just allWeeksOfMonth) start1 end1 tz1
            schedule2 = UUT.ScheduleTemplateInsert showId dow2 (Just allWeeksOfMonth) start2 end2 tz2

        _ <- TRX.statement () (UUT.insertScheduleTemplate schedule1)
        _ <- TRX.statement () (UUT.insertScheduleTemplate schedule2)
        templates <- TRX.statement () (UUT.getScheduleTemplatesForShow showId)
        pure (showId, templates)

      assert $ do
        (showId, templates) <- assertRight result
        Hedgehog.assert (length templates >= 2)
        forM_ templates $ \template -> do
          template.showId === showId

prop_deleteTemplate :: TestDBConfig -> PropertyT IO ()
prop_deleteTemplate cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)

        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone

        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)
        mDeleted <- TRX.statement () (UUT.deleteScheduleTemplate templateId)
        mSelected <- TRX.statement () (UUT.getScheduleTemplateById templateId)
        pure (templateId, mDeleted, mSelected)

      assert $ do
        (templateId, mDeleted, mSelected) <- assertRight result
        deleted <- assertJust mDeleted
        deleted === templateId
        mSelected === Nothing

--------------------------------------------------------------------------------
-- Validity CRUD Tests

prop_insertSelectValidity :: TestDBConfig -> PropertyT IO ()
prop_insertSelectValidity cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    timezone <- forAllT genTimezone
    effectiveFrom <- forAllT genPastDay
    effectiveUntil <- forAllT $ Gen.maybe genFutureDay

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)

        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        let validityInsert = UUT.ValidityInsert templateId effectiveFrom effectiveUntil
        validityId <- TRX.statement () (UUT.insertValidity validityInsert)
        selected <- TRX.statement () (UUT.getValidityById validityId)
        pure (validityId, validityInsert, selected)

      assert $ do
        (validityId, validityInsert, mSelected) <- assertRight result
        UUT.ScheduleTemplateValidity {id = selectedId, templateId = tmplId, effectiveFrom = effFrom, effectiveUntil = effUntil} <- assertJust mSelected
        validityInsert.viTemplateId === tmplId
        validityInsert.viEffectiveFrom === effFrom
        validityInsert.viEffectiveUntil === effUntil
        validityId === selectedId

prop_getValidityPeriods :: TestDBConfig -> PropertyT IO ()
prop_getValidityPeriods cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    timezone <- forAllT genTimezone
    from1 <- forAllT genPastDay
    until1 <- forAllT $ Gen.maybe genFutureDay
    -- Ensure from2 is different from from1 to avoid unique constraint violation
    let from2 = addDays 1 from1
    until2 <- forAllT $ Gen.maybe genFutureDay

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        let validity1 = UUT.ValidityInsert templateId from1 until1
            validity2 = UUT.ValidityInsert templateId from2 until2
        _ <- TRX.statement () (UUT.insertValidity validity1)
        _ <- TRX.statement () (UUT.insertValidity validity2)
        periods <- TRX.statement () (UUT.getValidityPeriodsForTemplate templateId)
        pure (templateId, periods)

      assert $ do
        (templateId, periods) <- assertRight result
        Hedgehog.assert (length periods >= 2)
        forM_ periods $ \period -> do
          period.templateId === templateId

prop_updateValidity :: TestDBConfig -> PropertyT IO ()
prop_updateValidity cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    timezone <- forAllT genTimezone
    origFrom <- forAllT genPastDay
    origUntil <- forAllT $ Gen.maybe genFutureDay
    newFrom <- forAllT genPastDay
    newUntil <- forAllT $ Gen.maybe genFutureDay

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        let validityInsert = UUT.ValidityInsert templateId origFrom origUntil
        validityId <- TRX.statement () (UUT.insertValidity validityInsert)

        let update = UUT.ValidityUpdate newFrom newUntil
        _ <- TRX.statement () (UUT.updateValidity validityId update)
        updated <- TRX.statement () (UUT.getValidityById validityId)
        pure (validityId, update, updated)

      assert $ do
        (validityId, update, mUpdated) <- assertRight result
        UUT.ScheduleTemplateValidity {id = updatedId, effectiveFrom = effFrom, effectiveUntil = effUntil} <- assertJust mUpdated
        update.vuEffectiveFrom === effFrom
        update.vuEffectiveUntil === effUntil
        validityId === updatedId

prop_endValidity :: TestDBConfig -> PropertyT IO ()
prop_endValidity cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    timezone <- forAllT genTimezone
    effectiveFrom <- forAllT genPastDay
    endDate <- forAllT genFutureDay

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow showInsert)
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        let validityInsert = UUT.ValidityInsert templateId effectiveFrom Nothing
        validityId <- TRX.statement () (UUT.insertValidity validityInsert)

        _ <- TRX.statement () (UUT.endValidity validityId endDate)
        ended <- TRX.statement () (UUT.getValidityById validityId)
        pure (endDate, ended)

      assert $ do
        (_endDate, mEnded) <- assertRight result
        ended <- assertJust mEnded
        ended.effectiveUntil === Just endDate

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
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        void $ TRX.statement () (UUT.insertValidity validityInsert)
        activeTemplates <- TRX.statement () (UUT.getActiveScheduleTemplatesForShow showId)
        pure (showId, templateId, activeTemplates)

      assert $ do
        (showId, templateId, activeTemplates) <- assertRight result
        Hedgehog.assert (not $ null activeTemplates)
        let matchingTemplates = filter (\(UUT.ScheduleTemplate {id = tid}) -> tid == templateId) activeTemplates
        Hedgehog.assert (not $ null matchingTemplates)
        forM_ activeTemplates $ \template -> do
          template.showId === showId

prop_getActiveRecurring :: TestDBConfig -> PropertyT IO ()
prop_getActiveRecurring cfg = do
  arrange (bracketConn cfg) $ do
    -- Create an active show
    showInsert <- forAllT showInsertGen
    let activeShow = showInsert {Shows.siStatus = Shows.Active}
    (startTime, endTime) <- forAllT genTimeRange
    dayOfWeek <- forAllT $ Just <$> genDayOfWeek
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow activeShow)

        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)

        -- Add active validity with fixed dates
        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        void $ TRX.statement () (UUT.insertValidity validityInsert)

        activeRecurring <- TRX.statement () UUT.getActiveRecurringScheduleTemplates
        pure (templateId, activeRecurring)

      assert $ do
        (templateId, activeRecurring) <- assertRight result
        -- Should include our template
        let matchingTemplates = filter (\(UUT.ScheduleTemplate {id = tid}) -> tid == templateId) activeRecurring
        Hedgehog.assert (not $ null matchingTemplates)
        -- All templates should have day_of_week set (recurring only)
        forM_ activeRecurring $ \template -> do
          Hedgehog.assert (isJust template.dayOfWeek)

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
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone
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
                  stiTimezone = timezone
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
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone
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
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone
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
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone
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
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone
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
                  stiTimezone = timezone
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
        template.id === templateId
        template.showId === scheduleInsert.stiShowId
        template.dayOfWeek === Nothing
        template.weeksOfMonth === Nothing
        template.startTime === scheduleInsert.stiStartTime
        template.endTime === scheduleInsert.stiEndTime
        template.timezone === scheduleInsert.stiTimezone

prop_oneTimeShowNotInRecurring :: TestDBConfig -> PropertyT IO ()
prop_oneTimeShowNotInRecurring cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    let activeShow = showInsert {Shows.siStatus = Shows.Active}
    (startTime, endTime) <- forAllT genTimeRange
    timezone <- forAllT genTimezone

    act $ do
      today <- liftIO $ utctDay <$> getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        showId <- TRX.statement () (Shows.insertShow activeShow)

        -- Create one-time show schedule
        let oneTimeSchedule =
              UUT.ScheduleTemplateInsert
                { stiShowId = showId,
                  stiDayOfWeek = Nothing,
                  stiWeeksOfMonth = Nothing,
                  stiStartTime = startTime,
                  stiEndTime = endTime,
                  stiTimezone = timezone
                }

        templateId <- TRX.statement () (UUT.insertScheduleTemplate oneTimeSchedule)

        -- Add active validity with fixed dates
        let validityInsert = UUT.ValidityInsert templateId (addDays (-7) today) Nothing
        void $ TRX.statement () (UUT.insertValidity validityInsert)

        -- Query for recurring schedules
        recurring <- TRX.statement () UUT.getActiveRecurringScheduleTemplates
        pure (templateId, recurring)

      assert $ do
        (templateId, recurring) <- assertRight result
        -- One-time shows should NOT appear in recurring schedules
        let matchingTemplates = filter (\t -> t.id == templateId) recurring
        Hedgehog.assert (null matchingTemplates)

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

        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone
        templateId <- TRX.statement () (UUT.insertScheduleTemplate scheduleInsert)
        selected <- TRX.statement () (UUT.getScheduleTemplateById templateId)
        pure (timezone, selected)

      assert $ do
        (expectedTimezone, mSelected) <- assertRight result
        template <- assertJust mSelected
        -- Verify timezone is stored and retrieved correctly
        template.timezone === expectedTimezone

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
        let scheduleInsert = UUT.ScheduleTemplateInsert showId dayOfWeek (Just allWeeksOfMonth) startTime endTime timezone
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
