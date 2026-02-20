module API.Schedule.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Schedule.Get.Handler (ScheduleViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Either (isRight)
import Domain.Types.WeekOffset (WeekOffset (..))
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Schedule.Get.Handler" $ do
      describe "action" $ do
        it "succeeds on empty DB" test_returnsRight
        it "empty DB returns empty schedule and 7 week days" test_emptySchedule
        it "always returns exactly 7 week days" test_weekDaysCount
        it "current week offset has Just dayOfWeek and Just timeOfDay" test_currentWeekHasDayAndTime
        it "future week offset has Nothing for dayOfWeek and timeOfDay" test_futureWeekHasNoDayOrTime

--------------------------------------------------------------------------------

-- | Action succeeds (returns Right) on an empty database.
test_returnsRight :: TestDBConfig -> IO ()
test_returnsRight cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ action Nothing
    liftIO $ isRight result `shouldBe` True

-- | Empty database returns an empty schedule with exactly 7 week days.
test_emptySchedule :: TestDBConfig -> IO ()
test_emptySchedule cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ action Nothing
    liftIO $ case result of
      Left err ->
        expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        svdScheduledShows vd `shouldBe` []
        length (svdWeekDays vd) `shouldBe` 7

-- | Regardless of week offset, exactly 7 days are always returned.
test_weekDaysCount :: TestDBConfig -> IO ()
test_weekDaysCount cfg =
  bracketAppM cfg $ do
    result0 <- runExceptT $ action Nothing
    result1 <- runExceptT $ action (Just (WeekOffset 1))
    result2 <- runExceptT $ action (Just (WeekOffset 2))
    result52 <- runExceptT $ action (Just (WeekOffset 52))
    liftIO $ do
      case result0 of
        Left err -> expectationFailure $ "offset 0 failed: " <> show err
        Right vd -> length (svdWeekDays vd) `shouldBe` 7
      case result1 of
        Left err -> expectationFailure $ "offset 1 failed: " <> show err
        Right vd -> length (svdWeekDays vd) `shouldBe` 7
      case result2 of
        Left err -> expectationFailure $ "offset 2 failed: " <> show err
        Right vd -> length (svdWeekDays vd) `shouldBe` 7
      case result52 of
        Left err -> expectationFailure $ "offset 52 failed: " <> show err
        Right vd -> length (svdWeekDays vd) `shouldBe` 7

-- | When viewing the current week (offset Nothing or 0), currentDayOfWeek
-- and currentTimeOfDay are both Just.
test_currentWeekHasDayAndTime :: TestDBConfig -> IO ()
test_currentWeekHasDayAndTime cfg =
  bracketAppM cfg $ do
    resultNothing <- runExceptT $ action Nothing
    resultZero <- runExceptT $ action (Just (WeekOffset 0))
    liftIO $ do
      case resultNothing of
        Left err ->
          expectationFailure $ "Expected Right for offset Nothing but got: " <> show err
        Right vd -> do
          svdCurrentDayOfWeek vd `shouldSatisfy` (/= Nothing)
          svdCurrentTimeOfDay vd `shouldSatisfy` (/= Nothing)
      case resultZero of
        Left err ->
          expectationFailure $ "Expected Right for offset 0 but got: " <> show err
        Right vd -> do
          svdCurrentDayOfWeek vd `shouldSatisfy` (/= Nothing)
          svdCurrentTimeOfDay vd `shouldSatisfy` (/= Nothing)

-- | When viewing a future week (offset > 0), currentDayOfWeek and
-- currentTimeOfDay are both Nothing.
test_futureWeekHasNoDayOrTime :: TestDBConfig -> IO ()
test_futureWeekHasNoDayOrTime cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ action (Just (WeekOffset 1))
    liftIO $ case result of
      Left err ->
        expectationFailure $ "Expected Right but got: " <> show err
      Right vd -> do
        svdCurrentDayOfWeek vd `shouldBe` Nothing
        svdCurrentTimeOfDay vd `shouldBe` Nothing
