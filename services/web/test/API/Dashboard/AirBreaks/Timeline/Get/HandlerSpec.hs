{-# LANGUAGE PackageImports #-}

module API.Dashboard.AirBreaks.Timeline.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Timeline.Get.Handler (action)
import API.Dashboard.AirBreaks.Timeline.Get.Templates.Page (DayKind (..), TimelineView (..), template)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text.Lazy qualified as Lazy
import Data.Time (LocalTime (..), TimeOfDay (..), UTCTime, fromGregorian)
import Data.Time.Calendar (DayOfWeek (..))
import Domain.DayTimeline (SegmentView (..))
import Domain.Timeline (Segment (..))
import Domain.Types.Slug (Slug (..))
import Domain.Types.Timezone (pacificToUtc)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakPlans qualified as BreakPlans
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Transaction.Sessions qualified as TRX
import Lucid qualified
import Test.Database.Helpers (insertTestShowWithSchedule, unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import "kpbj-web" App.Monad (AppM)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.AirBreaks.Timeline.Get.Handler.action" $ do
      it "forecasts a future day without storing it" futureDayStoresNothing
      it "builds today's plan when viewed" todayBuildsPlan
      it "shows only the scheduled segments" showsOnlyScheduled

--------------------------------------------------------------------------------

-- | Noon Pacific on Monday, January 6, 2025.
noon :: UTCTime
noon = pacificToUtc (LocalTime (fromGregorian 2025 1 6) (TimeOfDay 12 0 0))

futureDayStoresNothing :: TestDBConfig -> IO ()
futureDayStoresNothing cfg = bracketAppM cfg $ do
  let tomorrow = fromGregorian 2025 1 7
  view <- runExceptT (action noon tomorrow)
  -- True means this call created the day row, so the view did not.
  created <- execQuery (BreakPlans.insertPlanDay tomorrow)
  liftIO $ do
    case view of
      Right v -> tvKind v `shouldBe` FutureDay
      Left err -> expectationFailure ("Expected a view, got " <> show err)
    created `shouldBe` Right True

todayBuildsPlan :: TestDBConfig -> IO ()
todayBuildsPlan cfg = bracketAppM cfg $ do
  let today = fromGregorian 2025 1 6
  _ <- runExceptT (action noon today)
  created <- execQuery (BreakPlans.insertPlanDay today)
  liftIO $ created `shouldBe` Right False

-- | A show on Mondays from 11:00 to 13:00.
seedNoonSlot :: AppM ()
seedNoonSlot = do
  let showInsert = Shows.Insert "Noon Show" (Slug "noon-show") Nothing Nothing Shows.Active
      scheduleTemplate =
        ShowSchedule.ScheduleTemplateInsert
          { stiShowId = Shows.Id 0,
            stiDayOfWeek = Monday,
            stiWeeksOfMonth = [1, 2, 3, 4, 5],
            stiStartTime = TimeOfDay 11 0 0,
            stiEndTime = TimeOfDay 13 0 0,
            stiTimezone = "America/Los_Angeles",
            stiReplayStartTime = Nothing
          }
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (_, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate
    unwrapInsert $
      ShowSchedule.insertValidity $
        ShowSchedule.ValidityInsert templateId (fromGregorian 2024 12 1) Nothing
  case result of
    Left err -> error ("Setup failed: " <> show err)
    Right _ -> pure ()

-- | The unscheduled hours are left out. The slot has no break inside it, so
-- it is one segment.
showsOnlyScheduled :: TestDBConfig -> IO ()
showsOnlyScheduled cfg = bracketAppM cfg $ do
  seedNoonSlot
  view <- runExceptT (action noon (fromGregorian 2025 1 6))
  liftIO $ case view of
    Left err -> expectationFailure ("Expected a view, got " <> show err)
    Right v -> do
      let at hour = pacificToUtc (LocalTime (fromGregorian 2025 1 6) (TimeOfDay hour 0 0))
      map (segStart . svSegment) (tvSegments v) `shouldBe` [at 11]
      -- An item airs at noon, so the page marks it.
      Lazy.isInfixOf "id=\"now\"" (Lucid.renderText (template v)) `shouldBe` True
