module API.Dashboard.Underwriting.Plan.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Underwriting.Plan.Get.Handler (action)
import API.Dashboard.Underwriting.Plan.Get.Templates.Page (PlanView (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Time (LocalTime (..), TimeOfDay (..), UTCTime, fromGregorian)
import Domain.Types.Timezone (pacificToUtc)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakPlans qualified as BreakPlans
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Underwriting.Plan.Get.Handler.action" $ do
      it "forecasts a future day without storing it" futureDayStoresNothing
      it "builds today's plan when viewed" todayBuildsPlan

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
      Right (ForecastPlan _) -> pure ()
      other -> expectationFailure ("Expected a forecast, got " <> show other)
    created `shouldBe` Right True

todayBuildsPlan :: TestDBConfig -> IO ()
todayBuildsPlan cfg = bracketAppM cfg $ do
  let today = fromGregorian 2025 1 6
  _ <- runExceptT (action noon today)
  created <- execQuery (BreakPlans.insertPlanDay today)
  liftIO $ created `shouldBe` Right False
