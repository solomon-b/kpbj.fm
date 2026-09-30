module API.Dashboard.BreakItems.SharedSpec where

--------------------------------------------------------------------------------

import API.Dashboard.BreakItems.Form (BreakItemForm (..))
import API.Dashboard.BreakItems.Shared (ParsedBreakItem (..), parseBreakItemForm)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Data.Time (fromGregorian)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.BreakItems.Shared" $
      describe "parseBreakItemForm" $ do
        it "parses an underwriting order" test_parsesUnderwriting
        it "accepts underwriting dates inside a month" test_acceptsMidMonthDates
        it "rejects zero spots per month" test_rejectsZeroSpots
        it "rejects underwriting with no underwriter" test_rejectsNoUnderwriter
        it "parses a PSA on any date with no underwriting fields" test_parsesPsa

--------------------------------------------------------------------------------

-- | A valid underwriting form. Each test changes one field.
underwritingForm :: BreakItemForm
underwritingForm =
  BreakItemForm
    { bifTitle = "Spot",
      bifAudioToken = "",
      bifDurationSeconds = "30",
      bifStartsOn = "2025-02-01",
      bifEndsOn = "",
      bifUnderwriterId = "1",
      bifSpotsPerMonth = "30"
    }

-- | Parse the form and expect a validation error with this message.
expectRejected :: TestDBConfig -> BreakItems.Category -> BreakItemForm -> Text -> IO ()
expectRejected cfg category form message = bracketAppM cfg $ do
  result <- runExceptT (parseBreakItemForm category form)
  liftIO $ case result of
    Left (ValidationError msg) -> msg `shouldBe` message
    other -> expectationFailure $ "Expected a validation error, got " <> show other

test_parsesUnderwriting :: TestDBConfig -> IO ()
test_parsesUnderwriting cfg = bracketAppM cfg $ do
  result <- runExceptT (parseBreakItemForm BreakItems.Underwriting underwritingForm)
  liftIO $ case result of
    Right parsed -> do
      parsed.pbiStartsOn `shouldBe` fromGregorian 2025 2 1
      parsed.pbiEndsOn `shouldBe` Nothing
      parsed.pbiUnderwriterId `shouldBe` Just (Underwriters.Id 1)
      parsed.pbiSpotsPerMonth `shouldBe` Just 30
    Left err -> expectationFailure $ "Expected a parse, got " <> show err

-- | A partial month is prorated, so any dates are valid.
test_acceptsMidMonthDates :: TestDBConfig -> IO ()
test_acceptsMidMonthDates cfg = bracketAppM cfg $ do
  let form = underwritingForm {bifStartsOn = "2025-02-03", bifEndsOn = "2025-02-27"}
  result <- runExceptT (parseBreakItemForm BreakItems.Underwriting form)
  liftIO $ case result of
    Right parsed -> do
      parsed.pbiStartsOn `shouldBe` fromGregorian 2025 2 3
      parsed.pbiEndsOn `shouldBe` Just (fromGregorian 2025 2 27)
    Left err -> expectationFailure $ "Expected a parse, got " <> show err

test_rejectsZeroSpots :: TestDBConfig -> IO ()
test_rejectsZeroSpots cfg =
  expectRejected
    cfg
    BreakItems.Underwriting
    underwritingForm {bifSpotsPerMonth = "0"}
    "Spots per month must be a whole number above zero."

test_rejectsNoUnderwriter :: TestDBConfig -> IO ()
test_rejectsNoUnderwriter cfg =
  expectRejected
    cfg
    BreakItems.Underwriting
    underwritingForm {bifUnderwriterId = ""}
    "Choose an underwriter."

test_parsesPsa :: TestDBConfig -> IO ()
test_parsesPsa cfg = bracketAppM cfg $ do
  let form =
        underwritingForm
          { bifStartsOn = "2025-02-03",
            bifUnderwriterId = "",
            bifSpotsPerMonth = ""
          }
  result <- runExceptT (parseBreakItemForm BreakItems.Psa form)
  liftIO $ case result of
    Right parsed -> do
      parsed.pbiStartsOn `shouldBe` fromGregorian 2025 2 3
      parsed.pbiUnderwriterId `shouldBe` Nothing
      parsed.pbiSpotsPerMonth `shouldBe` Nothing
    Left err -> expectationFailure $ "Expected a parse, got " <> show err
