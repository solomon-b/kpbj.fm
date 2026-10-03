module API.Dashboard.BreakItems.SharedSpec where

--------------------------------------------------------------------------------

import API.Dashboard.BreakItems.Form (BreakItemForm (..))
import API.Dashboard.BreakItems.Shared (ParsedBreakItem (..), checkItemLength, parseBreakItemForm)
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
spec = do
  withTestDB $
    describe "API.Dashboard.BreakItems.Shared" $
      describe "parseBreakItemForm" $ do
        it "parses an underwriting announcement" test_parsesUnderwriting
        it "accepts underwriting dates inside a month" test_acceptsMidMonthDates
        it "rejects underwriting with no underwriter" test_rejectsNoUnderwriter
        it "parses a PSA on any date with no underwriter" test_parsesPsa

  describe "API.Dashboard.BreakItems.Shared.checkItemLength" $ do
    it "rejects a PSA longer than 30 seconds" $
      checkItemLength BreakItems.Psa (Just 42)
        `shouldBe` Left "PSAs can run at most 30 seconds. This file runs 42."
    it "rejects underwriting longer than 30 seconds" $
      checkItemLength BreakItems.Underwriting (Just 31)
        `shouldBe` Left "Underwriting announcements can run at most 30 seconds. This file runs 31."
    it "accepts a PSA exactly at its limit" $
      checkItemLength BreakItems.Psa (Just 30) `shouldBe` Right 30
    it "accepts underwriting exactly at its limit" $
      checkItemLength BreakItems.Underwriting (Just 30) `shouldBe` Right 30
    it "rejects an upload with no measured length" $
      checkItemLength BreakItems.Psa Nothing
        `shouldBe` Left "Could not read the audio length. Upload the file again."

--------------------------------------------------------------------------------

-- | A valid underwriting form. Each test changes one field.
underwritingForm :: BreakItemForm
underwritingForm =
  BreakItemForm
    { bifTitle = "Spot",
      bifAudioToken = "",
      bifStartsOn = "2025-02-01",
      bifEndsOn = "",
      bifUnderwriterId = "1"
    }

-- | A valid PSA form.
psaForm :: BreakItemForm
psaForm = underwritingForm {bifUnderwriterId = ""}

-- | Parse the form and expect a validation error with this message.
expectRejected :: TestDBConfig -> BreakItems.Category -> BreakItemForm -> Text -> IO ()
expectRejected cfg category form message = bracketAppM cfg $ do
  result <- runExceptT (parseBreakItemForm category form)
  liftIO $ case result of
    Left (ValidationError msg) -> msg `shouldBe` message
    other -> expectationFailure $ "Expected a validation error, got " <> show other

-- | Parse the form and expect it to succeed.
expectParsed :: TestDBConfig -> BreakItems.Category -> BreakItemForm -> (ParsedBreakItem -> IO ()) -> IO ()
expectParsed cfg category form check = bracketAppM cfg $ do
  result <- runExceptT (parseBreakItemForm category form)
  liftIO $ case result of
    Right parsed -> check parsed
    Left err -> expectationFailure $ "Expected a parse, got " <> show err

test_parsesUnderwriting :: TestDBConfig -> IO ()
test_parsesUnderwriting cfg =
  expectParsed cfg BreakItems.Underwriting underwritingForm $ \parsed -> do
    parsed.pbiStartsOn `shouldBe` fromGregorian 2025 2 1
    parsed.pbiEndsOn `shouldBe` Nothing
    parsed.pbiUnderwriterId `shouldBe` Just (Underwriters.Id 1)

test_acceptsMidMonthDates :: TestDBConfig -> IO ()
test_acceptsMidMonthDates cfg =
  expectParsed
    cfg
    BreakItems.Underwriting
    underwritingForm {bifStartsOn = "2025-02-03", bifEndsOn = "2025-02-27"}
    $ \parsed -> do
      parsed.pbiStartsOn `shouldBe` fromGregorian 2025 2 3
      parsed.pbiEndsOn `shouldBe` Just (fromGregorian 2025 2 27)

test_rejectsNoUnderwriter :: TestDBConfig -> IO ()
test_rejectsNoUnderwriter cfg =
  expectRejected
    cfg
    BreakItems.Underwriting
    underwritingForm {bifUnderwriterId = ""}
    "Choose an underwriter."

test_parsesPsa :: TestDBConfig -> IO ()
test_parsesPsa cfg =
  expectParsed cfg BreakItems.Psa psaForm {bifStartsOn = "2025-02-03"} $ \parsed -> do
    parsed.pbiStartsOn `shouldBe` fromGregorian 2025 2 3
    parsed.pbiUnderwriterId `shouldBe` Nothing
