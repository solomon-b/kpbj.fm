module API.Dashboard.AirBreaks.BreakItems.SharedSpec where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.BreakItems.Form (BreakItemForm (..))
import API.Dashboard.AirBreaks.BreakItems.Shared (ParsedBreakItem (..), Section (..), announcementSection, checkItemLength, newPostAction, parseBreakItemForm)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (fromGregorian)
import Domain.BreakPlanner (announcementMaxSeconds)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.BreakItemFixtures (stagedBreakItemAudio, writeStagedAudioFile)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  withTestDB $
    describe "API.Dashboard.AirBreaks.BreakItems.Shared" $
      describe "parseBreakItemForm" $ do
        it "parses an underwriting announcement" test_parsesUnderwriting
        it "accepts underwriting dates inside a month" test_acceptsMidMonthDates
        it "rejects underwriting with no underwriter" test_rejectsNoUnderwriter
        it "parses a PSA on any date with no underwriter" test_parsesPsa

  withTestDB $
    describe "API.Dashboard.AirBreaks.BreakItems.Shared.announcementSection" $ do
      it "refuses a host" test_announcementRefusesHost
      it "saves an approved announcement for staff" test_announcementSaves

  describe "API.Dashboard.AirBreaks.BreakItems.Shared.checkItemLength" $ do
    it "rejects an announcement longer than its limit" $
      checkItemLength BreakItems.Announcement (Just (announcementMaxSeconds + 1))
        `shouldBe` Left
          ( "Announcements can run at most "
              <> Text.pack (show announcementMaxSeconds)
              <> " seconds. This file runs "
              <> Text.pack (show (announcementMaxSeconds + 1))
              <> "."
          )
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

--------------------------------------------------------------------------------
-- The announcement section

test_announcementRefusesHost :: TestDBConfig -> IO ()
test_announcementRefusesHost cfg = do
  userInsert <- mkUserInsert "announcement-host" UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ setupUserModels userInsert
    (_userModel, userMeta) <- liftIO $ expectSetupRight dbResult
    result <- runExceptT (announcementSection.secRequire userMeta)
    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      other -> expectationFailure $ "Expected NotAuthorized, got " <> show other

test_announcementSaves :: TestDBConfig -> IO ()
test_announcementSaves cfg = do
  let token = "announcement-fund-drive"
  userInsert <- mkUserInsert token UserMetadata.Staff
  writeStagedAudioFile token
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMeta) <- setupUserModels userInsert
      _uploadId <- TRX.statement () (StagedUploads.insert (stagedBreakItemAudio token userModel 28))
      pure (userModel, userMeta)
    (userModel, userMeta) <- liftIO $ expectSetupRight dbResult

    let form =
          psaForm
            { bifTitle = "Fall Fund Drive",
              bifAudioToken = token,
              bifStartsOn = "2026-10-20",
              bifEndsOn = "2026-11-15"
            }
    result <- runExceptT (newPostAction announcementSection userModel userMeta form)
    case result of
      Left err -> liftIO $ expectationFailure $ "Expected the announcement to save, got " <> show err
      Right itemId -> do
        stored <- execQuery (BreakItems.getById itemId)
        liftIO $ case stored of
          Right (Just item) -> do
            BreakItems.bimCategory item `shouldBe` BreakItems.Announcement
            BreakItems.bimReviewStatus item `shouldBe` BreakItems.Approved
            BreakItems.bimShowId item `shouldBe` Nothing
            BreakItems.bimDurationSeconds item `shouldBe` 28
          other -> expectationFailure $ "Expected the saved announcement, got " <> show other
