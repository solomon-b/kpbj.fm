module API.Dashboard.Invitations.New.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.New.Post.Handler (action)
import API.Dashboard.Invitations.New.Post.Route (NewInvitationForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, addDays, getCurrentTime, showGregorian)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.Timezone (pacificDay)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Gen.EmailAddress (mkEmailAddressUnsafe)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Invitations.New.Post.Handler.action" $ do
      it "rejects an empty start date" $ \cfg -> do
        result <- runAction cfg "empty-date" (form "empty@example.com" mondaySlot "")
        expectValidationError result "Schedule start date is required."

      it "rejects a past start date" $ \cfg -> do
        yesterday <- addDays (-1) <$> todayPacific
        result <- runAction cfg "past-date" (form "past@example.com" mondaySlot (dateText yesterday))
        expectValidationError result "The schedule start date can't be in the past."

      it "rejects a slot that a live invitation holds" $ \cfg -> do
        start <- addDays 14 <$> todayPacific
        first <- runAction cfg "first-host" (form "first@example.com" mondaySlot (dateText start))
        case first of
          Right _ -> pure ()
          Left err -> expectationFailure $ "First invitation failed: " <> show err
        second <- runAction cfg "second-host" (form "second@example.com" mondaySlot (dateText start))
        case second of
          Left (ValidationError msg) -> Text.take 18 msg `shouldBe` "Schedule conflict:"
          other -> expectationFailure $ "Expected a validation error, got " <> show other

      it "stores the start date" $ \cfg -> do
        start <- addDays 14 <$> todayPacific
        result <- runAction cfg "stores-date" (form "stored@example.com" mondaySlot (dateText start))
        case result of
          Left err -> expectationFailure $ "Expected success, got " <> show err
          Right (token, _) -> do
            mInvitation <- bracketAppM cfg $ execQuery (HostInvitation.getByToken token)
            case mInvitation of
              Right (Just invitation) -> HostInvitation.hiStartsOn invitation `shouldBe` start
              other -> expectationFailure $ "Expected the stored invitation, got " <> show other

--------------------------------------------------------------------------------
-- Helpers

-- | A weekly Monday slot, 20:00 for two hours.
mondaySlot :: Text
mondaySlot = "[{\"dayOfWeek\":\"monday\",\"weeksOfMonth\":[1,2,3,4,5],\"startTime\":\"20:00\",\"duration\":120}]"

-- | The new invitation form as the dashboard submits it.
form :: Text -> Text -> Text -> NewInvitationForm
form recipient schedules startDate =
  NewInvitationForm
    { nifRecipientEmail = mkEmailAddressUnsafe recipient,
      nifSchedulesJson = schedules,
      nifScheduleStartDate = startDate
    }

-- | A date in the form's @YYYY-MM-DD@ format.
dateText :: Day -> Text
dateText = Text.pack . showGregorian

-- | Today, as a Pacific date.
todayPacific :: IO Day
todayPacific = pacificDay <$> getCurrentTime

-- | Create a staff user and run the action as that user.
runAction :: TestDBConfig -> Text -> NewInvitationForm -> IO (Either HandlerError (HostInvitation.Token, EmailAddress))
runAction cfg name newForm = do
  userInsert <- mkUserInsert name UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ snd <$> setupUserModels userInsert
    userMetadata <- liftIO $ expectSetupRight dbResult
    runExceptT (action userMetadata newForm)

-- | Expect a validation error with exactly this message.
expectValidationError :: Either HandlerError a -> Text -> IO ()
expectValidationError result message = case result of
  Left (ValidationError msg) -> msg `shouldBe` message
  Left err -> expectationFailure $ "Expected a validation error, got " <> show err
  Right _ -> expectationFailure "Expected a validation error, got success"
