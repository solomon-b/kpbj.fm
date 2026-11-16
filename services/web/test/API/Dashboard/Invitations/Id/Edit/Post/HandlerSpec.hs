module API.Dashboard.Invitations.Id.Edit.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.Id.Edit.Post.Handler (action)
import API.Dashboard.Invitations.Id.Edit.Post.Route (EditInvitationForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (Day, addDays, getCurrentTime, showGregorian)
import Domain.Types.EmailAddress (mkEmailAddress)
import Domain.Types.Timezone (pacificDay)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Invitations.Id.Edit.Post.Handler.action" $ do
      it "stores the new recipient, schedule, and start date" $ \cfg -> do
        today <- todayPacific
        invId <- insertInvitation cfg "stores-all" mondaySlot (addDays 14 today)
        let newStart = addDays 21 today
        result <- runAction cfg invId (form "moved@example.com" tuesdaySlot (dateText newStart))
        expectSuccess result
        invitation <- storedInvitation cfg invId
        HostInvitation.hiRecipientEmail invitation `shouldBe` mkEmailAddress "moved@example.com"
        Just (HostInvitation.hiScheduleData invitation) `shouldBe` Aeson.decodeStrict (Text.encodeUtf8 tuesdaySlot)
        HostInvitation.hiStartsOn invitation `shouldBe` newStart

      it "accepts a slot that overlaps the old slot of the same invitation" $ \cfg -> do
        start <- addDays 14 <$> todayPacific
        invId <- insertInvitation cfg "own-slot" mondaySlot start
        result <- runAction cfg invId (form "own-slot@example.com" mondayLateSlot (dateText start))
        expectSuccess result

      it "rejects a slot that another live invitation holds" $ \cfg -> do
        start <- addDays 14 <$> todayPacific
        _ <- insertInvitation cfg "holder" mondaySlot start
        invId <- insertInvitation cfg "mover" tuesdaySlot start
        result <- runAction cfg invId (form "mover@example.com" mondayLateSlot (dateText start))
        case result of
          Left (ValidationError msg) -> Text.take 18 msg `shouldBe` "Schedule conflict:"
          other -> expectationFailure $ "Expected a validation error, got " <> show other

      it "accepts a past start date that did not change" $ \cfg -> do
        past <- addDays (-3) <$> todayPacific
        invId <- insertInvitation cfg "past-kept" mondaySlot past
        result <- runAction cfg invId (form "past-kept@example.com" tuesdaySlot (dateText past))
        expectSuccess result
        invitation <- storedInvitation cfg invId
        HostInvitation.hiStartsOn invitation `shouldBe` past

      it "rejects a past start date that changed" $ \cfg -> do
        today <- todayPacific
        invId <- insertInvitation cfg "past-changed" mondaySlot (addDays (-3) today)
        result <- runAction cfg invId (form "past-changed@example.com" mondaySlot (dateText (addDays (-1) today)))
        expectValidationError result "The schedule start date can't be in the past."

      it "rejects an empty start date" $ \cfg -> do
        start <- addDays 14 <$> todayPacific
        invId <- insertInvitation cfg "empty-date" mondaySlot start
        result <- runAction cfg invId (form "empty-date@example.com" mondaySlot "")
        expectValidationError result "Schedule start date is required."

      it "rejects a revoked invitation" $ \cfg -> do
        start <- addDays 14 <$> todayPacific
        invId <- insertInvitation cfg "revoked" mondaySlot start
        revoked <- bracketAppM cfg $ execQuery (HostInvitation.revokeInvitation invId)
        case revoked of
          Right (Just _) -> pure ()
          other -> expectationFailure $ "Revoke failed: " <> show other
        result <- runAction cfg invId (form "revoked@example.com" tuesdaySlot (dateText start))
        expectValidationError result "Only pending invitations can be edited."

--------------------------------------------------------------------------------
-- Helpers

-- | A weekly Monday slot, 20:00 for two hours.
mondaySlot :: Text
mondaySlot = "[{\"dayOfWeek\":\"monday\",\"weeksOfMonth\":[1,2,3,4,5],\"startTime\":\"20:00\",\"duration\":120}]"

-- | A weekly Monday slot inside 'mondaySlot', 21:00 for one hour.
mondayLateSlot :: Text
mondayLateSlot = "[{\"dayOfWeek\":\"monday\",\"weeksOfMonth\":[1,2,3,4,5],\"startTime\":\"21:00\",\"duration\":60}]"

-- | A weekly Tuesday slot, 20:00 for two hours.
tuesdaySlot :: Text
tuesdaySlot = "[{\"dayOfWeek\":\"tuesday\",\"weeksOfMonth\":[1,2,3,4,5],\"startTime\":\"20:00\",\"duration\":120}]"

-- | The edit invitation form as the dashboard submits it.
form :: Text -> Text -> Text -> EditInvitationForm
form recipient schedules startDate =
  EditInvitationForm
    { eifRecipientEmail = recipient,
      eifSchedulesJson = schedules,
      eifScheduleStartDate = startDate
    }

-- | A date in the form's @YYYY-MM-DD@ format.
dateText :: Day -> Text
dateText = Text.pack . showGregorian

-- | Today, as a Pacific date.
todayPacific :: IO Day
todayPacific = pacificDay <$> getCurrentTime

-- | Insert a pending invitation for @<name>@example.com@. A new staff user creates it.
insertInvitation :: TestDBConfig -> Text -> Text -> Day -> IO HostInvitation.Id
insertInvitation cfg name schedules startsOn = do
  userInsert <- mkUserInsert (name <> "-staff") UserMetadata.Staff
  token <- HostInvitation.generateInviteCode
  scheduleData <- maybe (fail "The test schedule is not JSON") pure (Aeson.decodeStrict (Text.encodeUtf8 schedules))
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, _) <- setupUserModels userInsert
      unwrapInsert $
        HostInvitation.insert
          HostInvitation.Insert
            { HostInvitation.iToken = token,
              HostInvitation.iScheduleData = scheduleData,
              HostInvitation.iRecipientEmail = mkEmailAddress (name <> "@example.com"),
              HostInvitation.iCreatedBy = User.mId userModel,
              HostInvitation.iStartsOn = startsOn
            }
    liftIO $ expectSetupRight dbResult

-- | Run the edit action on this invitation.
runAction :: TestDBConfig -> HostInvitation.Id -> EditInvitationForm -> IO (Either HandlerError ())
runAction cfg invId editForm = bracketAppM cfg $ runExceptT (action invId editForm)

-- | Read the invitation as it is stored after the action.
storedInvitation :: TestDBConfig -> HostInvitation.Id -> IO HostInvitation.Model
storedInvitation cfg invId = do
  result <- bracketAppM cfg $ execQuery (HostInvitation.getById invId)
  case result of
    Right (Just invitation) -> pure invitation
    other -> fail $ "Expected the stored invitation, got " <> show other

-- | Expect the action to pass.
expectSuccess :: Either HandlerError () -> IO ()
expectSuccess result = case result of
  Right () -> pure ()
  Left err -> expectationFailure $ "Expected success, got " <> show err

-- | Expect a validation error with exactly this message.
expectValidationError :: Either HandlerError () -> Text -> IO ()
expectValidationError result message = case result of
  Left (ValidationError msg) -> msg `shouldBe` message
  other -> expectationFailure $ "Expected a validation error, got " <> show other
