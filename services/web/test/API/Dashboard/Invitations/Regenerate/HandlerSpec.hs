{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Invitations.Regenerate.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.Regenerate.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, addDays, getCurrentTime)
import Domain.Types.Timezone (pacificDay)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Interpolate (interp, sql)
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Gen.EmailAddress (mkEmailAddressUnsafe)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Invitations.Regenerate.Handler.action" $ do
      it "rejects a live pending invitation" $ \cfg -> do
        startsOn <- addDays 14 <$> todayPacific
        (staff, invId) <- setupInvitation cfg "live" (slotJson "monday") startsOn (\_ _ -> pure ())
        result <- runRegenerate cfg staff invId
        expectValidationError result "Only expired or revoked invitations can be regenerated."

      it "rejects a claimed invitation" $ \cfg -> do
        startsOn <- addDays 14 <$> todayPacific
        (staff, invId) <- setupInvitation cfg "claimed" (slotJson "tuesday") startsOn claim
        result <- runRegenerate cfg staff invId
        expectValidationError result "Only expired or revoked invitations can be regenerated."

      it "regenerates an expired invitation with the same start date" $ \cfg -> do
        startsOn <- addDays 14 <$> todayPacific
        (staff, invId) <- setupInvitation cfg "expired" (slotJson "wednesday") startsOn (const expire)
        result <- runRegenerate cfg staff invId
        case result of
          Left err -> expectationFailure $ "Expected success, got " <> show err
          Right token -> do
            (mOld, mNew) <- bracketAppM cfg $ do
              old <- execQuery (HostInvitation.getById invId)
              new <- execQuery (HostInvitation.getByToken token)
              pure (old, new)
            case (mOld, mNew) of
              (Right (Just old), Right (Just new)) -> do
                HostInvitation.hiStatus old `shouldBe` HostInvitation.Expired
                HostInvitation.hiRecipientEmail new `shouldBe` HostInvitation.hiRecipientEmail old
                HostInvitation.hiScheduleData new `shouldBe` HostInvitation.hiScheduleData old
                HostInvitation.hiStartsOn new `shouldBe` startsOn
              other -> expectationFailure $ "Expected both invitations, got " <> show other

      it "regenerates a revoked invitation whose start date has passed" $ \cfg -> do
        startsOn <- addDays (-10) <$> todayPacific
        (staff, invId) <- setupInvitation cfg "revoked-past" (slotJson "thursday") startsOn (const revoke)
        result <- runRegenerate cfg staff invId
        case result of
          Right _ -> pure ()
          Left err -> expectationFailure $ "Expected success, got " <> show err

      it "rejects when another live invitation now holds the slot" $ \cfg -> do
        startsOn <- addDays 14 <$> todayPacific
        (staff, revokedId) <- setupInvitation cfg "revoked-a" (slotJson "friday") startsOn (const revoke)
        _ <- setupInvitation cfg "live-b" (slotJson "friday") startsOn (\_ _ -> pure ())
        result <- runRegenerate cfg staff revokedId
        case result of
          Left (ValidationError msg) -> Text.take 18 msg `shouldBe` "Schedule conflict:"
          other -> expectationFailure $ "Expected a validation error, got " <> show other

--------------------------------------------------------------------------------
-- Helpers

-- | Today, as a Pacific date.
todayPacific :: IO Day
todayPacific = pacificDay <$> getCurrentTime

-- | A weekly slot on this day, 20:00 for two hours.
slotJson :: Text -> Aeson.Value
slotJson day =
  Aeson.toJSON
    [ Aeson.object
        [ "dayOfWeek" .= day,
          "weeksOfMonth" .= ([1, 2, 3, 4, 5] :: [Int]),
          "startTime" .= ("20:00" :: Text),
          "duration" .= (120 :: Int)
        ]
    ]

-- | Create a staff user and an invitation for @<name>@example.com@, then apply
-- the change. Return the staff member's metadata and the invitation id.
setupInvitation ::
  TestDBConfig ->
  Text ->
  Aeson.Value ->
  Day ->
  (HostInvitation.Token -> HostInvitation.Id -> TRX.Transaction ()) ->
  IO (UserMetadata.Model, HostInvitation.Id)
setupInvitation cfg name json startsOn change = do
  userInsert <- mkUserInsert (name <> "-staff") UserMetadata.Staff
  token <- HostInvitation.generateInviteCode
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetadata) <- setupUserModels userInsert
      invId <-
        unwrapInsert $
          HostInvitation.insert
            HostInvitation.Insert
              { HostInvitation.iToken = token,
                HostInvitation.iScheduleData = json,
                HostInvitation.iRecipientEmail = mkEmailAddressUnsafe (name <> "@example.com"),
                HostInvitation.iCreatedBy = User.mId userModel,
                HostInvitation.iStartsOn = startsOn
              }
      change token invId
      pure (userMetadata, invId)
    liftIO $ expectSetupRight dbResult

-- | Mark the invitation claimed by its creator.
claim :: HostInvitation.Token -> HostInvitation.Id -> TRX.Transaction ()
claim token invId = do
  inv <- TRX.statement () (HostInvitation.getById invId)
  case inv of
    Just m -> void $ TRX.statement () (HostInvitation.claimInvitation token (HostInvitation.hiCreatedBy m))
    Nothing -> pure ()

-- | Revoke the invitation.
revoke :: HostInvitation.Id -> TRX.Transaction ()
revoke invId = void $ TRX.statement () (HostInvitation.revokeInvitation invId)

-- | Move the invitation's expiry into the past. The status stays pending.
expire :: HostInvitation.Id -> TRX.Transaction ()
expire invId =
  TRX.statement () $
    interp False [sql| UPDATE host_invitations SET expires_at = NOW() - INTERVAL '1 minute' WHERE id = #{invId} |]

-- | Run Regenerate as this staff member.
runRegenerate :: TestDBConfig -> UserMetadata.Model -> HostInvitation.Id -> IO (Either HandlerError HostInvitation.Token)
runRegenerate cfg staff invId = bracketAppM cfg $ runExceptT (action staff invId)

-- | Expect a validation error with exactly this message.
expectValidationError :: Either HandlerError a -> Text -> IO ()
expectValidationError result message = case result of
  Left (ValidationError msg) -> msg `shouldBe` message
  Left err -> expectationFailure $ "Expected a validation error, got " <> show err
  Right _ -> expectationFailure "Expected a validation error, got success"
