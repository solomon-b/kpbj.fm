{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Invitations.ConflictSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.Conflict (checkInvitationConflicts)
import API.Dashboard.Shows.Slug.Edit.Post.Handler (ParsedScheduleSlot, parseScheduleStartDate)
import API.Invite.Token.Post.Handler (parseInvitationSchedules)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day, DayOfWeek (..), TimeOfDay (..), addDays, fromGregorian)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
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
spec = do
  describe "API.Dashboard.Shows.Slug.Edit.Post.Handler.parseScheduleStartDate" $ do
    let today = fromGregorian 2027 1 10
    it "accepts a future date" $
      parseScheduleStartDate today "2027-01-15" `shouldBe` Right (fromGregorian 2027 1 15)
    it "accepts today" $
      parseScheduleStartDate today "2027-01-10" `shouldBe` Right today
    it "rejects a past date" $
      parseScheduleStartDate today "2027-01-09" `shouldBe` Left "The schedule start date can't be in the past."
    it "rejects a date it cannot parse" $
      parseScheduleStartDate today "nope" `shouldBe` Left "Invalid schedule start date."

  withTestDB $
    describe "API.Dashboard.Invitations.Conflict.checkInvitationConflicts" $ do
      it "rejects a slot that a live invitation holds" $ \cfg -> do
        _ <- insertInvitation cfg "live-monday" (slotJson "monday" weekly "20:00" 120) keep
        result <- runCheck cfg (slotJson "monday" weekly "21:00" 60) farFuture
        expectValidationError
          result
          "Schedule conflict: Monday 21:00-22:00 overlaps with a pending invitation for live-monday@example.com"

      it "ignores an invitation past expires_at" $ \cfg -> do
        _ <- insertInvitation cfg "expired-tuesday" (slotJson "tuesday" weekly "20:00" 120) expire
        result <- runCheck cfg (slotJson "tuesday" weekly "21:00" 60) farFuture
        expectNoConflict result

      it "ignores a live invitation on other weeks" $ \cfg -> do
        _ <- insertInvitation cfg "weeks-wednesday" (slotJson "wednesday" [1, 3] "20:00" 120) keep
        result <- runCheck cfg (slotJson "wednesday" [2, 4] "20:00" 120) farFuture
        expectNoConflict result

      it "rejects a replay that a live invitation holds" $ \cfg -> do
        _ <- insertInvitation cfg "replay-friday" (slotJson "friday" weekly "20:00" 120) keep
        result <- runCheck cfg (slotJsonWithReplay "friday" weekly "10:00" 60 "21:00") farFuture
        expectValidationError
          result
          "Replay conflict: Friday 21:00-22:00 overlaps with a pending invitation for replay-friday@example.com"

      it "rejects a slot that a live invitation reaches past midnight" $ \cfg -> do
        _ <- insertInvitation cfg "late-saturday" (slotJson "saturday" weekly "23:00" 120) keep
        result <- runCheck cfg (slotJson "sunday" weekly "00:00" 60) farFuture
        expectValidationError
          result
          "Schedule conflict: Sunday 00:00-01:00 overlaps with a pending invitation for late-saturday@example.com"

      it "ignores the invitation that an edit changes" $ \cfg -> do
        invId <- insertInvitation cfg "edited-monday" (slotJson "monday" [2] "08:00" 120) keep
        result <- runEditCheck cfg (Just invId) (slotJson "monday" [2] "09:00" 60) farFuture
        expectNoConflict result

      it "rejects an edit into a slot that another live invitation holds" $ \cfg -> do
        _ <- insertInvitation cfg "other-tuesday" (slotJson "tuesday" [2] "08:00" 120) keep
        invId <- insertInvitation cfg "edited-tuesday" (slotJson "tuesday" [4] "08:00" 120) keep
        result <- runEditCheck cfg (Just invId) (slotJson "tuesday" [2] "09:00" 60) farFuture
        expectValidationError
          result
          "Schedule conflict: Tuesday 09:00-10:00 overlaps with a pending invitation for other-tuesday@example.com"

      it "allows a slot that a show leaves before the start date" $ \cfg -> do
        bookShowUntil cfg Thursday (TimeOfDay 20 0 0) (TimeOfDay 22 0 0) farFuture
        onStart <- runCheck cfg (slotJson "thursday" weekly "20:00" 120) farFuture
        expectNoConflict onStart
        dayBefore <- runCheck cfg (slotJson "thursday" weekly "20:00" 120) (addDays (-1) farFuture)
        expectValidationError
          dayBefore
          "Schedule conflict: Thursday 20:00-22:00 overlaps with \"Outgoing Show\""

--------------------------------------------------------------------------------
-- Helpers

-- | Expect a validation error with exactly this message.
expectValidationError :: Either HandlerError () -> Text -> IO ()
expectValidationError result message = case result of
  Left (ValidationError msg) -> msg `shouldBe` message
  other -> expectationFailure $ "Expected a validation error, got " <> show other

-- | Expect the check to pass.
expectNoConflict :: Either HandlerError () -> IO ()
expectNoConflict result = case result of
  Right () -> pure ()
  Left err -> expectationFailure $ "Expected no conflict, got " <> show err

-- | Weekly: every week of the month.
weekly :: [Int64]
weekly = [1, 2, 3, 4, 5]

-- | A start date far enough ahead that no test date passes it.
farFuture :: Day
farFuture = fromGregorian 2030 6 1

-- | One schedule slot in the JSON shape the invitation form submits.
slotJson :: Text -> [Int64] -> Text -> Int -> Aeson.Value
slotJson day weeks start duration =
  Aeson.toJSON
    [ Aeson.object
        [ "dayOfWeek" .= day,
          "weeksOfMonth" .= weeks,
          "startTime" .= start,
          "duration" .= duration
        ]
    ]

-- | One schedule slot with a replay time.
slotJsonWithReplay :: Text -> [Int64] -> Text -> Int -> Text -> Aeson.Value
slotJsonWithReplay day weeks start duration replay =
  Aeson.toJSON
    [ Aeson.object
        [ "dayOfWeek" .= day,
          "weeksOfMonth" .= weeks,
          "startTime" .= start,
          "duration" .= duration,
          "replayTime" .= replay
        ]
    ]

-- | Parse slot JSON the way the handlers do.
parseSlot :: Aeson.Value -> IO (Maybe ParsedScheduleSlot)
parseSlot json = either (fail . show) pure (parseInvitationSchedules json)

-- | Run the conflict check for a new invitation with this slot and start date.
runCheck :: TestDBConfig -> Aeson.Value -> Day -> IO (Either HandlerError ())
runCheck cfg = runEditCheck cfg Nothing

-- | Run the conflict check for an edit of this invitation.
runEditCheck :: TestDBConfig -> Maybe HostInvitation.Id -> Aeson.Value -> Day -> IO (Either HandlerError ())
runEditCheck cfg edited json startsOn = do
  slot <- parseSlot json
  bracketAppM cfg $ runExceptT (checkInvitationConflicts edited slot startsOn)

-- | Insert a pending invitation for @<name>@example.com@, then apply the change.
insertInvitation ::
  TestDBConfig ->
  Text ->
  Aeson.Value ->
  (HostInvitation.Id -> TRX.Transaction ()) ->
  IO HostInvitation.Id
insertInvitation cfg name json change = do
  userInsert <- mkUserInsert name UserMetadata.Staff
  token <- HostInvitation.generateInviteCode
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, _) <- setupUserModels userInsert
      invId <-
        unwrapInsert $
          HostInvitation.insert
            HostInvitation.Insert
              { HostInvitation.iToken = token,
                HostInvitation.iScheduleData = json,
                HostInvitation.iRecipientEmail = mkEmailAddressUnsafe (name <> "@example.com"),
                HostInvitation.iCreatedBy = User.mId userModel,
                HostInvitation.iStartsOn = farFuture
              }
      change invId
      pure invId
    liftIO $ expectSetupRight dbResult

-- | Leave the invitation live.
keep :: HostInvitation.Id -> TRX.Transaction ()
keep _ = pure ()

-- | Move an invitation's expiry into the past. The status stays pending.
expire :: HostInvitation.Id -> TRX.Transaction ()
expire invId =
  TRX.statement () $
    interp False [sql| UPDATE host_invitations SET expires_at = NOW() - INTERVAL '1 minute' WHERE id = #{invId} |]

-- | Book an active show into a weekly slot that ends on this date.
bookShowUntil :: TestDBConfig -> DayOfWeek -> TimeOfDay -> TimeOfDay -> Day -> IO ()
bookShowUntil cfg day start end lastDay = bracketAppM cfg $ do
  dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    showId <-
      unwrapInsert $
        Shows.insertShow
          Shows.Insert
            { Shows.siTitle = "Outgoing Show",
              Shows.siSlug = "outgoing-show",
              Shows.siDescription = Nothing,
              Shows.siLogoUrl = Nothing,
              Shows.siStatus = Shows.Active
            }
    templateId <-
      TRX.statement () $
        ShowSchedule.insertScheduleTemplate
          (ShowSchedule.ScheduleTemplateInsert showId day weekly start end "America/Los_Angeles" Nothing)
    _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-30) lastDay) (Just lastDay)))
    pure ()
  liftIO $ expectSetupRight dbResult
