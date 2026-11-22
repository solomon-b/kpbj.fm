module API.Invite.Token.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Invite.Token.Post.Handler (OnboardResult (..), action)
import API.Invite.Token.Post.Route (InviteOnboardingForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Time (Day, DayOfWeek (..), TimeOfDay (..), addDays, getCurrentTime)
import Domain.Types.Timezone (pacificDay)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
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
    describe "API.Invite.Token.Post.Handler.action" $ do
      it "a future start date sets effective_from to that date" $ \cfg -> do
        today <- todayPacific
        let startsOn = addDays 14 today
        -- The outgoing show holds the slot until the start date.
        bookShowUntil cfg Friday (TimeOfDay 20 0 0) (TimeOfDay 22 0 0) startsOn
        token <- insertInvitation cfg "future-start" (slotJson "friday" "20:00" 120) startsOn
        result <- runClaim cfg token "future-start" "Fresh Friday Show"
        effectiveFrom <- expectEffectiveFrom cfg result
        effectiveFrom `shouldBe` [startsOn]

      it "a past start date sets effective_from to today" $ \cfg -> do
        today <- todayPacific
        token <- insertInvitation cfg "past-start" (slotJson "saturday" "20:00" 120) (addDays (-10) today)
        result <- runClaim cfg token "past-start" "Late Saturday Show"
        effectiveFrom <- expectEffectiveFrom cfg result
        effectiveFrom `shouldBe` [today]

      it "rejects a submission whose email differs from the invitation recipient" $ \cfg -> do
        today <- todayPacific
        token <- insertInvitation cfg "bound-email" (slotJson "sunday" "20:00" 120) (addDays 14 today)
        result <- runClaimWithEmail cfg token "someone-else@example.com" "bound-email" "Wrong Email Show"
        case result of
          Left (ValidationError msg) ->
            msg
              `shouldBe` "This invitation is bound to a different email address. Contact staff if the recipient needs to be changed."
          Left err -> expectationFailure $ "Expected a validation error, got " <> show err
          Right _ -> expectationFailure "Expected a validation error, got success"
        -- The invitation stays pending, so the real recipient can still claim it.
        mInvitation <- bracketAppM cfg $ execQuery (HostInvitation.getByToken token)
        case mInvitation of
          Right (Just invitation) -> HostInvitation.hiStatus invitation `shouldBe` HostInvitation.Pending
          other -> expectationFailure $ "Expected the pending invitation, got " <> show other

--------------------------------------------------------------------------------
-- Helpers

-- | Today, as a Pacific date.
todayPacific :: IO Day
todayPacific = pacificDay <$> getCurrentTime

-- | One weekly schedule slot in the JSON shape the invitation form submits.
slotJson :: Text -> Text -> Int -> Aeson.Value
slotJson day start duration =
  Aeson.toJSON
    [ Aeson.object
        [ "dayOfWeek" .= day,
          "weeksOfMonth" .= ([1, 2, 3, 4, 5] :: [Int]),
          "startTime" .= start,
          "duration" .= duration
        ]
    ]

-- | Insert a pending invitation for @<name>@example.com@ with this start date.
insertInvitation :: TestDBConfig -> Text -> Aeson.Value -> Day -> IO HostInvitation.Token
insertInvitation cfg name json startsOn = do
  userInsert <- mkUserInsert (name <> "-staff") UserMetadata.Staff
  token <- HostInvitation.generateInviteCode
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, _) <- setupUserModels userInsert
      _ <-
        unwrapInsert $
          HostInvitation.insert
            HostInvitation.Insert
              { HostInvitation.iToken = token,
                HostInvitation.iScheduleData = json,
                HostInvitation.iRecipientEmail = mkEmailAddressUnsafe (name <> "@example.com"),
                HostInvitation.iCreatedBy = User.mId userModel,
                HostInvitation.iStartsOn = startsOn
              }
      pure ()
    liftIO $ expectSetupRight dbResult
  pure token

-- | Submit the onboarding form for this invitation.
runClaim :: TestDBConfig -> HostInvitation.Token -> Text -> Text -> IO (Either HandlerError OnboardResult)
runClaim cfg token name =
  runClaimWithEmail cfg token (name <> "@example.com") name

-- | Submit the onboarding form for this invitation with this email.
runClaimWithEmail :: TestDBConfig -> HostInvitation.Token -> Text -> Text -> Text -> IO (Either HandlerError OnboardResult)
runClaimWithEmail cfg token email name showTitle =
  bracketAppM cfg $
    runExceptT $
      action
        token
        InviteOnboardingForm
          { iofFullName = "Test Host",
            iofEmail = mkEmailAddressUnsafe email,
            iofDisplayName = name,
            iofPassword = "a-long-password",
            iofNewsletter = Nothing,
            iofShowTitle = showTitle,
            iofShowDescription = "",
            iofShowTags = Nothing,
            iofShowLogo = Nothing
          }

-- | Expect a successful claim. Return the effective_from of each validity row
-- of the new show's schedule.
expectEffectiveFrom :: TestDBConfig -> Either HandlerError OnboardResult -> IO [Day]
expectEffectiveFrom cfg result = case result of
  Left err -> expectationFailure ("Expected the claim to succeed, got " <> show err) >> pure []
  Right onboard -> bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ do
      templates <- TRX.statement () (ShowSchedule.getScheduleTemplatesForShow (orShowId onboard))
      validities <- traverse (TRX.statement () . ShowSchedule.getValidityPeriodsForTemplate . ShowSchedule.stId) templates
      pure (map ShowSchedule.stvEffectiveFrom (concat validities))
    liftIO $ expectSetupRight dbResult

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
          (ShowSchedule.ScheduleTemplateInsert showId day [1, 2, 3, 4, 5] start end "America/Los_Angeles" Nothing)
    _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-30) lastDay) (Just lastDay)))
    pure ()
  liftIO $ expectSetupRight dbResult
