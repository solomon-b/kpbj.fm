{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Invitations.New.Post.Handler
  ( handler,
    action,
    validateRecipientEmail,
    validateSchedulesJson,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.Conflict (checkInvitationConflicts)
import API.Dashboard.Invitations.New.Post.Route (NewInvitationForm (..))
import API.Dashboard.Shows.Slug.Edit.Post.Handler (ParsedScheduleSlot, parseScheduleStartDate)
import API.Invite.Token.Post.Handler (parseInvitationSchedules)
import API.Links (dashboardInvitationsLinks, inviteLinks, rootLink)
import API.Types
import App.Auth.Roles (RoleAuth (..), Satisfies (..))
import App.BaseUrl (baseUrl)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson qualified as Aeson
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.Timezone (pacificDay)
import Effects.Clock (currentSystemTime)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.HostInvitationEmail (sendHostInvitationEmail)
import Log qualified
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardInvitationsGetUrl :: Text
dashboardInvitationsGetUrl =
  let listUrl = Links.linkURI dashboardInvitationsLinks.list
   in [i|/#{listUrl}|]

--------------------------------------------------------------------------------

-- | Handler for POST /dashboard/invitations/new.
handler ::
  Satisfies 'UserMetadata.Staff RoleAuth ->
  NewInvitationForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler (Satisfies _ auth) form =
  handleRedirectErrors "Invitation creation" dashboardInvitationsLinks.newGet $ do
    -- 1. Validate and store the invitation.
    (token, validatedEmail) <- action auth.raMetadata form

    -- 2. Fire-and-forget invitation email. Logs internally on failure;
    --    must not block the redirect.
    lift $ sendHostInvitationEmail validatedEmail (HostInvitation.unToken token)

    -- 3. Build the invite URL for the flash message (manual-fallback copy/paste).
    appBaseUrl <- lift baseUrl
    let inviteUrl = appBaseUrl <> inviteLinkPath token

    Log.logInfo "Created host invitation" (HostInvitation.unToken token)

    -- 4. Redirect to invitation list with success flash.
    let flash =
          FlashMessage
            Success
            "Invitation sent"
            [i|Emailed #{display validatedEmail}. Copy the link if you need to send it manually: #{inviteUrl}|]
    pure $
      Servant.addHeader dashboardInvitationsGetUrl $
        Servant.addHeader (flashCookie (Just flash)) Servant.NoContent

-- | Validate the form and store a new invitation.
--
-- Returns the new token and the recipient. The caller sends the email.
action ::
  -- | The staff member who creates the invitation
  UserMetadata.Model ->
  NewInvitationForm ->
  ExceptT HandlerError AppM (HostInvitation.Token, EmailAddress)
action userMetadata form = do
  -- 1. Validate the recipient. The email must not match an existing KPBJ user.
  let validatedEmail = nifRecipientEmail form
  validateRecipientEmail validatedEmail

  -- 2. Validate that schedulesJson is non-empty valid JSON
  (scheduleData, slot) <- validateSchedulesJson (nifSchedulesJson form)

  -- 3. Validate the start date. The form requires one.
  today <- pacificDay <$> lift currentSystemTime
  startsOn <- case Text.strip (nifScheduleStartDate form) of
    "" -> throwValidationError "Schedule start date is required."
    dateText -> either throwValidationError pure (parseScheduleStartDate today dateText)

  -- 4. Reject a slot that a show holds on the start date, or that another
  --    live invitation holds.
  checkInvitationConflicts Nothing (Just slot) startsOn

  -- 5. Generate a human-readable invite code (INV-XXXX-XXXX).
  token <- liftIO HostInvitation.generateInviteCode

  -- 6. Insert into host_invitations
  let insertData =
        HostInvitation.Insert
          { HostInvitation.iToken = token,
            HostInvitation.iScheduleData = scheduleData,
            HostInvitation.iRecipientEmail = validatedEmail,
            HostInvitation.iCreatedBy = userMetadata.mUserId,
            HostInvitation.iStartsOn = startsOn
          }

  insertResult <- execQuery (HostInvitation.insert insertData)
  case insertResult of
    Left dbErr -> throwDatabaseError dbErr
    Right Nothing -> throwHandlerFailure "Failed to create invitation."
    Right (Just _invitationId) -> pure (token, validatedEmail)

--------------------------------------------------------------------------------

-- | Validate the recipient email of an invitation.
--
-- The email must not belong to an existing KPBJ user. An existing user gets a
-- show through the host-management screen.
validateRecipientEmail ::
  EmailAddress ->
  ExceptT HandlerError AppM ()
validateRecipientEmail recipientEmail = do
  existing <- execQuery (User.getUserByEmail recipientEmail)
  case existing of
    Left dbErr -> throwDatabaseError dbErr
    Right (Just _) ->
      throwValidationError
        "That email belongs to an existing KPBJ account. \
        \Use the user's host-management screen instead."
    Right Nothing -> pure ()

-- | Validate the schedules JSON and return it for storage on the invitation.
--
-- The slots go through 'parseInvitationSchedules', the same function that runs when
-- the host opens the link. Without that, a schedule the acceptance path rejects is
-- stored here and fails later, in front of the host, who cannot fix it. Staff see the
-- message while they still have the form open.
--
-- The raw 'Aeson.Value' is what gets stored, not the parsed slot, because the
-- invitation keeps the submission as sent. The parsed slot is returned too, for
-- the conflict check.
validateSchedulesJson ::
  Text ->
  ExceptT HandlerError AppM (Aeson.Value, ParsedScheduleSlot)
validateSchedulesJson schedulesJson
  | Text.null (Text.strip schedulesJson) = missingSchedule
  | schedulesJson == "[]" = missingSchedule
  | otherwise =
      case Aeson.eitherDecodeStrict (Text.encodeUtf8 schedulesJson) of
        Left err ->
          throwValidationError ("Invalid schedule data: " <> Text.pack err)
        Right val -> case parseInvitationSchedules val of
          Left err -> throwValidationError err
          Right Nothing -> missingSchedule
          Right (Just slot) -> pure (val, slot)
  where
    missingSchedule =
      throwValidationError "Schedule is required. Please add at least one time slot."

--------------------------------------------------------------------------------

-- | Build the path portion of the invite link for a given token.
inviteLinkPath :: HostInvitation.Token -> Text
inviteLinkPath token =
  rootLink $ onboardGet inviteLinks token
