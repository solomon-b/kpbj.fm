{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Invitations.New.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.New.Post.Route (NewInvitationForm (..))
import API.Links (dashboardInvitationsLinks, inviteLinks, rootLink)
import API.Types
import App.BaseUrl (baseUrl)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
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
import Domain.Types.Cookie (Cookie)
import Domain.Types.EmailAddress (mkEmailAddress)
import Domain.Types.EmailAddress qualified as EmailAddress
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
  Maybe Cookie ->
  NewInvitationForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler cookie form =
  handleRedirectErrors "Invitation creation" dashboardInvitationsLinks.newGet $ do
    -- 1. Authenticate and require staff role
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can create invitations." userMetadata

    -- 2. Validate recipient email format.
    let trimmedEmail = Text.strip (nifRecipientEmail form)
    validatedEmail <- case EmailAddress.validate (mkEmailAddress trimmedEmail) of
      Left _ -> throwValidationError "Recipient email is not a valid email address."
      Right addr -> pure addr

    -- 3. Block creation when the email matches an existing KPBJ user.
    existing <- execQuery (User.getUserByEmail validatedEmail)
    case existing of
      Left dbErr -> throwDatabaseError dbErr
      Right (Just _) ->
        throwValidationError
          "That email belongs to an existing KPBJ account. \
          \Use the user's host-management screen instead."
      Right Nothing -> pure ()

    -- 4. Validate that schedulesJson is non-empty valid JSON
    scheduleData <- validateSchedulesJson (nifSchedulesJson form)

    -- 5. Generate a human-readable invite code (INV-XXXX-XXXX).
    token <- liftIO HostInvitation.generateInviteCode

    -- 6. Insert into host_invitations
    let insertData =
          HostInvitation.Insert
            { HostInvitation.iToken = token,
              HostInvitation.iScheduleData = scheduleData,
              HostInvitation.iRecipientEmail = validatedEmail,
              HostInvitation.iCreatedBy = userMetadata.mUserId
            }

    insertResult <- execQuery (HostInvitation.insert insertData)
    case insertResult of
      Left dbErr -> throwDatabaseError dbErr
      Right Nothing -> throwHandlerFailure "Failed to create invitation."
      Right (Just _invitationId) -> pure ()

    -- 7. Fire-and-forget invitation email. Logs internally on failure;
    --    must not block the redirect.
    lift $ sendHostInvitationEmail validatedEmail (HostInvitation.unToken token)

    -- 8. Build the invite URL for the flash message (manual-fallback copy/paste).
    appBaseUrl <- lift baseUrl
    let inviteUrl = appBaseUrl <> inviteLinkPath token

    Log.logInfo "Created host invitation" (HostInvitation.unToken token)

    -- 9. Redirect to invitation list with success flash.
    let flash =
          FlashMessage
            Success
            "Invitation sent"
            [i|Emailed #{display validatedEmail}. Copy the link if you need to send it manually: #{inviteUrl}|]
    pure $
      Servant.addHeader dashboardInvitationsGetUrl $
        Servant.addHeader (flashCookie (Just flash)) Servant.NoContent

--------------------------------------------------------------------------------

-- | Validate that the schedules JSON is non-empty and valid.
--
-- Returns the parsed Aeson 'Value' for storage in the database.
validateSchedulesJson ::
  Text ->
  ExceptT HandlerError AppM Aeson.Value
validateSchedulesJson schedulesJson
  | Text.null (Text.strip schedulesJson) =
      throwValidationError "Schedule is required. Please add at least one time slot."
  | schedulesJson == "[]" =
      throwValidationError "Schedule is required. Please add at least one time slot."
  | otherwise =
      case Aeson.eitherDecodeStrict (Text.encodeUtf8 schedulesJson) of
        Left err ->
          throwValidationError ("Invalid schedule data: " <> Text.pack err)
        Right val -> pure val

--------------------------------------------------------------------------------

-- | Build the path portion of the invite link for a given token.
inviteLinkPath :: HostInvitation.Token -> Text
inviteLinkPath token =
  rootLink $ onboardGet inviteLinks token
