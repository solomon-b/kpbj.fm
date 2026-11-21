{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Invitations.Regenerate.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.Conflict (checkInvitationConflicts)
import API.Invite.Token.Post.Handler (parseInvitationSchedules)
import API.Links (dashboardInvitationsLinks)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Domain.Types.Timezone (pacificDay)
import Effects.Clock (currentSystemTime)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | The URL for the dashboard invitations list page.
dashboardInvitationsGetUrl :: Text
dashboardInvitationsGetUrl =
  let listUrl = Links.linkURI dashboardInvitationsLinks.list
   in [i|/#{listUrl}|]

--------------------------------------------------------------------------------

-- | Handler for POST /dashboard/invitations/:invitationId/regenerate.
--
-- Creates a new invitation with the same schedule data, recipient, and start
-- date as an expired or revoked original. Only staff and above can regenerate
-- invitations. Redirects to the invitation list page with a success banner.
handler ::
  HostInvitation.Id ->
  Satisfies 'UserMetadata.Staff RoleAuth ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler invitationId (Satisfies _ auth) =
  handleRedirectErrors "Invitation regenerate" dashboardInvitationsLinks.list $ do
    _token <- action auth.raMetadata invitationId

    -- Redirect to the invitation list with a success flash
    let flash =
          FlashMessage
            Success
            "Invitation Regenerated"
            "A new invitation link has been created with the same schedule."
    pure $
      Servant.addHeader dashboardInvitationsGetUrl $
        Servant.addHeader (flashCookie (Just flash)) Servant.NoContent

-- | Create a new invitation from an old one, and return the new token.
action ::
  -- | The staff member who regenerates the invitation
  UserMetadata.Model ->
  HostInvitation.Id ->
  ExceptT HandlerError AppM HostInvitation.Token
action userMetadata invitationId = do
  -- Look up the original invitation
  getResult <- execQuery (HostInvitation.getById invitationId)
  inv <- case getResult of
    Left dbErr -> throwDatabaseError dbErr
    Right Nothing -> throwNotFound "Invitation"
    Right (Just inv) -> pure inv

  -- Only a dead link can be regenerated. A live one would leave two links for
  -- one slot.
  case inv.hiStatus of
    HostInvitation.Expired -> pure ()
    HostInvitation.Revoked -> pure ()
    _ -> throwValidationError "Only expired or revoked invitations can be regenerated."

  -- The slot may have been taken since the original invitation was created.
  slot <- either throwValidationError pure (parseInvitationSchedules inv.hiScheduleData)
  today <- pacificDay <$> lift currentSystemTime
  checkInvitationConflicts Nothing slot (max inv.hiStartsOn today)

  -- Generate a new human-readable token
  token <- liftIO HostInvitation.generateInviteCode

  -- Create a new invitation with the same schedule data, recipient, and start date
  let insertData =
        HostInvitation.Insert
          { HostInvitation.iToken = token,
            HostInvitation.iScheduleData = inv.hiScheduleData,
            HostInvitation.iRecipientEmail = inv.hiRecipientEmail,
            HostInvitation.iCreatedBy = userMetadata.mUserId,
            HostInvitation.iStartsOn = inv.hiStartsOn
          }

  insertResult <- execQuery (HostInvitation.insert insertData)
  case insertResult of
    Left dbErr -> throwDatabaseError dbErr
    Right Nothing -> throwHandlerFailure "Failed to create new invitation."
    Right (Just _newId) -> pure token
