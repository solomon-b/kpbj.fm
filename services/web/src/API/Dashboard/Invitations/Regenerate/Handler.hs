{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Invitations.Regenerate.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (dashboardInvitationsLinks)
import API.Types
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad.IO.Class (liftIO)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
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
-- Creates a new invitation with the same schedule data as the original.
-- Only staff and above can regenerate invitations. Redirects to the
-- invitation list page with a success banner.
handler ::
  HostInvitation.Id ->
  Maybe Cookie ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler invitationId cookie =
  handleRedirectErrors "Invitation regenerate" dashboardInvitationsLinks.list $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can regenerate invitations." userMetadata

    -- Look up the original invitation
    getResult <- execQuery (HostInvitation.getById invitationId)
    case getResult of
      Left dbErr -> throwDatabaseError dbErr
      Right Nothing -> throwNotFound "Invitation"
      Right (Just inv) -> do
        -- Generate a new human-readable token
        token <- liftIO HostInvitation.generateInviteCode

        -- Create a new invitation with the same schedule data and recipient
        let insertData =
              HostInvitation.Insert
                { HostInvitation.iToken = token,
                  HostInvitation.iScheduleData = inv.hiScheduleData,
                  HostInvitation.iRecipientEmail = inv.hiRecipientEmail,
                  HostInvitation.iCreatedBy = userMetadata.mUserId
                }

        insertResult <- execQuery (HostInvitation.insert insertData)
        case insertResult of
          Left dbErr -> throwDatabaseError dbErr
          Right Nothing -> throwHandlerFailure "Failed to create new invitation."
          Right (Just _newId) -> pure ()

        -- Redirect to the invitation list with a success flash
        let flash =
              FlashMessage
                Success
                "Invitation Regenerated"
                "A new invitation link has been created with the same schedule."
        pure $
          Servant.addHeader dashboardInvitationsGetUrl $
            Servant.addHeader (flashCookie (Just flash)) Servant.NoContent
