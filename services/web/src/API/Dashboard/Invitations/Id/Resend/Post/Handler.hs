{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Invitations.Id.Resend.Post.Handler (handler) where

--------------------------------------------------------------------------------

import App.Auth.Roles (RoleAuth, Satisfies)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Trans (lift)
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Effects.HostInvitationEmail (sendHostInvitationEmail)
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Handler for POST /dashboard/invitations/:invitationId/resend.
--
-- Re-sends the invitation email using the existing token. No DB change.
-- Returns an OOB success banner.
handler ::
  HostInvitation.Id ->
  Satisfies 'Staff RoleAuth ->
  AppM (Lucid.Html ())
handler invitationId _ =
  handleBannerErrors "Resend invitation" $ do
    inv <- fromRightM throwDatabaseError $ execQuery (HostInvitation.getById invitationId)
    case inv of
      Nothing -> throwNotFound "Invitation"
      Just m
        | m.hiStatus /= HostInvitation.Pending ->
            throwValidationError "Only pending invitations can be resent."
        | otherwise -> do
            let recipient = m.hiRecipientEmail
                tokenText = HostInvitation.unToken m.hiToken
            lift $ sendHostInvitationEmail recipient tokenText
            pure $ do
              mempty
              renderBanner Success "Resent" [i|Re-sent invitation to #{display recipient}.|]
