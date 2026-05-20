{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Invitations.Id.Resend.Post.Handler (handler) where

--------------------------------------------------------------------------------

import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Trans (lift)
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
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
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler invitationId cookie =
  handleBannerErrors "Resend invitation" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can resend invitations." userMetadata
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
