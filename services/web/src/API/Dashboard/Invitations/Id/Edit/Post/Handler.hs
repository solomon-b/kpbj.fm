{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Invitations.Id.Edit.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.Get.Templates.Page (renderInvitationRow)
import API.Dashboard.Invitations.Id.Edit.Post.Route (EditInvitationForm (..))
import App.BaseUrl (baseUrl)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Trans (lift)
import Data.List qualified as List
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.EmailAddress (mkEmailAddress)
import Domain.Types.EmailAddress qualified as EmailAddress
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.User qualified as User
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Handler for POST /dashboard/invitations/:invitationId/edit.
--
-- Updates the recipient email of a pending invitation and returns the
-- normal-row fragment plus an OOB success banner.
handler ::
  HostInvitation.Id ->
  Maybe Cookie ->
  EditInvitationForm ->
  AppM (Lucid.Html ())
handler invitationId cookie form =
  handleBannerErrors "Update invitation email" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can edit invitations." userMetadata

    -- Validate new email format.
    let trimmedEmail = Text.strip (eifRecipientEmail form)
    validatedEmail <- case EmailAddress.validate (mkEmailAddress trimmedEmail) of
      Left _ -> throwValidationError "Not a valid email address."
      Right addr -> pure addr

    -- Block when the new email matches an existing KPBJ user.
    existing <- execQuery (User.getUserByEmail validatedEmail)
    case existing of
      Left dbErr -> throwDatabaseError dbErr
      Right (Just _) ->
        throwValidationError
          "That email belongs to an existing KPBJ account. \
          \Use the user's host-management screen instead."
      Right Nothing -> pure ()

    -- Update only if the invitation is still pending.
    updated <-
      fromRightM throwDatabaseError $
        execQuery (HostInvitation.updateRecipientEmail invitationId validatedEmail)
    case updated of
      Nothing -> throwValidationError "Invitation not found or no longer pending."
      Just _ -> pure ()

    -- Re-fetch the joined row so we can render with creator name.
    rows <- fromRightM throwDatabaseError $ execQuery HostInvitation.getAllWithCreator
    case List.find (\r -> r.mwcId == invitationId) rows of
      Nothing -> throwNotFound "Invitation"
      Just rowModel -> do
        appBaseUrl <- lift baseUrl
        pure $ do
          renderInvitationRow appBaseUrl rowModel
          renderBanner Success "Email updated" "Recipient email saved."
