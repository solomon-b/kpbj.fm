{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Invite.Token.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Invite.Token.Get.Templates.ErrorPage qualified as ErrorPage
import API.Invite.Token.Get.Templates.Page qualified as Page
import App.Common (getUserInfo, renderUnauthTemplate)
import App.Monad (AppM)
import Component.Flash (FlashMessage)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

--------------------------------------------------------------------------------

-- | Handler for GET /invite/:token.
--
-- Renders the host onboarding form page for a valid invitation token.
-- Shows an error page if the token is invalid, expired, or already claimed.
--
-- A logged-in user cannot claim an invitation, because the claim creates a
-- new account. Staff and admins see the invitation details in any status, so
-- that they can check the link. Other logged-in users see that the
-- invitation is not claimed and that they must log out to claim it.
handler ::
  -- | Invitation token from URL
  HostInvitation.Token ->
  -- | Session cookie
  Maybe Cookie ->
  -- | Flash message of the request
  Maybe FlashMessage ->
  -- | HTMX request header
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler token cookie mFlash (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie
  case mUserInfo of
    Just (_, userMetadata)
      | UserMetadata.isStaffOrHigher userMetadata.mUserRole -> do
          result <- execQuery (HostInvitation.getByTokenWithCreator token)
          case result of
            Right (Just invitation) ->
              renderUnauthTemplate hxRequest mFlash (Page.staffPreviewTemplate invitation)
            _ ->
              renderUnauthTemplate hxRequest mFlash ErrorPage.template
    Just _ -> do
      result <- execQuery (HostInvitation.getByToken token)
      case result of
        Right (Just invitation) ->
          renderUnauthTemplate hxRequest mFlash (Page.alreadyLoggedInTemplate invitation.hiRecipientEmail)
        _ ->
          renderUnauthTemplate hxRequest mFlash ErrorPage.template
    Nothing -> do
      result <- execQuery (HostInvitation.getByToken token)
      case result of
        Left _ ->
          renderUnauthTemplate hxRequest mFlash ErrorPage.template
        Right Nothing ->
          renderUnauthTemplate hxRequest mFlash ErrorPage.template
        Right (Just invitation) ->
          renderUnauthTemplate hxRequest mFlash (Page.template token invitation)
