{-# LANGUAGE ViewPatterns #-}

module API.Invite.Token.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Invite.Token.Get.Templates.ErrorPage qualified as ErrorPage
import API.Invite.Token.Get.Templates.Page qualified as Page
import App.Common (getUserInfo, renderUnauthTemplate)
import App.Monad (AppM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Lucid qualified

--------------------------------------------------------------------------------

-- | Handler for GET /invite/:token.
--
-- Renders the host onboarding form page for a valid invitation token.
-- Shows an error page if the token is invalid, expired, or already claimed.
-- Shows an "already logged in" message if the user has a session.
handler ::
  -- | Invitation token from URL
  HostInvitation.Token ->
  -- | Session cookie
  Maybe Cookie ->
  -- | HTMX request header
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler token cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie
  case mUserInfo of
    Just _ ->
      renderUnauthTemplate hxRequest Page.alreadyLoggedInTemplate
    Nothing -> do
      result <- execQuery (HostInvitation.getByToken token)
      case result of
        Left _ ->
          renderUnauthTemplate hxRequest ErrorPage.template
        Right Nothing ->
          renderUnauthTemplate hxRequest ErrorPage.template
        Right (Just invitation) ->
          renderUnauthTemplate hxRequest (Page.template token invitation)
