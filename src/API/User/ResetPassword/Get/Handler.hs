{-# LANGUAGE ViewPatterns #-}

-- | Handler for GET /user/reset-password
--
-- Shows the password reset form if the token is valid.
-- Shows an error page if the token is invalid, expired, or already used.
module API.User.ResetPassword.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.User.ResetPassword.Get.Templates.InvalidToken qualified as InvalidTokenTemplate
import API.User.ResetPassword.Get.Templates.Page qualified as Templates
import App.Common (renderUnauthTemplate)
import App.Monad (AppM)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Tables.PasswordResetTokens (Token)
import Effects.PasswordReset qualified as PasswordReset
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  Maybe HxRequest ->
  Maybe Token ->
  AppM (Lucid.Html ())
handler (foldHxReq -> hxRequest) mToken = do
  case mToken of
    Nothing ->
      -- No token provided, show invalid token page
      renderInvalidToken hxRequest
    Just token -> do
      -- Validate the token (without consuming it)
      validationResult <- PasswordReset.validateToken token
      case validationResult of
        Left _err ->
          -- Token is invalid, expired, or already used
          renderInvalidToken hxRequest
        Right _tokenModel ->
          -- Token is valid, show the password reset form
          renderResetForm hxRequest token

--------------------------------------------------------------------------------

-- | Render the password reset form.
renderResetForm ::
  HxRequest ->
  Token ->
  AppM (Lucid.Html ())
renderResetForm hxRequest token = do
  let content = Templates.template token Nothing
  renderUnauthTemplate hxRequest content

-- | Render the invalid token page.
renderInvalidToken ::
  HxRequest ->
  AppM (Lucid.Html ())
renderInvalidToken hxRequest = do
  let content = InvalidTokenTemplate.template
  renderUnauthTemplate hxRequest content
