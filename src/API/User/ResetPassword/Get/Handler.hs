{-# LANGUAGE ViewPatterns #-}

-- | Handler for GET /user/reset-password
--
-- Shows the password reset form if the token is valid.
-- Shows an error page if the token is invalid, expired, or already used.
module API.User.ResetPassword.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.User.ResetPassword.Get.Templates.InvalidToken qualified as InvalidTokenTemplate
import API.User.ResetPassword.Get.Templates.Page qualified as Templates
import App.Monad (AppM)
import Component.Frame (loadContentOnly, loadFrame)
import Control.Monad.Reader (asks)
import Data.Has (getter)
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Tables.PasswordResetTokens (Token)
import Effects.PasswordReset qualified as PasswordReset
import Lucid qualified
import OpenTelemetry.Trace qualified as Trace

--------------------------------------------------------------------------------

handler ::
  Trace.Tracer ->
  Maybe HxRequest ->
  Maybe Token ->
  AppM (Lucid.Html ())
handler _tracer (foldHxReq -> hxRequest) mToken = do
  mGoogleAnalyticsId <- asks getter

  case mToken of
    Nothing ->
      -- No token provided, show invalid token page
      renderInvalidToken mGoogleAnalyticsId hxRequest
    Just token -> do
      -- Validate the token (without consuming it)
      validationResult <- PasswordReset.validateToken token
      case validationResult of
        Left _err ->
          -- Token is invalid, expired, or already used
          renderInvalidToken mGoogleAnalyticsId hxRequest
        Right _tokenModel ->
          -- Token is valid, show the password reset form
          renderResetForm mGoogleAnalyticsId hxRequest token

--------------------------------------------------------------------------------

-- | Render the password reset form.
renderResetForm ::
  Maybe GoogleAnalyticsId ->
  HxRequest ->
  Token ->
  AppM (Lucid.Html ())
renderResetForm mGoogleAnalyticsId hxRequest token = do
  let content = Templates.template token Nothing
  case hxRequest of
    IsHxRequest -> loadContentOnly content
    IsNotHxRequest -> loadFrame mGoogleAnalyticsId content

-- | Render the invalid token page.
renderInvalidToken ::
  Maybe GoogleAnalyticsId ->
  HxRequest ->
  AppM (Lucid.Html ())
renderInvalidToken mGoogleAnalyticsId hxRequest = do
  let content = InvalidTokenTemplate.template
  case hxRequest of
    IsHxRequest -> loadContentOnly content
    IsNotHxRequest -> loadFrame mGoogleAnalyticsId content
