{-# LANGUAGE QuasiQuotes #-}

module API.User.VerifyEmail.Get.Handler where

--------------------------------------------------------------------------------

import API.Links (userLinks)
import API.Types
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Domain.Types.EmailAddress (EmailAddress)
import Effects.EmailVerification qualified as EmailVerification
import Lucid qualified
import OpenTelemetry.Trace qualified as Trace
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

userLoginGetUri :: Links.URI
userLoginGetUri = Links.linkURI (userLinks.loginGet Nothing Nothing)

verifyEmailSentGetUri :: Maybe EmailAddress -> Links.URI
verifyEmailSentGetUri mEmail = Links.linkURI (userLinks.verifyEmailSentGet mEmail)

loginUrl :: Text
loginUrl = [i|/#{userLoginGetUri}|]

verifyEmailSentUrl :: Maybe EmailAddress -> Text
verifyEmailSentUrl mEmail = [i|/#{verifyEmailSentGetUri mEmail}|]

--------------------------------------------------------------------------------

handler ::
  Trace.Tracer ->
  Maybe Text ->
  Maybe Text ->
  AppM (Lucid.Html ())
handler _tracer _hxRequest mToken = do
  case mToken of
    Nothing ->
      pure $
        redirectWithBanner (verifyEmailSentUrl Nothing) $
          BannerParams Error "Verification Failed" "No verification token provided."
    Just token -> do
      result <- EmailVerification.verifyEmail token
      case result of
        Left err ->
          pure $
            redirectWithBanner (verifyEmailSentUrl Nothing) $
              BannerParams Error "Verification Failed" (EmailVerification.verificationErrorToText err)
        Right _email ->
          pure $
            redirectWithBanner loginUrl $
              BannerParams Success "Email Verified" "Your email has been verified. You can now log in."
