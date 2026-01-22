module API.User.VerifyEmail.Get.Handler where

--------------------------------------------------------------------------------

import API.Links (apiLinks, userLinks)
import API.Types
import App.Auth qualified as Auth
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.HxRedirect (HxRedirect (..))
import Domain.Types.SetCookie (SetCookie (..))
import Effects.Database.Execute (execQuerySpanThrow)
import Effects.Database.Tables.ServerSessions qualified as Session
import Effects.Database.Tables.User qualified as User
import Effects.EmailVerification qualified as EmailVerification
import Log qualified
import Lucid qualified
import Network.Socket (SockAddr)
import OpenTelemetry.Trace qualified as Trace
import Servant qualified
import Web.HttpApiData qualified as Http

--------------------------------------------------------------------------------

verifyEmailSentUrl :: Text
verifyEmailSentUrl = "/" <> Http.toUrlPiece (userLinks.verifyEmailSentGet (Nothing :: Maybe EmailAddress))

rootUrl :: Text
rootUrl = "/" <> Http.toUrlPiece apiLinks.rootGet

--------------------------------------------------------------------------------

-- | Handle email verification requests with auto-login.
--
-- On successful verification:
-- 1. Verifies the token and marks email as verified
-- 2. Creates a session for the user (auto-login)
-- 3. Redirects to home page with session cookie set
--
-- On failure:
-- - Redirects to verify-email-sent page with error banner
handler ::
  Trace.Tracer ->
  SockAddr ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  AppM
    ( Servant.Headers
        '[ Servant.Header "Set-Cookie" SetCookie,
           Servant.Header "HX-Redirect" HxRedirect
         ]
        (Lucid.Html ())
    )
handler _tracer sockAddr mUserAgent _hxRequest mToken = do
  case mToken of
    Nothing ->
      pure $
        Servant.noHeader $
          Servant.addHeader (HxRedirect verifyEmailSentUrl) $
            redirectWithBanner verifyEmailSentUrl $
              BannerParams Error "Verification Failed" "No verification token provided."
    Just token -> do
      result <- EmailVerification.verifyEmail token
      case result of
        Left err ->
          pure $
            Servant.noHeader $
              Servant.addHeader (HxRedirect verifyEmailSentUrl) $
                redirectWithBanner verifyEmailSentUrl $
                  BannerParams Error "Verification Failed" (EmailVerification.verificationErrorToText err)
        Right (userId, email) -> do
          -- Auto-login: Create session for the verified user
          Log.logInfo "Email verified, creating session for auto-login" email
          loginResult <- attemptAutoLogin userId sockAddr mUserAgent
          case loginResult of
            Left errMsg -> do
              -- Session creation failed, but email is verified
              -- Redirect to login page so user can log in manually
              Log.logInfo "Auto-login failed after email verification" errMsg
              let loginUrl = "/" <> Http.toUrlPiece (userLinks.loginGet (Nothing :: Maybe Text) (Nothing :: Maybe EmailAddress))
              pure $
                Servant.noHeader $
                  Servant.addHeader (HxRedirect loginUrl) $
                    redirectWithBanner loginUrl $
                      BannerParams Success "Email Verified" "Your email has been verified. Please log in."
            Right sessionCookie ->
              -- Success: Set cookie and redirect to home
              pure $
                Servant.addHeader sessionCookie $
                  Servant.addHeader (HxRedirect rootUrl) $
                    redirectWithBanner rootUrl $
                      BannerParams Success "Welcome!" "Your email has been verified and you are now logged in."

--------------------------------------------------------------------------------

-- | Attempt to create a session for auto-login after email verification.
--
-- Returns the session cookie on success, or an error message on failure.
attemptAutoLogin ::
  User.Id ->
  SockAddr ->
  Maybe Text ->
  AppM (Either Text SetCookie)
attemptAutoLogin userId sockAddr mUserAgent = do
  -- Check if user already has an active session
  execQuerySpanThrow (Session.getServerSessionByUser userId) >>= \case
    Just session -> do
      -- Reuse existing session
      let sessionId = Session.mSessionId session
      pure $ Right $ SetCookie $ Auth.mkCookieSession sessionId
    Nothing -> do
      -- Create new session
      Auth.login userId sockAddr mUserAgent >>= \case
        Left err -> do
          pure $ Left $ Text.pack $ show err
        Right sessionId -> do
          pure $ Right $ SetCookie $ Auth.mkCookieSession sessionId
