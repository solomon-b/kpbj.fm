module API.User.VerifyEmail.Get.Handler where

--------------------------------------------------------------------------------

import API.Links (apiLinks, userLinks)
import API.Types
import App.Auth qualified as Auth
import App.Config (Environment)
import App.Domains qualified as Domains
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie, jsRedirectBody)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Tables.User qualified as User
import Effects.EmailVerification qualified as EmailVerification
import Log qualified
import Lucid qualified
import Network.HTTP.Types qualified
import Network.Socket (SockAddr)
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
  SockAddr ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  AppM (Lucid.Html ())
handler sockAddr mUserAgent _hxRequest mToken = do
  case mToken of
    Nothing ->
      redirectWithFlash verifyEmailSentUrl [] $
        FlashMessage Error "Verification Failed" "No verification token provided."
    Just token -> do
      result <- EmailVerification.verifyEmail token
      case result of
        Left err ->
          redirectWithFlash verifyEmailSentUrl [] $
            FlashMessage Error "Verification Failed" (EmailVerification.verificationErrorToText err)
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
              redirectWithFlash loginUrl [] $
                FlashMessage Success "Email Verified" "Your email has been verified. Please log in."
            Right sessionCookie ->
              redirectWithFlash rootUrl [("Set-Cookie", Text.Encoding.encodeUtf8 sessionCookie)] $
                FlashMessage Success "Welcome!" "Your email has been verified and you are now logged in."

-- | Redirect with flash message and optional extra headers (e.g. session cookie).
redirectWithFlash :: Text -> [Network.HTTP.Types.Header] -> FlashMessage -> AppM a
redirectWithFlash url extraHeaders flash =
  throwM $
    Servant.ServerError
      { errHTTPCode = 200,
        errReasonPhrase = "OK",
        errBody = Lucid.renderBS $ jsRedirectBody url,
        errHeaders =
          [ ("Content-Type", "text/html; charset=utf-8"),
            ("HX-Redirect", Text.Encoding.encodeUtf8 url),
            ("Set-Cookie", Text.Encoding.encodeUtf8 $ flashCookie (Just flash))
          ]
            <> extraHeaders
      }

--------------------------------------------------------------------------------

-- | Attempt to create a session for auto-login after email verification.
attemptAutoLogin ::
  User.Id ->
  SockAddr ->
  Maybe Text ->
  AppM (Either Text Text)
attemptAutoLogin userId sockAddr mUserAgent = do
  env <- asks (Has.getter @Environment)
  -- Always create a fresh session (upstream insertServerSession caps at 5 per user)
  Auth.login userId sockAddr mUserAgent >>= \case
    Left err -> do
      pure $ Left $ Text.pack $ show err
    Right sessionId -> do
      let newCookie = Auth.mkCookieSession env (Domains.cookieDomainMaybe env) sessionId
      pure $ Right newCookie
