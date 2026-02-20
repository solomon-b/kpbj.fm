module API.User.VerifyEmailResend.Post.Handler where

--------------------------------------------------------------------------------

import API.User.VerifyEmailResend.Post.Route (ResendForm (..))
import App.Handler.Error (HandlerError)
import App.Monad (AppM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.User qualified as User
import Effects.EmailVerification qualified as EmailVerification
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

-- | Result of the resend verification action.
data ResendResult
  = ResendSuccess
  | ResendError Text

--------------------------------------------------------------------------------

-- | Core business logic for resending a verification email.
--
-- Looks up the user by email and resends the verification email if found.
-- Returns success regardless of whether the email exists (prevents enumeration).
action ::
  ResendForm ->
  ExceptT HandlerError AppM ResendResult
action ResendForm {..} = do
  result <- execQuery (User.getUserByEmail rfEmail)
  case result of
    Left err -> do
      Log.logInfo "Failed to look up user for resend" (show err)
      pure $ ResendError "An error occurred. Please try again."
    Right Nothing -> do
      Log.logInfo "Resend requested for unknown email" (display rfEmail)
      pure ResendSuccess
    Right (Just user) -> do
      resendResult <- lift $ EmailVerification.resendVerification (User.mId user) rfEmail
      case resendResult of
        Left err -> do
          Log.logInfo "Failed to resend verification" (EmailVerification.verificationErrorToText err)
          pure $ ResendError "Failed to send verification email. Please try again."
        Right () -> do
          Log.logInfo "Verification email resent" (display rfEmail)
          pure ResendSuccess

handler ::
  ResendForm ->
  AppM (Lucid.Html ())
handler form = do
  result <- runExceptT $ action form
  case result of
    Left _ -> pure $ errorMessage "An unexpected error occurred. Please try again."
    Right ResendSuccess -> pure successMessage
    Right (ResendError msg) -> pure $ errorMessage msg

--------------------------------------------------------------------------------

-- | Success message fragment returned to HTMX.
successMessage :: Lucid.Html ()
successMessage =
  Lucid.div_ [Lucid.class_ "p-4 bg-green-50 border border-green-200 text-green-800"] $ do
    Lucid.p_ [Lucid.class_ "font-semibold"] "Verification email sent!"
    Lucid.p_ [Lucid.class_ "text-sm mt-1"] "Please check your inbox and spam folder."

-- | Error message fragment returned to HTMX.
errorMessage :: Text -> Lucid.Html ()
errorMessage msg =
  Lucid.div_ [Lucid.class_ "p-4 bg-red-50 border border-red-200 text-red-800"] $ do
    Lucid.p_ [Lucid.class_ "font-semibold"] "Error"
    Lucid.p_ [Lucid.class_ "text-sm mt-1"] $ Lucid.toHtml msg
