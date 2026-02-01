module API.User.VerifyEmailResend.Post.Handler where

--------------------------------------------------------------------------------

import API.User.VerifyEmailResend.Post.Route (ResendForm (..))
import App.Monad (AppM)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.User qualified as User
import Effects.EmailVerification qualified as EmailVerification
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  ResendForm ->
  AppM (Lucid.Html ())
handler ResendForm {..} = do
  -- Look up the user by email
  result <- execQuery (User.getUserByEmail rfEmail)
  case result of
    Left err -> do
      Log.logInfo "Failed to look up user for resend" (show err)
      pure $ errorMessage "An error occurred. Please try again."
    Right Nothing -> do
      -- Don't reveal whether the email exists
      Log.logInfo "Resend requested for unknown email" (display rfEmail)
      pure successMessage
    Right (Just user) -> do
      -- Check if already verified
      -- Note: This depends on the User model having an emailVerified field
      -- For now, we'll just resend regardless
      resendResult <- EmailVerification.resendVerification (User.mId user) rfEmail
      case resendResult of
        Left err -> do
          Log.logInfo "Failed to resend verification" (EmailVerification.verificationErrorToText err)
          pure $ errorMessage "Failed to send verification email. Please try again."
        Right () -> do
          Log.logInfo "Verification email resent" (display rfEmail)
          pure successMessage

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
