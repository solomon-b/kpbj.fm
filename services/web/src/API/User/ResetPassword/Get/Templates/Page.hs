{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Template for the password reset page (new password form).
module API.User.ResetPassword.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (userLinks)
import API.Types
import Component.PageHeader (pageHeader)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.PasswordResetTokens (Token (..))
import Lucid qualified
import Lucid.Form.Builder
import Servant.Links qualified as Link

--------------------------------------------------------------------------------

userResetPasswordPostUrl :: Link.URI
userResetPasswordPostUrl = Link.linkURI userLinks.resetPasswordPost

--------------------------------------------------------------------------------

-- | Password reset form template.
--
-- Shows a form for the user to enter their new password.
-- The token is included as a hidden field.
template :: Token -> Maybe Text -> Lucid.Html ()
template token mError =
  let postUrl = [i|/#{userResetPasswordPostUrl}|]
   in Lucid.div_
        [class_ $ base [Tokens.fullWidth, "max-w-md", "mx-auto"]]
        do
          Lucid.div_ [class_ $ base [Tokens.bgMain, "p-8"]] do
            pageHeader "RESET PASSWORD"

            -- Error alert (shown when validation failed)
            maybe mempty errorAlert mError

            -- Instructions
            Lucid.p_
              [class_ $ base [Tokens.textSm, Tokens.fgMuted, "mb-6"]]
              "Enter your new password below."

            -- Reset password form using form builder
            renderForm (resetPasswordFormConfig postUrl) (resetPasswordForm token)

--------------------------------------------------------------------------------
-- Form Configuration

resetPasswordFormConfig :: Text -> FormConfig
resetPasswordFormConfig postUrl =
  defaultFormConfig
    { fcAction = postUrl,
      fcMethod = "post",
      fcHtmxTarget = Just "body"
    }

--------------------------------------------------------------------------------
-- Form Definition

resetPasswordForm :: Token -> FormBuilder
resetPasswordForm token = do
  -- Hidden token field
  hidden "token" (unToken token)

  passwordField "password" $ do
    label "New Password"
    placeholder "Enter your new password"
    required

  passwordField "passwordConfirm" $ do
    label "Confirm New Password"
    placeholder "Confirm your new password"
    required

  -- Password requirements info
  plain passwordRequirements

  submitButton "RESET PASSWORD"

--------------------------------------------------------------------------------
-- Custom Components

passwordRequirements :: Lucid.Html ()
passwordRequirements =
  Lucid.div_ [class_ $ base [Tokens.bgAlt, "border", Tokens.borderMuted, Tokens.p3, Tokens.textXs, "mb-4"]] do
    Lucid.div_ [class_ $ base [Tokens.fontBold, "mb-1"]] "Password Requirements:"
    Lucid.div_ "• At least 8 characters long"
    Lucid.div_ "• Include uppercase and lowercase letters"
    Lucid.div_ "• Include at least one number"

errorAlert :: Text -> Lucid.Html ()
errorAlert message =
  Lucid.div_
    [class_ $ base [Tokens.p4, Tokens.mb4, Tokens.textSm, Tokens.errorText, "rounded-lg", Tokens.errorBg], Lucid.role_ "alert"]
    (Lucid.toHtml message)
