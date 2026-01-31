{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Template for the forgot password page.
module API.User.ForgotPassword.Get.Templates.Page
  ( template,
    successTemplate,
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
import Lucid qualified
import Lucid.Form.Builder
import Lucid.HTMX
import Servant.Links qualified as Link

--------------------------------------------------------------------------------

userForgotPasswordPostUrl :: Link.URI
userForgotPasswordPostUrl = Link.linkURI userLinks.forgotPasswordPost

userLoginGetUrl :: Link.URI
userLoginGetUrl = Link.linkURI (userLinks.loginGet Nothing Nothing)

--------------------------------------------------------------------------------

-- | Main forgot password form template.
template :: Lucid.Html ()
template =
  let postUrl = [i|/#{userForgotPasswordPostUrl}|]
   in Lucid.div_ [class_ $ base [Tokens.fullWidth, "max-w-md", "mx-auto"]] $ do
        Lucid.div_ [class_ $ base [Tokens.bgMain, "p-8"]] $ do
          pageHeader "FORGOT PASSWORD"

          -- Instructions
          Lucid.p_
            [class_ $ base [Tokens.textSm, Tokens.fgMuted, "mb-6"]]
            "Enter your email address and we'll send you a link to reset your password."

          -- Forgot password form using form builder
          renderForm (forgotPasswordFormConfig postUrl) forgotPasswordForm

          -- Back to login
          loginSection

--------------------------------------------------------------------------------

-- | Success template shown after form submission.
--
-- Always shows the same message regardless of whether the email exists.
-- This prevents email enumeration attacks.
successTemplate :: Lucid.Html ()
successTemplate =
  Lucid.div_ [class_ $ base [Tokens.fullWidth, "max-w-md", "mx-auto"]] $ do
    Lucid.div_ [class_ $ base [Tokens.bgMain, "p-8"]] $ do
      -- Success icon
      Lucid.div_ [Lucid.class_ "text-center"] $ do
        Lucid.div_ [Lucid.class_ "mx-auto h-16 w-16 flex items-center justify-center rounded-full bg-green-100"] $ do
          Lucid.span_ [Lucid.class_ "text-green-600 text-3xl"] "✓"

      -- Title
      Lucid.h1_
        [class_ $ base ["mt-6", "text-center", "text-2xl", Tokens.fontBold, "text-gray-900"]]
        "Check Your Email"

      -- Message
      Lucid.div_ [class_ $ base ["mt-4", "text-center", Tokens.fgMuted]] $ do
        Lucid.p_ "If an account exists with that email address, we've sent you a link to reset your password."

      Lucid.p_
        [class_ $ base ["mt-4", "text-center", Tokens.fgMuted]]
        "The link will expire in 1 hour."

      -- Instructions
      Lucid.div_ [class_ $ base ["mt-6", Tokens.p4, Tokens.bgAlt, "border", "border-gray-200"]] $ do
        Lucid.h3_ [class_ $ base [Tokens.fontBold, "text-gray-900", Tokens.textSm]] "Didn't receive the email?"
        Lucid.ul_ [class_ $ base ["mt-2", Tokens.textSm, Tokens.fgMuted, "space-y-1"]] $ do
          Lucid.li_ "• Check your spam or junk folder"
          Lucid.li_ "• Make sure your email address is correct"
          Lucid.li_ "• Wait a few minutes and check again"

      -- Login link
      Lucid.p_ [class_ $ base ["mt-6", "text-center", Tokens.textSm, Tokens.fgMuted]] $ do
        "Remember your password? "
        Lucid.a_
          [ Lucid.href_ [i|/#{userLoginGetUrl}|],
            hxGet_ [i|/#{userLoginGetUrl}|],
            hxSwap_ "innerHTML",
            hxTarget_ "body",
            hxPushUrl_ "true",
            class_ $ base [Tokens.fontBold, "text-gray-900", "hover:underline"]
          ]
          "Log in"

--------------------------------------------------------------------------------
-- Form Configuration

forgotPasswordFormConfig :: Text -> FormConfig
forgotPasswordFormConfig postUrl =
  defaultFormConfig
    { fcAction = postUrl,
      fcMethod = "post",
      fcHtmxTarget = Just "body"
    }

--------------------------------------------------------------------------------
-- Form Definition

forgotPasswordForm :: FormBuilder
forgotPasswordForm = do
  textField "email" $ do
    label "Email Address"
    placeholder "your@email.com"
    hint "Enter the email address associated with your account"
    required

  submitButton "SEND RESET LINK"

--------------------------------------------------------------------------------
-- Custom Components

loginSection :: Lucid.Html ()
loginSection =
  Lucid.div_ [class_ $ base ["mt-8", "pt-6", "border-t", "border-gray-300", "text-center"]] do
    Lucid.a_
      [ Lucid.href_ [i|/#{userLoginGetUrl}|],
        hxGet_ [i|/#{userLoginGetUrl}|],
        hxSwap_ "innerHTML",
        hxTarget_ "body",
        hxPushUrl_ "true",
        class_ $ base [Tokens.textSm, "text-gray-900", "hover:underline"]
      ]
      "← Back to login"
