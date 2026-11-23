{-# LANGUAGE QuasiQuotes #-}

module API.User.VerifyEmailSent.Get.Templates.Page (template) where

--------------------------------------------------------------------------------

import API.Links (userLinks)
import API.Types (UserRoutes (loginGet, verifyEmailResendPost))
import Component.Card qualified as Card
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.EmailAddress (EmailAddress)
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | URI for the login page.
userLoginGetUri :: Links.URI
userLoginGetUri = Links.linkURI (userLinks.loginGet Nothing Nothing)

-- | URI for resending verification email.
verifyEmailResendPostUri :: Links.URI
verifyEmailResendPostUri = Links.linkURI userLinks.verifyEmailResendPost

-- | URL for the login page.
loginUrl :: Text
loginUrl = [i|/#{userLoginGetUri}|]

-- | URL for resending verification email.
resendUrl :: Text
resendUrl = [i|/#{verifyEmailResendPostUri}|]

--------------------------------------------------------------------------------

-- | Template for the "check your email" page.
template :: Maybe EmailAddress -> Lucid.Html ()
template mEmail = do
  Card.card Card.Auth ["max-w-md", "w-full", "space-y-8"] $ do
    -- Email icon
    Lucid.div_ [class_ $ base ["text-center"]] $ do
      Lucid.div_ [class_ $ base ["mx-auto", "h-16", "w-16", "flex", "items-center", "justify-center", "rounded-full", Tokens.infoBg]] $ do
        Lucid.span_ [class_ $ base [Tokens.infoText, Tokens.text3xl]] "✉"

    -- Title
    Lucid.h1_
      [class_ $ base [Tokens.mt6, "text-center", Tokens.text2xl, Tokens.fontBold, Tokens.fgPrimary]]
      "Check Your Email"

    -- Message
    Lucid.div_ [class_ $ base [Tokens.mt4, "text-center", Tokens.fgMuted]] $ do
      case mEmail of
        Nothing -> do
          Lucid.p_ "We've sent you a verification email."
        Just email -> do
          Lucid.p_ $ do
            "We've sent a verification email to:"
          Lucid.p_ [class_ $ base [Tokens.fontSemibold, Tokens.fgPrimary, Tokens.mt2]] $
            Lucid.toHtml (display email)

    Lucid.p_
      [class_ $ base [Tokens.mt4, "text-center", Tokens.fgMuted]]
      "Please click the link in the email to verify your account."

    -- Instructions
    Lucid.div_ [class_ $ base [Tokens.mt6, Tokens.p4, Tokens.bgAlt, "border", Tokens.borderMuted]] $ do
      Lucid.h3_ [class_ $ base [Tokens.fontSemibold, Tokens.fgPrimary, Tokens.textSm]] "Didn't receive the email?"
      Lucid.ul_ [class_ $ base [Tokens.mt2, Tokens.textSm, Tokens.fgMuted, "space-y-1"]] $ do
        Lucid.li_ "• Check your spam or junk folder"
        Lucid.li_ "• Make sure your email address is correct"
        Lucid.li_ "• Wait a few minutes and check again"

    -- Resend button (only show if we have the email)
    case mEmail of
      Nothing -> pure ()
      Just email -> do
        Lucid.div_ [Lucid.id_ "resend-container", class_ $ base [Tokens.mt6]] $ do
          Lucid.form_
            [ class_ $ base ["space-y-4"],
              hxPost_ resendUrl,
              hxTarget_ "#resend-container",
              hxSwap_ "innerHTML"
            ]
            $ do
              Lucid.input_
                [ Lucid.type_ "hidden",
                  Lucid.name_ "email",
                  Lucid.value_ (display email)
                ]
              Lucid.button_
                [ Lucid.type_ "submit",
                  class_ $ base [Tokens.fullWidth, "flex", "justify-center", "py-3", Tokens.px4, Tokens.border2, Tokens.borderDefault, Tokens.textSm, Tokens.fontBold, Tokens.fgPrimary, Tokens.bgMain, Tokens.hoverBg, "transition-colors"]
                ]
                "Resend Verification Email"

    -- Login link
    Lucid.p_ [class_ $ base [Tokens.mt6, "text-center", Tokens.textSm, Tokens.fgMuted]] $ do
      "Already verified? "
      Lucid.a_
        [ Lucid.href_ loginUrl,
          class_ $ base [Tokens.fontSemibold, Tokens.fgPrimary, "hover:underline"]
        ]
        "Log in"
