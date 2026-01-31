{-# LANGUAGE QuasiQuotes #-}

module API.User.VerifyEmailSent.Get.Templates.Page (template) where

--------------------------------------------------------------------------------

import API.Links (userLinks)
import API.Types (UserRoutes (loginGet, verifyEmailResendPost))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
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
  Lucid.div_ [Lucid.class_ "max-w-md w-full space-y-8 bg-[var(--theme-bg)] p-8"] $ do
    -- Email icon
    Lucid.div_ [Lucid.class_ "text-center"] $ do
      Lucid.div_ [Lucid.class_ "mx-auto h-16 w-16 flex items-center justify-center rounded-full bg-[var(--theme-info-bg)]"] $ do
        Lucid.span_ [Lucid.class_ "text-[var(--theme-info-text)] text-3xl"] "✉"

    -- Title
    Lucid.h1_
      [Lucid.class_ "mt-6 text-center text-2xl font-bold text-[var(--theme-fg)]"]
      "Check Your Email"

    -- Message
    Lucid.div_ [Lucid.class_ "mt-4 text-center text-[var(--theme-fg-muted)]"] $ do
      case mEmail of
        Nothing -> do
          Lucid.p_ "We've sent you a verification email."
        Just email -> do
          Lucid.p_ $ do
            "We've sent a verification email to:"
          Lucid.p_ [Lucid.class_ "font-semibold text-[var(--theme-fg)] mt-2"] $
            Lucid.toHtml (display email)

    Lucid.p_
      [Lucid.class_ "mt-4 text-center text-[var(--theme-fg-muted)]"]
      "Please click the link in the email to verify your account."

    -- Instructions
    Lucid.div_ [Lucid.class_ "mt-6 p-4 bg-[var(--theme-bg-alt)] border border-[var(--theme-border-muted)]"] $ do
      Lucid.h3_ [Lucid.class_ "font-semibold text-[var(--theme-fg)] text-sm"] "Didn't receive the email?"
      Lucid.ul_ [Lucid.class_ "mt-2 text-sm text-[var(--theme-fg-muted)] space-y-1"] $ do
        Lucid.li_ "• Check your spam or junk folder"
        Lucid.li_ "• Make sure your email address is correct"
        Lucid.li_ "• Wait a few minutes and check again"

    -- Resend button (only show if we have the email)
    case mEmail of
      Nothing -> pure ()
      Just email -> do
        Lucid.div_ [Lucid.id_ "resend-container", Lucid.class_ "mt-6"] $ do
          Lucid.form_
            [ Lucid.class_ "space-y-4",
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
                  Lucid.class_ "w-full flex justify-center py-3 px-4 border-2 border-[var(--theme-border)] text-sm font-bold text-[var(--theme-fg)] bg-[var(--theme-bg)] hover:bg-[var(--theme-hover-bg)] transition-colors"
                ]
                "Resend Verification Email"

    -- Login link
    Lucid.p_ [Lucid.class_ "mt-6 text-center text-sm text-[var(--theme-fg-muted)]"] $ do
      "Already verified? "
      Lucid.a_
        [ Lucid.href_ loginUrl,
          Lucid.class_ "font-semibold text-[var(--theme-fg)] hover:underline"
        ]
        "Log in"
