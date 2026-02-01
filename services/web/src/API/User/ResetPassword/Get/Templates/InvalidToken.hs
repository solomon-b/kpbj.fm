{-# LANGUAGE QuasiQuotes #-}

-- | Template for the invalid/expired token page.
module API.User.ResetPassword.Get.Templates.InvalidToken
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (userLinks)
import API.Types
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Link

--------------------------------------------------------------------------------

userForgotPasswordGetUrl :: Link.URI
userForgotPasswordGetUrl = Link.linkURI userLinks.forgotPasswordGet

userLoginGetUrl :: Link.URI
userLoginGetUrl = Link.linkURI (userLinks.loginGet Nothing Nothing)

--------------------------------------------------------------------------------

-- | Invalid token template.
--
-- Shows when the reset token is invalid, expired, or already used.
template :: Lucid.Html ()
template =
  Lucid.div_ [class_ $ base [Tokens.fullWidth, "max-w-md", "mx-auto"]] $ do
    Lucid.div_ [class_ $ base [Tokens.bgMain, "p-8"]] $ do
      -- Warning icon
      Lucid.div_ [Lucid.class_ "text-center"] $ do
        Lucid.div_ [class_ $ base ["mx-auto", "h-16", "w-16", "flex", "items-center", "justify-center", "rounded-full", Tokens.errorBg]] $ do
          Lucid.span_ [class_ $ base [Tokens.errorText, "text-3xl"]] "!"

      -- Title
      Lucid.h1_
        [class_ $ base ["mt-6", "text-center", "text-2xl", Tokens.fontBold, Tokens.fgPrimary]]
        "Link Expired"

      -- Message
      Lucid.div_ [class_ $ base ["mt-4", "text-center", Tokens.fgMuted]] $ do
        Lucid.p_ "This password reset link is invalid or has expired."

      Lucid.p_
        [class_ $ base ["mt-4", "text-center", Tokens.fgMuted]]
        "Password reset links are only valid for 1 hour and can only be used once."

      -- Request new link button
      Lucid.div_ [class_ $ base ["mt-6", "text-center"]] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{userForgotPasswordGetUrl}|],
            hxGet_ [i|/#{userForgotPasswordGetUrl}|],
            hxSwap_ "innerHTML",
            hxTarget_ "body",
            hxPushUrl_ "true",
            class_ $ base [Tokens.bgInverse, Tokens.fgInverse, Tokens.px6, "py-3", Tokens.fontBold, Tokens.hoverBg, "inline-block"]
          ]
          "REQUEST NEW LINK"

      -- Login link
      Lucid.p_ [class_ $ base ["mt-6", "text-center", Tokens.textSm, Tokens.fgMuted]] $ do
        "Remember your password? "
        Lucid.a_
          [ Lucid.href_ [i|/#{userLoginGetUrl}|],
            hxGet_ [i|/#{userLoginGetUrl}|],
            hxSwap_ "innerHTML",
            hxTarget_ "body",
            hxPushUrl_ "true",
            class_ $ base [Tokens.fontBold, Tokens.fgPrimary, "hover:underline"]
          ]
          "Log in"
