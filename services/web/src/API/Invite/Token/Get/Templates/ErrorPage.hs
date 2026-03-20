{-# LANGUAGE QuasiQuotes #-}

module API.Invite.Token.Get.Templates.ErrorPage (template) where

--------------------------------------------------------------------------------

import API.Links (apiLinks, rootLink)
import API.Types (Routes (..))
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.HTMX

--------------------------------------------------------------------------------

-- | Template for the invite error page (expired/invalid token).
--
-- Shown when the invitation link is no longer valid (expired, claimed, or
-- revoked). Provides a simple message and a link back to the home page.
template :: Lucid.Html ()
template =
  Lucid.div_ [class_ $ base ["max-w-2xl", "mx-auto", Tokens.py8]] $
    Lucid.div_ [class_ $ base [Tokens.bgMain, "p-8", "text-center"]] $ do
      Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb4]] $
        "This invitation link is no longer valid"
      Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.mb2]] $
        "It may have expired or already been used."
      Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.mb8]] $
        "Contact the station for a new invitation."
      Lucid.a_
        [ Lucid.href_ homeUrl,
          hxGet_ homeUrl,
          hxSwap_ "innerHTML",
          hxTarget_ "body",
          hxPushUrl_ "true",
          class_ $ base [Tokens.linkText]
        ]
        "\x2190 Back to kpbj.fm"
  where
    homeUrl = rootLink apiLinks.rootGet
