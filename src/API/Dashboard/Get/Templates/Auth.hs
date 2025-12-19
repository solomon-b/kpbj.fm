{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Get.Templates.Auth
  ( notAuthorizedTemplate,
    notLoggedInTemplate,
  )
where

import API.Links (showsLinks, userLinks)
import API.Types
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

-- | Template for unauthorized access (non-hosts)
notAuthorizedTemplate :: Lucid.Html ()
notAuthorizedTemplate = do
  Lucid.div_ [class_ $ base [Tokens.errorBg, Tokens.border2, Tokens.errorBorder, "p-8", "text-center"]] $ do
    Lucid.h2_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb4, Tokens.errorText]] "Access Denied"
    Lucid.p_ [Lucid.class_ Tokens.mb6] "Only Host, Staff and Admin users can access the host dashboard."
    Lucid.div_ [Lucid.class_ "space-x-4"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showsGetUrl}|],
          hxGet_ [i|/#{showsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700", "inline-block"]
        ]
        "← BACK TO SHOWS"
      Lucid.a_
        [ Lucid.href_ [i|/#{userLoginGetUrl}|],
          hxGet_ [i|/#{userLoginGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base ["bg-blue-600", Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-blue-700", "inline-block"]
        ]
        "LOGIN"
  where
    showsGetUrl :: Links.URI
    showsGetUrl = Links.linkURI $ showsLinks.list Nothing Nothing Nothing Nothing

    userLoginGetUrl :: Links.URI
    userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

-- | Template for users not logged in
notLoggedInTemplate :: Lucid.Html ()
notLoggedInTemplate = do
  Lucid.div_ [class_ $ base ["bg-yellow-100", Tokens.border2, "border-yellow-600", "p-8", "text-center"]] $ do
    Lucid.h2_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb4, "text-yellow-800"]] "Login Required"
    Lucid.p_ [Lucid.class_ Tokens.mb6] "Please login to access the host dashboard."
    Lucid.a_
      [ Lucid.href_ [i|/#{userLoginGetUrl}|],
        hxGet_ [i|/#{userLoginGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base ["bg-blue-600", Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-blue-700", "inline-block"]
      ]
      "LOGIN"
  where
    userLoginGetUrl :: Links.URI
    userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing
