{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Get.Templates.Auth
  ( notAuthorizedTemplate,
    notLoggedInTemplate,
  )
where

import API.Links (showsLinks, userLinks)
import API.Types
import Data.String.Interpolate (i)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

-- | Template for unauthorized access (non-hosts)
notAuthorizedTemplate :: Lucid.Html ()
notAuthorizedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "Only Host, Staff and Admin users can access the host dashboard."
    Lucid.div_ [Lucid.class_ "space-x-4"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showsGetUrl}|],
          hxGet_ [i|/#{showsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
        ]
        "← BACK TO SHOWS"
      Lucid.a_
        [ Lucid.href_ [i|/#{userLoginGetUrl}|],
          hxGet_ [i|/#{userLoginGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:blue-700 inline-block"
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
  Lucid.div_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "Login Required"
    Lucid.p_ [Lucid.class_ "mb-6"] "Please login to access the host dashboard."
    Lucid.a_
      [ Lucid.href_ [i|/#{userLoginGetUrl}|],
        hxGet_ [i|/#{userLoginGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700 inline-block"
      ]
      "LOGIN"
  where
    userLoginGetUrl :: Links.URI
    userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing
