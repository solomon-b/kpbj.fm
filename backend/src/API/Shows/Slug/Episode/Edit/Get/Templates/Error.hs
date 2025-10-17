{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Episode.Edit.Get.Templates.Error
  ( notAuthorizedTemplate,
    notLoggedInTemplate,
    notFoundTemplate,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (hostDashboardGetLink, userLoginGetLink)
import Data.String.Interpolate (i)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI $ hostDashboardGetLink Nothing

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLoginGetLink Nothing Nothing

--------------------------------------------------------------------------------

-- | Template for unauthorized access
notAuthorizedTemplate :: Lucid.Html ()
notAuthorizedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "You can only edit your own episodes."
    Lucid.div_ [Lucid.class_ "space-x-4"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
          hxGet_ [i|/#{hostDashboardGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
        ]
        "← BACK TO DASHBOARD"

-- | Template for users not logged in
notLoggedInTemplate :: Lucid.Html ()
notLoggedInTemplate = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "Login Required"
    Lucid.p_ [Lucid.class_ "mb-6"] "Please login to edit episodes."
    Lucid.a_
      [ Lucid.href_ [i|/#{userLoginGetUrl}|],
        hxGet_ [i|/#{userLoginGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700 inline-block"
      ]
      "LOGIN"

-- | Template for episode not found
notFoundTemplate :: Lucid.Html ()
notFoundTemplate = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "Episode Not Found"
    Lucid.p_ [Lucid.class_ "mb-6"] "The episode you're trying to edit doesn't exist."
    Lucid.a_
      [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
        hxGet_ [i|/#{hostDashboardGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
      ]
      "← BACK TO DASHBOARD"
