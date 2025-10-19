{-# LANGUAGE QuasiQuotes #-}

module API.Events.New.Get.Templates.Auth
  ( notAuthorizedTemplate,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventsGetLink, userLoginGetLink)
import Data.String.Interpolate (i)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
eventsGetUrl :: Links.URI
eventsGetUrl = Links.linkURI $ eventsGetLink Nothing Nothing

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLoginGetLink Nothing Nothing

--------------------------------------------------------------------------------

-- | Template for unauthorized access
notAuthorizedTemplate :: Lucid.Html ()
notAuthorizedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "Only Staff and Admin users can create events."
    Lucid.div_ [Lucid.class_ "space-x-4"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{eventsGetUrl}|],
          hxGet_ [i|/#{eventsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
        ]
        "← BACK TO EVENTS"
      Lucid.a_
        [ Lucid.href_ [i|/#{userLoginGetUrl}|],
          hxGet_ [i|/#{userLoginGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700 inline-block"
        ]
        "LOGIN"
