{-# LANGUAGE QuasiQuotes #-}

module API.Events.Edit.Get.Templates.Error
  ( notLoggedInTemplate,
    notFoundTemplate,
    notAuthorizedTemplate,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventsGetLink, hostDashboardGetLink)
import Data.String.Interpolate (i)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

eventsGetUrl :: Links.URI
eventsGetUrl = Links.linkURI $ eventsGetLink Nothing Nothing

hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI hostDashboardGetLink

--------------------------------------------------------------------------------

notLoggedInTemplate :: Lucid.Html ()
notLoggedInTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "You must be logged in to edit events."
    Lucid.a_
      [ Lucid.href_ [i|/#{eventsGetUrl}|],
        hxGet_ [i|/#{eventsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO EVENTS"

notFoundTemplate :: Lucid.Html ()
notFoundTemplate = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "Event Not Found"
    Lucid.p_ [Lucid.class_ "mb-6"] "The event you're trying to edit doesn't exist."
    Lucid.a_
      [ Lucid.href_ [i|/#{eventsGetUrl}|],
        hxGet_ [i|/#{eventsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO EVENTS"

notAuthorizedTemplate :: Lucid.Html ()
notAuthorizedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "You can only edit events you created or have staff permissions."
    Lucid.a_
      [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
        hxGet_ [i|/#{hostDashboardGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO DASHBOARD"
