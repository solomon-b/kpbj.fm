{-# LANGUAGE QuasiQuotes #-}

module API.Episode.Edit.Get.Templates.Error
  ( notLoggedInTemplate,
    notFoundTemplate,
    notAuthorizedTemplate,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (rootGetLink)
import Data.String.Interpolate (i)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
homeGetUrl :: Links.URI
homeGetUrl = Links.linkURI rootGetLink

--------------------------------------------------------------------------------

-- | Template when user is not logged in
notLoggedInTemplate :: Lucid.Html ()
notLoggedInTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "You must be logged in to edit episodes."
    Lucid.a_
      [ Lucid.href_ [i|/#{homeGetUrl}|],
        hxGet_ [i|/#{homeGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO HOME"

-- | Template when episode is not found
notFoundTemplate :: Lucid.Html ()
notFoundTemplate = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "Episode Not Found"
    Lucid.p_ [Lucid.class_ "mb-6"] "The episode you're trying to edit doesn't exist."
    Lucid.a_
      [ Lucid.href_ [i|/#{homeGetUrl}|],
        hxGet_ [i|/#{homeGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO HOME"

-- | Template when user is not authorized
notAuthorizedTemplate :: Lucid.Html ()
notAuthorizedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "You can only edit episodes you created, or episodes for shows you host (or with staff permissions)."
    Lucid.a_
      [ Lucid.href_ [i|/#{homeGetUrl}|],
        hxGet_ [i|/#{homeGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO HOME"
