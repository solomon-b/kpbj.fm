{-# LANGUAGE QuasiQuotes #-}

module API.Show.Edit.Get.Templates.Error
  ( notLoggedInTemplate,
    notFoundTemplate,
    notAuthorizedTemplate,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showsGetLink, userLoginGetLink)
import Data.String.Interpolate (i)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

showsGetUrl :: Links.URI
showsGetUrl = Links.linkURI $ showsGetLink Nothing Nothing Nothing Nothing

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLoginGetLink Nothing Nothing

--------------------------------------------------------------------------------

notLoggedInTemplate :: Lucid.Html ()
notLoggedInTemplate = do
  Lucid.section_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "NOT LOGGED IN"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-800"] "You must be logged in to edit a show."
    Lucid.div_ [Lucid.class_ "space-x-4"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{userLoginGetUrl}|],
          hxGet_ [i|/#{userLoginGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
        ]
        "LOG IN"
      Lucid.a_
        [ Lucid.href_ [i|/#{showsGetUrl}|],
          hxGet_ [i|/#{showsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-400 text-white px-6 py-3 font-bold hover:bg-gray-500 inline-block"
        ]
        "BACK TO SHOWS"

notFoundTemplate :: Lucid.Html ()
notFoundTemplate = do
  Lucid.section_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "SHOW NOT FOUND"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-800"] "The show you're trying to edit doesn't exist."
    Lucid.a_
      [ Lucid.href_ [i|/#{showsGetUrl}|],
        hxGet_ [i|/#{showsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
      ]
      "BACK TO SHOWS"

notAuthorizedTemplate :: Lucid.Html ()
notAuthorizedTemplate = do
  Lucid.section_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "NOT AUTHORIZED"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-800"] "You don't have permission to edit this show. Only hosts and staff can edit show details."
    Lucid.a_
      [ Lucid.href_ [i|/#{showsGetUrl}|],
        hxGet_ [i|/#{showsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
      ]
      "BACK TO SHOWS"
