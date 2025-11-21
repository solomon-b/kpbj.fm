{-# LANGUAGE QuasiQuotes #-}

module API.Admin.Users.Edit.Post.Templates.Error where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (adminUserEditGetLink)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Effects.Database.Tables.User qualified as User
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Links

notLoggedInTemplate :: Lucid.Html ()
notLoggedInTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "LOGIN REQUIRED"
    Lucid.p_ [Lucid.class_ "mb-6"] "You must be logged in to edit users."
    Lucid.a_
      [ Lucid.href_ "/user/login",
        hxGet_ "/user/login",
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "inline-block bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "LOGIN"

notAuthorizedTemplate :: Lucid.Html ()
notAuthorizedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "ADMIN ACCESS REQUIRED"
    Lucid.p_ [Lucid.class_ "mb-6"] "You do not have permission to edit users."
    Lucid.a_
      [ Lucid.href_ "/",
        hxGet_ "/",
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "inline-block bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← HOME"

errorTemplate :: User.Id -> Text -> Lucid.Html ()
errorTemplate userId errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "ERROR UPDATING USER"
    Lucid.p_ [Lucid.class_ "mb-6 text-red-700"] $ Lucid.toHtml errorMsg
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{editUrl}|],
          hxGet_ [i|/#{editUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
        ]
        "TRY AGAIN"
      Lucid.a_
        [ Lucid.href_ "/admin/users",
          hxGet_ "/admin/users",
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-600 text-white px-6 py-3 font-bold hover:bg-gray-500"
        ]
        "← BACK TO USERS"
  where
    editUrl = Links.linkURI $ adminUserEditGetLink userId
