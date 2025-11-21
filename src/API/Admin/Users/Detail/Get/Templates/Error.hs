module API.Admin.Users.Detail.Get.Templates.Error where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Lucid qualified
import Lucid.Extras

notLoggedInTemplate :: Lucid.Html ()
notLoggedInTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "LOGIN REQUIRED"
    Lucid.p_ [Lucid.class_ "mb-6"] "You must be logged in to access this page."
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
    Lucid.p_ [Lucid.class_ "mb-6"] "You do not have permission to access this page."
    Lucid.a_
      [ Lucid.href_ "/",
        hxGet_ "/",
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "inline-block bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← HOME"

notFoundTemplate :: Lucid.Html ()
notFoundTemplate = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "USER NOT FOUND"
    Lucid.p_ [Lucid.class_ "mb-6"] "The user you are looking for does not exist."
    Lucid.a_
      [ Lucid.href_ "/admin/users",
        hxGet_ "/admin/users",
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "inline-block bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO USERS"

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "ERROR"
    Lucid.p_ [Lucid.class_ "mb-6"] $ Lucid.toHtml errorMsg
    Lucid.a_
      [ Lucid.href_ "/admin/users",
        hxGet_ "/admin/users",
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "inline-block bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO USERS"
