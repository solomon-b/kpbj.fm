{-# LANGUAGE QuasiQuotes #-}

module API.Admin.Users.Edit.Post.Templates.Success where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (adminUserDetailGetLink, adminUsersGetLink)
import Data.String.Interpolate (i)
import Effects.Database.Tables.User qualified as User
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Links

successTemplate :: User.Id -> Lucid.Html ()
successTemplate userId = do
  Lucid.div_ [Lucid.class_ "bg-green-100 border-2 border-green-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-green-800"] "✓ USER UPDATED SUCCESSFULLY"
    Lucid.p_ [Lucid.class_ "mb-6 text-green-700"] "The user's information has been updated."
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{detailUrl}|],
          hxGet_ [i|/#{detailUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
        ]
        "VIEW USER"
      Lucid.a_
        [ Lucid.href_ [i|/#{usersUrl}|],
          hxGet_ [i|/#{usersUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-600 text-white px-6 py-3 font-bold hover:bg-gray-700"
        ]
        "← BACK TO USERS"
  where
    detailUrl = Links.linkURI $ adminUserDetailGetLink userId
    usersUrl = Links.linkURI $ adminUsersGetLink Nothing Nothing Nothing Nothing
