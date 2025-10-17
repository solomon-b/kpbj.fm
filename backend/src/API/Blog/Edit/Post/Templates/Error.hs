{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Edit.Post.Templates.Error
  ( unauthorizedTemplate,
    notFoundTemplate,
    forbiddenTemplate,
    errorTemplate,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogGetLink, hostDashboardGetLink)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing

hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI $ hostDashboardGetLink Nothing

--------------------------------------------------------------------------------

unauthorizedTemplate :: Lucid.Html ()
unauthorizedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "You must be logged in to edit blog posts."

notFoundTemplate :: Lucid.Html ()
notFoundTemplate = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "Blog Post Not Found"
    Lucid.p_ [Lucid.class_ "mb-6"] "The blog post you're trying to update doesn't exist."
    Lucid.a_
      [ Lucid.href_ [i|/#{blogGetUrl}|],
        hxGet_ [i|/#{blogGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO BLOG"

forbiddenTemplate :: Lucid.Html ()
forbiddenTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "You can only edit blog posts you authored or have staff permissions."
    Lucid.a_
      [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
        hxGet_ [i|/#{hostDashboardGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO DASHBOARD"

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Update Failed"
    Lucid.p_ [Lucid.class_ "mb-6"] $ Lucid.toHtml errorMsg
    Lucid.a_
      [ Lucid.href_ [i|/#{blogGetUrl}|],
        hxGet_ [i|/#{blogGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO BLOG"
