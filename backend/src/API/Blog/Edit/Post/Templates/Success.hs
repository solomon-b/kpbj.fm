{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Edit.Post.Templates.Success
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogGetLink, blogPostGetLink)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing

blogPostGetUrl :: Text -> Links.URI
blogPostGetUrl slug = Links.linkURI $ blogPostGetLink slug

--------------------------------------------------------------------------------

template :: Text -> Lucid.Html ()
template postSlug = do
  let postUrl = blogPostGetUrl postSlug
  Lucid.div_ [Lucid.class_ "bg-green-100 border-2 border-green-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-green-800"] "✓ Blog Post Updated Successfully!"
    Lucid.p_ [Lucid.class_ "mb-6"] "Your blog post has been updated and saved."
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{postUrl}|],
          hxGet_ [i|/#{postUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
        ]
        "VIEW POST"
      Lucid.a_
        [ Lucid.href_ [i|/#{blogGetUrl}|],
          hxGet_ [i|/#{blogGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-400 text-white px-6 py-3 font-bold hover:bg-gray-500"
        ]
        "BACK TO BLOG"
