{-# LANGUAGE QuasiQuotes #-}

module API.Blog.New.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogGetLink, blogNewPostLink)
import Data.String.Interpolate (i)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing Nothing

blogNewPostUrl :: Links.URI
blogNewPostUrl = Links.linkURI blogNewPostLink

--------------------------------------------------------------------------------

-- | New blog post form template
template :: UserMetadata.Model -> Lucid.Html ()
template userMeta = do
  -- Form Header
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "NEW BLOG POST"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          Lucid.strong_ "Author: "
          Lucid.toHtml (UserMetadata.mDisplayName userMeta)
      Lucid.div_ [Lucid.class_ "text-center"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{blogGetUrl}|],
            hxGet_ [i|/#{blogGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "VIEW BLOG"

  -- New Blog Post Form
  Lucid.form_ [Lucid.action_ [i|/#{blogNewPostUrl}|], Lucid.method_ "post", Lucid.class_ "space-y-8 w-full"] $ do
    -- Post Details
    Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "POST DETAILS"

      Lucid.div_ [Lucid.class_ "space-y-6"] $ do
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Post Title *"
          Lucid.input_
            [ Lucid.type_ "text",
              Lucid.name_ "title",
              Lucid.required_ "true",
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "e.g. The Evolution of Industrial Ambient"
            ]

        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Post Content *"
          Lucid.textarea_
            [ Lucid.name_ "content",
              Lucid.required_ "true",
              Lucid.rows_ "12",
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "Write your blog post content here..."
            ]
            ""

        Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 gap-6"] $ do
          Lucid.div_ $ do
            Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Category"
            Lucid.select_ [Lucid.name_ "category", Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono bg-white"] $ do
              Lucid.option_ [Lucid.value_ "Station News"] "Station News"
              Lucid.option_ [Lucid.value_ "Community"] "Community"
              Lucid.option_ [Lucid.value_ "Events"] "Events"
              Lucid.option_ [Lucid.value_ "Music"] "Music"
              Lucid.option_ [Lucid.value_ "Host Spotlights"] "Host Spotlights"
              Lucid.option_ [Lucid.value_ "Local Scene"] "Local Scene"

          Lucid.div_ $ do
            Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Tags"
            Lucid.input_
              [ Lucid.type_ "text",
                Lucid.name_ "tags",
                Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
                Lucid.placeholder_ "industrial, ambient, interview, chrome-valley"
              ]
            Lucid.div_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "Comma separated tags"

    -- Publishing Options
    Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "PUBLISHING OPTIONS"

      Lucid.div_ [Lucid.class_ "space-y-4"] $ do
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Publication Status"
          Lucid.select_ [Lucid.name_ "status", Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono bg-white"] $ do
            Lucid.option_ [Lucid.value_ "published", Lucid.selected_ "true"] "Publish Immediately"
            Lucid.option_ [Lucid.value_ "draft"] "Save as Draft"

        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Excerpt (Optional)"
          Lucid.textarea_
            [ Lucid.name_ "excerpt",
              Lucid.rows_ "3",
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "Short preview of your post (optional - will auto-generate if left blank)"
            ]
            ""

    -- Form Actions
    Lucid.section_ [Lucid.class_ "bg-gray-100 border-2 border-gray-400 p-6"] $ do
      Lucid.div_ [Lucid.class_ "flex justify-end items-center"] $ do
        Lucid.div_ [Lucid.class_ "flex gap-4"] $ do
          Lucid.a_
            [ Lucid.href_ [i|/#{blogGetUrl}|],
              hxGet_ [i|/#{blogGetUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "bg-gray-600 text-white px-6 py-3 font-bold hover:bg-gray-700 text-center"
            ]
            "CANCEL"
          Lucid.button_
            [ Lucid.type_ "submit",
              Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
            ]
            "SAVE POST"
