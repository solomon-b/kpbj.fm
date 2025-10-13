{-# LANGUAGE QuasiQuotes #-}

module API.Blog.New.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogGetLink, blogNewPostLink)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xBindClass_, xData_, xModel_, xOnClick_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing

blogNewPostUrl :: Links.URI
blogNewPostUrl = Links.linkURI blogNewPostLink

--------------------------------------------------------------------------------

alpineState :: Text
alpineState =
  [i|{
  fields: {
    title: { value: '', isValid: true },
    content: { value: '', isValid: true },
    tags: { value: '', isValid: true },
    excerpt: { value: '', isValid: true }
  },
  showErrors: false,

  validateAndSubmit(event) {
    this.showErrors = true;

    // Validate required fields
    this.fields.title.isValid = this.fields.title.value.trim() !== '';
    this.fields.content.isValid = this.fields.content.value.trim() !== '';

    // Optional fields are always valid
    this.fields.tags.isValid = true;
    this.fields.excerpt.isValid = true;

    // Check if all required fields are valid
    const allFieldsValid = this.fields.title.isValid && this.fields.content.isValid;

    if (!allFieldsValid) {
      event.preventDefault();
      return false;
    }

    return true;
  }
}|]

--------------------------------------------------------------------------------

-- | New blog post form template
template :: UserMetadata.Model -> Lucid.Html ()
template userMeta = do
  Lucid.div_ [Lucid.class_ "max-w-2xl mx-auto", xData_ alpineState] $ do
    header userMeta

    -- New Blog Post Form
    Lucid.form_ [Lucid.action_ [i|/#{blogNewPostUrl}|], Lucid.method_ "post", Lucid.class_ "space-y-8 w-full"] $ do
      -- Post Details
      Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "POST DETAILS"

        Lucid.div_ [Lucid.class_ "space-y-6"] $ do
          titleField
          contentField
          tagsField

      -- Publishing Options
      Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "PUBLISHING OPTIONS"

        Lucid.div_ [Lucid.class_ "space-y-4"] $ do
          statusField
          excerptField

      -- Form Actions
      Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
        Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
          submitButton
          cancelButton

header :: UserMetadata.Model -> Lucid.Html ()
header userMeta =
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

titleField :: Lucid.Html ()
titleField =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Post Title *"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "title",
        Lucid.required_ "true",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        Lucid.placeholder_ "e.g. The Evolution of Industrial Ambient",
        xModel_ "fields.title.value",
        xBindClass_ "showErrors && !fields.title.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

contentField :: Lucid.Html ()
contentField =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Post Content *"
    Lucid.textarea_
      [ Lucid.name_ "content",
        Lucid.required_ "true",
        Lucid.rows_ "12",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono leading-relaxed",
        Lucid.placeholder_ "Write your blog post content here...",
        xModel_ "fields.content.value",
        xBindClass_ "showErrors && !fields.content.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]
      ""

tagsField :: Lucid.Html ()
tagsField =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Tags"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "tags",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        Lucid.placeholder_ "industrial, ambient, interview, chrome-valley",
        xModel_ "fields.tags.value"
      ]
    Lucid.div_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "Comma separated tags"

statusField :: Lucid.Html ()
statusField =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Publication Status"
    Lucid.select_ [Lucid.name_ "status", Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono bg-white"] $ do
      Lucid.option_ [Lucid.value_ "published", Lucid.selected_ "true"] "Publish Immediately"
      Lucid.option_ [Lucid.value_ "draft"] "Save as Draft"

excerptField :: Lucid.Html ()
excerptField =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Excerpt (Optional)"
    Lucid.textarea_
      [ Lucid.name_ "excerpt",
        Lucid.rows_ "3",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono leading-relaxed",
        Lucid.placeholder_ "Short preview of your post (optional - will auto-generate if left blank)",
        xModel_ "fields.excerpt.value"
      ]
      ""

submitButton :: Lucid.Html ()
submitButton =
  Lucid.button_
    [ Lucid.type_ "submit",
      Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors",
      xOnClick_ "validateAndSubmit($event)"
    ]
    "SAVE POST"

cancelButton :: Lucid.Html ()
cancelButton =
  Lucid.a_
    [ Lucid.href_ [i|/#{blogGetUrl}|],
      hxGet_ [i|/#{blogGetUrl}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline"
    ]
    "CANCEL"
