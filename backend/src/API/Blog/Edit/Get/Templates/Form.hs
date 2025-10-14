{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogGetLink, blogPostGetLink)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.PostStatus (BlogPostStatus (..))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xBindClass_, xData_, xModel_, xOnClick_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing

blogPostGetUrl :: Text -> Links.URI
blogPostGetUrl slug = Links.linkURI $ blogPostGetLink slug

--------------------------------------------------------------------------------

alpineState :: BlogPosts.Model -> Text
alpineState BlogPosts.Model {bpmTitle, bpmContent, bpmExcerpt} =
  let excerptText = case bpmExcerpt of
        Nothing -> ""
        Just e -> e
   in [i|{
  fields: {
    title: { value: `#{bpmTitle}`, isValid: true },
    content: { value: `#{bpmContent}`, isValid: true },
    excerpt: { value: `#{excerptText}`, isValid: true }
  },
  showErrors: false,

  validateAndSubmit(event) {
    this.showErrors = true;

    // Validate required fields
    this.fields.title.isValid = this.fields.title.value.trim() !== '';
    this.fields.content.isValid = this.fields.content.value.trim() !== '';

    // Optional field is always valid
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

-- | Blog post edit template
template :: BlogPosts.Model -> [BlogTags.Model] -> UserMetadata.Model -> Lucid.Html ()
template blogPost tags userMeta = do
  let postSlug = blogPost.bpmSlug
      postBackUrl = blogPostGetUrl postSlug
      tagsText = Text.intercalate ", " $ map (\t -> t.btmName) tags

  Lucid.div_ [Lucid.class_ "max-w-2xl mx-auto", xData_ (alpineState blogPost)] $ do
    header blogPost userMeta postBackUrl

    -- Edit Blog Post Form
    Lucid.form_ [Lucid.action_ [i|/blog/#{postSlug}/edit|], Lucid.method_ "post", Lucid.class_ "space-y-8 w-full"] $ do
      -- Post Details
      Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "POST DETAILS"

        Lucid.div_ [Lucid.class_ "space-y-6"] $ do
          titleField blogPost
          contentField blogPost
          tagsField tagsText

      -- Publishing Options
      Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "PUBLISHING OPTIONS"

        Lucid.div_ [Lucid.class_ "space-y-4"] $ do
          statusField blogPost
          excerptField blogPost

      -- Form Actions
      Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
        Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
          submitButton
          cancelButton postBackUrl

selectedIf :: Bool -> Lucid.Attributes
selectedIf True = Lucid.selected_ "selected"
selectedIf False = mempty

header :: BlogPosts.Model -> UserMetadata.Model -> Links.URI -> Lucid.Html ()
header blogPost userMeta postBackUrl = do
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "EDIT BLOG POST"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          Lucid.strong_ "Post: "
          Lucid.toHtml blogPost.bpmTitle
          " • "
          Lucid.strong_ "Editor: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [Lucid.class_ "space-x-4"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{postBackUrl}|],
            hxGet_ [i|/#{postBackUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "← BACK TO POST"
        Lucid.a_
          [ Lucid.href_ [i|/#{blogGetUrl}|],
            hxGet_ [i|/#{blogGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "VIEW BLOG"

titleField :: BlogPosts.Model -> Lucid.Html ()
titleField blogPost =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Post Title *"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "title",
        Lucid.required_ "true",
        Lucid.value_ blogPost.bpmTitle,
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        Lucid.placeholder_ "e.g. The Evolution of Industrial Ambient",
        xModel_ "fields.title.value",
        xBindClass_ "showErrors && !fields.title.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

contentField :: BlogPosts.Model -> Lucid.Html ()
contentField blogPost =
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
      (Lucid.toHtml blogPost.bpmContent)

tagsField :: Text -> Lucid.Html ()
tagsField tagsText =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Tags"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "tags",
        Lucid.value_ tagsText,
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        Lucid.placeholder_ "industrial, ambient, interview, chrome-valley"
      ]
    Lucid.div_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "Comma separated tags"

statusField :: BlogPosts.Model -> Lucid.Html ()
statusField blogPost =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Publication Status"
    Lucid.select_ [Lucid.name_ "status", Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono bg-white"] $ do
      Lucid.option_ [Lucid.value_ "published", selectedIf (blogPost.bpmStatus == Published)] "Publish Immediately"
      Lucid.option_ [Lucid.value_ "draft", selectedIf (blogPost.bpmStatus == Draft)] "Save as Draft"

excerptField :: BlogPosts.Model -> Lucid.Html ()
excerptField blogPost =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Excerpt (Optional)"
    Lucid.textarea_
      [ Lucid.name_ "excerpt",
        Lucid.rows_ "3",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono leading-relaxed",
        Lucid.placeholder_ "Short preview of your post (optional - will auto-generate if left blank)",
        xModel_ "fields.excerpt.value"
      ]
      (Lucid.toHtml $ case blogPost.bpmExcerpt of Nothing -> ""; Just e -> e)

submitButton :: Lucid.Html ()
submitButton =
  Lucid.button_
    [ Lucid.type_ "submit",
      Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors",
      xOnClick_ "validateAndSubmit($event)"
    ]
    "UPDATE POST"

cancelButton :: Links.URI -> Lucid.Html ()
cancelButton postBackUrl =
  Lucid.a_
    [ Lucid.href_ [i|/#{postBackUrl}|],
      hxGet_ [i|/#{postBackUrl}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline"
    ]
    "CANCEL"
