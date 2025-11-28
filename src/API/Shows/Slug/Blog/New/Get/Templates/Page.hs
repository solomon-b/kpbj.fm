{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Blog.New.Get.Templates.Page
  ( newBlogPostForm,
    notLoggedInTemplate,
    errorTemplate,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showBlogGetLink, showBlogNewPostLink, showGetLink)
import Component.Form.Builder
import Data.String.Interpolate (i)
import Data.Text (Text)
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

showBlogGetUrl :: Shows.Model -> Links.URI
showBlogGetUrl showModel = Links.linkURI $ showBlogGetLink (Shows.slug showModel) Nothing Nothing

showBlogNewPostUrl :: Shows.Model -> Links.URI
showBlogNewPostUrl showModel = Links.linkURI $ showBlogNewPostLink (Shows.slug showModel)

showGetUrl :: Shows.Model -> Links.URI
showGetUrl showModel = Links.linkURI $ showGetLink (Shows.slug showModel)

--------------------------------------------------------------------------------

-- | New show blog post form template
newBlogPostForm :: Shows.Model -> Lucid.Html ()
newBlogPostForm showModel = do
  buildValidatedForm
    FormBuilder
      { fbAction = [i|/#{showBlogNewPostUrl showModel}|],
        fbMethod = "post",
        fbHeader = Just (renderFormHeader showModel),
        fbFields = showBlogFormFields,
        fbAdditionalContent = [renderSubmitActions showModel],
        fbStyles = defaultFormStyles,
        fbHtmx = Nothing
      }

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: Shows.Model -> Lucid.Html ()
renderFormHeader showModel =
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "NEW BLOG POST"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          Lucid.strong_ "Show: "
          Lucid.toHtml (Shows.title showModel)
      Lucid.div_ [Lucid.class_ "text-center flex gap-4"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{showGetUrl showModel}|],
            hxGet_ [i|/#{showGetUrl showModel}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "VIEW SHOW"
        Lucid.a_
          [ Lucid.href_ [i|/#{showBlogGetUrl showModel}|],
            hxGet_ [i|/#{showBlogGetUrl showModel}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "VIEW BLOG"

--------------------------------------------------------------------------------
-- Form Fields Definition

showBlogFormFields :: [FormField]
showBlogFormFields =
  [ -- Post Details Section
    SectionField
      { sfTitle = "POST DETAILS",
        sfFields =
          [ ValidatedTextField
              { vfName = "title",
                vfLabel = "Post Title",
                vfInitialValue = Nothing,
                vfPlaceholder = Just "e.g. Behind the Scenes: Selecting Music for This Week's Show",
                vfHint = Nothing,
                vfValidation =
                  ValidationRules
                    { vrMinLength = Just 3,
                      vrMaxLength = Just 200,
                      vrPattern = Nothing,
                      vrRequired = True,
                      vrCustomValidation = Nothing
                    }
              },
            ValidatedTextareaField
              { vtName = "content",
                vtLabel = "Post Content",
                vtInitialValue = Nothing,
                vtRows = 12,
                vtPlaceholder = Just "Write your blog post content here...",
                vtHint = Nothing,
                vtValidation =
                  ValidationRules
                    { vrMinLength = Just 10,
                      vrMaxLength = Just 50000,
                      vrPattern = Nothing,
                      vrRequired = True,
                      vrCustomValidation = Nothing
                    }
              },
            ValidatedTextField
              { vfName = "tags",
                vfLabel = "Tags",
                vfInitialValue = Nothing,
                vfPlaceholder = Just "music-selection, behind-the-scenes, guest-interview",
                vfHint = Just "Comma separated tags (lowercase with hyphens)",
                vfValidation =
                  ValidationRules
                    { vrMinLength = Nothing,
                      vrMaxLength = Just 500,
                      vrPattern = Nothing,
                      vrRequired = False,
                      vrCustomValidation = Nothing
                    }
              }
          ]
      },
    -- Publishing Options Section
    SectionField
      { sfTitle = "PUBLISHING OPTIONS",
        sfFields =
          [ ValidatedSelectField
              { vsName = "status",
                vsLabel = "Publication Status",
                vsOptions =
                  [ SelectOption "published" "Publish Immediately" True Nothing,
                    SelectOption "draft" "Save as Draft" False Nothing
                  ],
                vsHint = Nothing,
                vsValidation = emptyValidation {vrRequired = True}
              },
            ValidatedTextareaField
              { vtName = "excerpt",
                vtLabel = "Excerpt (Optional)",
                vtInitialValue = Nothing,
                vtRows = 3,
                vtPlaceholder = Just "Short preview of your post (optional - will auto-generate if left blank)",
                vtHint = Nothing,
                vtValidation =
                  ValidationRules
                    { vrMinLength = Nothing,
                      vrMaxLength = Just 500,
                      vrPattern = Nothing,
                      vrRequired = False,
                      vrCustomValidation = Nothing
                    }
              }
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Form Submit Actions (rendered inside <form>)

renderSubmitActions :: Shows.Model -> Lucid.Html ()
renderSubmitActions showModel =
  Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.button_
        [ Lucid.type_ "submit",
          Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors"
        ]
        "SAVE POST"
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogGetUrl showModel}|],
          hxGet_ [i|/#{showBlogGetUrl showModel}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline inline-block"
        ]
        "CANCEL"

--------------------------------------------------------------------------------
-- Error Templates

notLoggedInTemplate :: Lucid.Html ()
notLoggedInTemplate = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Authentication Required"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] "You must be logged in to create blog posts."

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Error"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] $ Lucid.toHtml errorMsg
