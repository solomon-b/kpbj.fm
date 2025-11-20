{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Blog.Edit.Get.Templates.Page
  ( editBlogPostForm,
    notLoggedInTemplate,
    permissionDeniedTemplate,
    errorTemplate,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showBlogEditPostLink, showBlogGetLink, showBlogPostGetLink)
import Component.Form.Builder
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.PostStatus (BlogPostStatus (..))
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

showBlogGetUrl :: Shows.Model -> Links.URI
showBlogGetUrl showModel = Links.linkURI $ showBlogGetLink (Shows.slug showModel) Nothing Nothing

showBlogPostGetUrl :: Shows.Model -> ShowBlogPosts.Model -> Links.URI
showBlogPostGetUrl showModel post = Links.linkURI $ showBlogPostGetLink (Shows.id showModel) (ShowBlogPosts.id post) (ShowBlogPosts.slug post)

showBlogEditPostUrl :: Shows.Model -> ShowBlogPosts.Model -> Links.URI
showBlogEditPostUrl showModel post = Links.linkURI $ showBlogEditPostLink (Shows.id showModel) (ShowBlogPosts.id post) (ShowBlogPosts.slug post)

--------------------------------------------------------------------------------

-- | Edit show blog post form template
editBlogPostForm :: Shows.Model -> ShowBlogPosts.Model -> [ShowBlogTags.Model] -> Lucid.Html ()
editBlogPostForm showModel post tags = do
  buildValidatedForm
    FormBuilder
      { fbAction = [i|/#{showBlogEditPostUrl showModel post}|],
        fbMethod = "post",
        fbHeader = Just (renderFormHeader showModel post),
        fbFields = showBlogEditFormFields post tags,
        fbAdditionalContent = [renderSubmitActions showModel post],
        fbStyles = defaultFormStyles
      }

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: Shows.Model -> ShowBlogPosts.Model -> Lucid.Html ()
renderFormHeader showModel post =
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "EDIT BLOG POST"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          Lucid.strong_ "Show: "
          Lucid.toHtml (Shows.title showModel)
      Lucid.div_ [Lucid.class_ "text-center flex gap-4"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{showBlogPostGetUrl showModel post}|],
            hxGet_ [i|/#{showBlogPostGetUrl showModel post}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "VIEW POST"
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

showBlogEditFormFields :: ShowBlogPosts.Model -> [ShowBlogTags.Model] -> [FormField]
showBlogEditFormFields post tags =
  let tagsText = Text.intercalate ", " $ map ShowBlogTags.sbtmName tags
      statusValue :: Text
      statusValue = case ShowBlogPosts.status post of
        Published -> "published"
        Draft -> "draft"
        _ -> "draft"
   in [ -- Post Details Section
        SectionField
          { sfTitle = "POST DETAILS",
            sfFields =
              [ ValidatedTextField
                  { vfName = "title",
                    vfLabel = "Post Title",
                    vfInitialValue = Just (ShowBlogPosts.title post),
                    vfPlaceholder = Nothing,
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
                    vtInitialValue = Just (ShowBlogPosts.content post),
                    vtRows = 12,
                    vtPlaceholder = Nothing,
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
                    vfInitialValue = if Text.null tagsText then Nothing else Just tagsText,
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
                      [ SelectOption "published" "Publish Immediately" (statusValue == "published") Nothing,
                        SelectOption "draft" "Save as Draft" (statusValue == "draft") Nothing
                      ],
                    vsHint = Nothing,
                    vsValidation = emptyValidation {vrRequired = True}
                  },
                ValidatedTextareaField
                  { vtName = "excerpt",
                    vtLabel = "Excerpt (Optional)",
                    vtInitialValue = ShowBlogPosts.excerpt post,
                    vtRows = 3,
                    vtPlaceholder = Just "Short preview of your post (optional)",
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

renderSubmitActions :: Shows.Model -> ShowBlogPosts.Model -> Lucid.Html ()
renderSubmitActions showModel post =
  Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.button_
        [ Lucid.type_ "submit",
          Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors"
        ]
        "UPDATE POST"
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogPostGetUrl showModel post}|],
          hxGet_ [i|/#{showBlogPostGetUrl showModel post}|],
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
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] "You must be logged in to edit blog posts."

permissionDeniedTemplate :: Lucid.Html ()
permissionDeniedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Permission Denied"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] "You are not authorized to edit this blog post."

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Error"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] $ Lucid.toHtml errorMsg
