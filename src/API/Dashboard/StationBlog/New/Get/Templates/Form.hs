{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StationBlog.New.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStationBlogLinks)
import API.Types (DashboardStationBlogRoutes (..))
import Component.Form.Builder
import Component.ImageFilePicker qualified as ImageFilePicker
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardStationBlogGetUrl :: Links.URI
dashboardStationBlogGetUrl = Links.linkURI $ dashboardStationBlogLinks.list Nothing

dashboardStationBlogNewPostUrl :: Links.URI
dashboardStationBlogNewPostUrl = Links.linkURI dashboardStationBlogLinks.newPost

--------------------------------------------------------------------------------

-- | New blog post form template using FormBuilder
template :: UserMetadata.Model -> Lucid.Html ()
template userMeta = do
  buildValidatedForm
    FormBuilder
      { fbAction = [i|/#{dashboardStationBlogNewPostUrl}|],
        fbMethod = "post",
        fbHeader = Just (renderFormHeader userMeta),
        fbFields = blogFormFields,
        fbAdditionalContent = [renderSubmitActions],
        fbStyles = defaultFormStyles,
        fbHtmx = Nothing
      }

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: UserMetadata.Model -> Lucid.Html ()
renderFormHeader userMeta =
  Lucid.section_ [class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "NEW BLOG POST"
        Lucid.div_ [class_ $ base ["text-gray-300", Tokens.textSm]] $ do
          Lucid.strong_ "Author: "
          Lucid.toHtml userMeta.mDisplayName

--------------------------------------------------------------------------------
-- Form Fields Definition

blogFormFields :: [FormField]
blogFormFields =
  [ -- Post Details Section
    SectionField
      { sfTitle = "POST DETAILS",
        sfFields =
          [ ValidatedTextField
              { vfName = "title",
                vfLabel = "Post Title",
                vfInitialValue = Nothing,
                vfPlaceholder = Just "e.g. The Evolution of Industrial Ambient",
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
            PlainFileField
              { pffHtml = heroImageField
              },
            ValidatedTextField
              { vfName = "tags",
                vfLabel = "Tags",
                vfInitialValue = Nothing,
                vfPlaceholder = Just "industrial, ambient, interview, chrome-valley",
                vfHint = Just "Comma separated tags",
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
-- Helper Functions

-- | Render hero image field with integrated preview.
heroImageField :: Lucid.Html ()
heroImageField =
  ImageFilePicker.render
    ImageFilePicker.Config
      { ImageFilePicker.fieldName = "hero_image",
        ImageFilePicker.label = "Hero Image (Optional)",
        ImageFilePicker.existingImageUrl = "",
        ImageFilePicker.accept = "image/*",
        ImageFilePicker.maxSizeMB = 10,
        ImageFilePicker.isRequired = False,
        ImageFilePicker.aspectRatio = (16, 9)
      }

--------------------------------------------------------------------------------
-- Form Submit Actions (rendered inside <form>)

renderSubmitActions :: Lucid.Html ()
renderSubmitActions =
  Lucid.section_ [class_ $ base ["bg-gray-50", Tokens.border2, "border-gray-300", Tokens.p6]] $ do
    Lucid.div_ [class_ $ base ["flex", Tokens.gap4, "justify-center"]] $ do
      Lucid.button_
        [ Lucid.type_ "submit",
          class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.px8, "py-3", Tokens.fontBold, "hover:bg-gray-700", "transition-colors"]
        ]
        "SAVE POST"
      Lucid.a_
        [ Lucid.href_ [i|/#{dashboardStationBlogGetUrl}|],
          hxGet_ [i|/#{dashboardStationBlogGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base ["bg-gray-400", Tokens.textWhite, Tokens.px8, "py-3", Tokens.fontBold, "hover:bg-gray-500", "transition-colors", "no-underline"]
        ]
        "CANCEL"
