{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StationBlog.New.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStationBlogLinks)
import API.Types (DashboardStationBlogRoutes (..))
import Component.Form.Builder
import Data.String.Interpolate (i)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Lucid.Responsive (cls)
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
  Lucid.section_ [Lucid.class_ $ cls [Tokens.bgGray800, Tokens.textWhite, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [Lucid.class_ $ cls ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ $ cls [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "NEW BLOG POST"
        Lucid.div_ [Lucid.class_ $ cls ["text-gray-300", Tokens.textSm]] $ do
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
            ValidatedFileField
              { vffName = "hero_image",
                vffLabel = "Hero Image (Optional)",
                vffAccept = Just "image/*",
                vffHint = Just "Banner image displayed at top of post. Recommended: 1200x630px or larger",
                vffMaxSizeMB = Just 10,
                vffValidation = emptyValidation {vrRequired = False},
                vffButtonText = "Choose Image",
                vffButtonClasses = "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700",
                vffCurrentValue = Nothing
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
-- Form Submit Actions (rendered inside <form>)

renderSubmitActions :: Lucid.Html ()
renderSubmitActions =
  Lucid.section_ [Lucid.class_ $ cls ["bg-gray-50", Tokens.border2, "border-gray-300", Tokens.p6]] $ do
    Lucid.div_ [Lucid.class_ $ cls ["flex", Tokens.gap4, "justify-center"]] $ do
      Lucid.button_
        [ Lucid.type_ "submit",
          Lucid.class_ $ cls [Tokens.bgGray800, Tokens.textWhite, Tokens.px8, "py-3", Tokens.fontBold, "hover:bg-gray-700", "transition-colors"]
        ]
        "SAVE POST"
      Lucid.a_
        [ Lucid.href_ [i|/#{dashboardStationBlogGetUrl}|],
          hxGet_ [i|/#{dashboardStationBlogGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ $ cls ["bg-gray-400", Tokens.textWhite, Tokens.px8, "py-3", Tokens.fontBold, "hover:bg-gray-500", "transition-colors", "no-underline"]
        ]
        "CANCEL"
