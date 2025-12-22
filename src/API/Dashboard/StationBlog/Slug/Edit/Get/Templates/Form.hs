{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StationBlog.Slug.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, dashboardStationBlogLinks)
import API.Types (DashboardStationBlogRoutes (..), Routes (..))
import Component.Form.Builder
import Component.ImageFilePicker qualified as ImageFilePicker
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
dashboardStationBlogGetUrl :: Links.URI
dashboardStationBlogGetUrl = Links.linkURI $ dashboardStationBlogLinks.list Nothing

dashboardStationBlogDetailGetUrl :: BlogPosts.Id -> Slug -> Links.URI
dashboardStationBlogDetailGetUrl postId slug = Links.linkURI $ dashboardStationBlogLinks.detail postId slug

dashboardStationBlogEditPostUrl :: BlogPosts.Id -> Slug -> Links.URI
dashboardStationBlogEditPostUrl postId slug = Links.linkURI $ dashboardStationBlogLinks.editPost postId slug

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

--------------------------------------------------------------------------------

-- | Blog post edit template using FormBuilder
template :: BlogPosts.Model -> [BlogTags.Model] -> UserMetadata.Model -> Lucid.Html ()
template blogPost tags userMeta = do
  let postId = blogPost.bpmId
      postSlug = blogPost.bpmSlug
      postBackUrl = dashboardStationBlogDetailGetUrl postId postSlug
      postEditUrl = dashboardStationBlogEditPostUrl postId postSlug
      tagsText = Text.intercalate ", " $ map (\t -> t.btmName) tags

  buildValidatedForm
    FormBuilder
      { fbAction = [i|/#{postEditUrl}|],
        fbMethod = "post",
        fbHeader = Just (renderFormHeader blogPost userMeta postBackUrl),
        fbFields = blogEditFormFields blogPost tagsText,
        fbAdditionalContent = [renderSubmitActions postBackUrl],
        fbStyles = defaultFormStyles,
        fbHtmx = Nothing
      }

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: BlogPosts.Model -> UserMetadata.Model -> Links.URI -> Lucid.Html ()
renderFormHeader blogPost userMeta postBackUrl =
  Lucid.section_ [class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "EDIT BLOG POST"
        Lucid.div_ [class_ $ base ["text-gray-300", Tokens.textSm]] $ do
          Lucid.strong_ "Post: "
          Lucid.toHtml blogPost.bpmTitle
          " • "
          Lucid.strong_ "Editor: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [class_ $ base ["space-x-4"]] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{postBackUrl}|],
            hxGet_ [i|/#{postBackUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "<- BACK TO POST"
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardStationBlogGetUrl}|],
            hxGet_ [i|/#{dashboardStationBlogGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "VIEW ALL POSTS"

--------------------------------------------------------------------------------
-- Form Fields Definition

blogEditFormFields :: BlogPosts.Model -> Text -> [FormField]
blogEditFormFields blogPost tagsText =
  [ -- Post Details Section
    SectionField
      { sfTitle = "POST DETAILS",
        sfFields =
          [ ValidatedTextField
              { vfName = "title",
                vfLabel = "Post Title",
                vfInitialValue = Just blogPost.bpmTitle,
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
                vtInitialValue = Just blogPost.bpmContent,
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
              { pffHtml = heroImageField blogPost
              },
            ValidatedTextField
              { vfName = "tags",
                vfLabel = "Tags",
                vfInitialValue = Just tagsText,
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
                  [ SelectOption "published" "Publish Immediately" (blogPost.bpmStatus == Published) Nothing,
                    SelectOption "draft" "Save as Draft" (blogPost.bpmStatus == Draft) Nothing
                  ],
                vsHint = Nothing,
                vsValidation = emptyValidation {vrRequired = True}
              },
            ValidatedTextareaField
              { vtName = "excerpt",
                vtLabel = "Excerpt (Optional)",
                vtInitialValue = blogPost.bpmExcerpt,
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
heroImageField :: BlogPosts.Model -> Lucid.Html ()
heroImageField blogPost =
  let imageUrl = maybe "" (\path -> [i|/#{mediaGetUrl}/#{path}|]) blogPost.bpmHeroImageUrl
   in ImageFilePicker.render
        ImageFilePicker.Config
          { ImageFilePicker.fieldName = "hero_image",
            ImageFilePicker.label = "Hero Image",
            ImageFilePicker.existingImageUrl = imageUrl,
            ImageFilePicker.accept = "image/*",
            ImageFilePicker.maxSizeMB = 10,
            ImageFilePicker.isRequired = False,
            ImageFilePicker.aspectRatio = (16, 9)
          }

--------------------------------------------------------------------------------
-- Form Submit Actions (rendered inside <form>)

renderSubmitActions :: Links.URI -> Lucid.Html ()
renderSubmitActions postBackUrl =
  Lucid.section_ [class_ $ base ["bg-gray-50", Tokens.border2, "border-gray-300", Tokens.p6]] $ do
    Lucid.div_ [class_ $ base ["flex", Tokens.gap4, "justify-center"]] $ do
      Lucid.button_
        [ Lucid.type_ "submit",
          class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.px8, "py-3", Tokens.fontBold, "hover:bg-gray-700", "transition-colors"]
        ]
        "UPDATE POST"
      Lucid.a_
        [ Lucid.href_ [i|/#{postBackUrl}|],
          hxGet_ [i|/#{postBackUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base ["bg-gray-400", Tokens.textWhite, Tokens.px8, "py-3", Tokens.fontBold, "hover:bg-gray-500", "transition-colors", "no-underline"]
        ]
        "CANCEL"
