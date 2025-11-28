{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogEditPostLink, blogGetLink, blogPostGetLink, mediaGetLink)
import Component.Form.Builder
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
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
blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing

blogPostGetUrl :: BlogPosts.Id -> Slug -> Links.URI
blogPostGetUrl postId slug = Links.linkURI $ blogPostGetLink postId slug

blogEditPostUrl :: BlogPosts.Id -> Slug -> Links.URI
blogEditPostUrl postId slug = Links.linkURI $ blogEditPostLink postId slug

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI mediaGetLink

--------------------------------------------------------------------------------

-- | Blog post edit template using FormBuilder
template :: BlogPosts.Model -> [BlogTags.Model] -> UserMetadata.Model -> Lucid.Html ()
template blogPost tags userMeta = do
  let postId = blogPost.bpmId
      postSlug = blogPost.bpmSlug
      postBackUrl = blogPostGetUrl postId postSlug
      postEditUrl = blogEditPostUrl postId postSlug
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
            ValidatedFileField
              { vffName = "hero_image",
                vffLabel = "Hero Image",
                vffAccept = Just "image/*",
                vffHint = Just "Banner image displayed at top of post. Recommended: 1200x630px or larger. Leave empty to keep current image.",
                vffMaxSizeMB = Just 10,
                vffValidation = emptyValidation {vrRequired = False},
                vffButtonText = "Choose New Image",
                vffButtonClasses = "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700",
                vffCurrentValue = case blogPost.bpmHeroImageUrl of
                  Just imageUrl -> Just [i|/#{mediaGetUrl}/#{imageUrl}|]
                  Nothing -> Nothing
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
-- Form Submit Actions (rendered inside <form>)

renderSubmitActions :: Links.URI -> Lucid.Html ()
renderSubmitActions postBackUrl =
  Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.button_
        [ Lucid.type_ "submit",
          Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors"
        ]
        "UPDATE POST"
      Lucid.a_
        [ Lucid.href_ [i|/#{postBackUrl}|],
          hxGet_ [i|/#{postBackUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline"
        ]
        "CANCEL"
