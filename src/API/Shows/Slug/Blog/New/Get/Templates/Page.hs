{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Blog.New.Get.Templates.Page
  ( newBlogPostForm,
    notLoggedInTemplate,
    errorTemplate,
  )
where

--------------------------------------------------------------------------------

import API.Links (showBlogLinks, showsLinks)
import API.Types
import Component.Form.Builder
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

showBlogGetUrl :: Shows.Model -> Links.URI
showBlogGetUrl showModel = Links.linkURI $ showBlogLinks.list (Shows.slug showModel) Nothing Nothing

showBlogNewPostUrl :: Shows.Model -> Links.URI
showBlogNewPostUrl showModel = Links.linkURI $ showBlogLinks.newPost (Shows.slug showModel)

showGetUrl :: Shows.Model -> Links.URI
showGetUrl showModel = Links.linkURI $ showsLinks.detail (Shows.slug showModel) Nothing

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
        fbAdditionalContent = [renderSubmitActions showModel, renderStatusToggleScript],
        fbStyles = defaultFormStyles,
        fbHtmx = Nothing
      }

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: Shows.Model -> Lucid.Html ()
renderFormHeader showModel =
  Lucid.section_ [class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "NEW BLOG POST"
        Lucid.div_ [class_ $ base ["text-gray-300", Tokens.textSm]] $ do
          Lucid.strong_ "Show: "
          Lucid.toHtml (Shows.title showModel)
      Lucid.div_ [class_ $ base ["text-center", "flex", Tokens.gap4]] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{showGetUrl showModel}|],
            hxGet_ [i|/#{showGetUrl showModel}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "VIEW SHOW"
        Lucid.a_
          [ Lucid.href_ [i|/#{showBlogGetUrl showModel}|],
            hxGet_ [i|/#{showBlogGetUrl showModel}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "VIEW BLOG"

--------------------------------------------------------------------------------
-- Form Fields Definition

showBlogFormFields :: [FormField]
showBlogFormFields =
  [ -- Hidden status field (controlled by toggle)
    HiddenField
      { hfName = "status",
        hfValue = "draft"
      },
    -- Post Details Section
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
renderSubmitActions _showModel =
  Lucid.section_ [class_ $ base [Tokens.bgGray100, Tokens.border2, "border-gray-400", Tokens.p6]] $ do
    Lucid.div_ [class_ $ base ["flex", "justify-end", "items-center"]] $ do
      Lucid.div_ [class_ $ base ["flex", Tokens.gap4, "items-center"]] $ do
        -- Status toggle switch
        Lucid.div_ [class_ $ base ["flex", "items-center", "gap-3"]] $ do
          Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.textGray600]] "Draft"
          Lucid.label_ [class_ $ base ["relative", "inline-flex", "items-center", "cursor-pointer"]] $ do
            Lucid.input_
              [ Lucid.type_ "checkbox",
                Lucid.id_ "status-toggle",
                Lucid.class_ "sr-only peer"
              ]
            Lucid.div_
              [ Lucid.class_ $
                  "w-11 h-6 bg-gray-300 peer-focus:outline-none peer-focus:ring-2 "
                    <> "peer-focus:ring-blue-300 rounded-full peer "
                    <> "peer-checked:after:translate-x-full peer-checked:after:border-white "
                    <> "after:content-[''] after:absolute after:top-[2px] after:left-[2px] "
                    <> "after:bg-white after:border-gray-300 after:border after:rounded-full "
                    <> "after:h-5 after:w-5 after:transition-all peer-checked:bg-green-600"
              ]
              mempty
          Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.textGray600]] "Published"
        Lucid.button_
          [ Lucid.type_ "submit",
            class_ $ base ["bg-blue-600", Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-blue-700"]
          ]
          "SUBMIT"

-- | JavaScript for status toggle
renderStatusToggleScript :: Lucid.Html ()
renderStatusToggleScript =
  Lucid.script_
    [i|
(function() {
  const statusToggle = document.getElementById('status-toggle');
  const statusField = document.querySelector('input[name="status"]');
  statusToggle?.addEventListener('change', () => {
    if (statusField) {
      statusField.value = statusToggle.checked ? 'published' : 'draft';
    }
  });
})();
|]

--------------------------------------------------------------------------------
-- Error Templates

notLoggedInTemplate :: Lucid.Html ()
notLoggedInTemplate = do
  Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, "p-8", "text-center"]] $ do
    Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "Authentication Required"
    Lucid.p_ [class_ $ base [Tokens.mb4, Tokens.textGray600]] "You must be logged in to create blog posts."

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [class_ $ base [Tokens.errorBg, Tokens.border2, Tokens.errorBorder, Tokens.p6]] $ do
    Lucid.h2_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2, Tokens.errorText]] "Error"
    Lucid.p_ [Lucid.class_ "text-red-700"] $ Lucid.toHtml errorMsg
