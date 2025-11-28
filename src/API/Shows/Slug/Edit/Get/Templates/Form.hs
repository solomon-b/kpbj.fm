{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (hostDashboardGetLink, mediaGetLink, showGetLink)
import Component.Form.Builder
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI $ hostDashboardGetLink Nothing

showGetUrl :: Slug -> Links.URI
showGetUrl slug = Links.linkURI $ showGetLink slug

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI mediaGetLink

--------------------------------------------------------------------------------

-- | Show edit template using FormBuilder
template :: Shows.Model -> UserMetadata.Model -> Bool -> Lucid.Html ()
template showModel userMeta isStaff = do
  let showSlug = showModel.slug
  buildValidatedForm
    FormBuilder
      { fbAction = [i|/shows/#{display showSlug}/edit|],
        fbMethod = "post",
        fbHeader = Just (renderFormHeader userMeta showModel),
        fbFields = showEditFormFields showModel isStaff,
        fbAdditionalContent = [renderSubmitActions showSlug],
        fbStyles = defaultFormStyles,
        fbHtmx = Nothing
      }

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: UserMetadata.Model -> Shows.Model -> Lucid.Html ()
renderFormHeader userMeta showModel = do
  let showBackUrl = showGetUrl showModel.slug
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "EDIT SHOW"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          Lucid.strong_ "Show: "
          Lucid.toHtml showModel.title
          " • "
          Lucid.strong_ "Editor: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [Lucid.class_ "space-x-4"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{showBackUrl}|],
            hxGet_ [i|/#{showBackUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "← BACK TO SHOW"
        Lucid.a_
          [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
            hxGet_ [i|/#{hostDashboardGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "DASHBOARD"

--------------------------------------------------------------------------------
-- Form Fields Definition

showEditFormFields :: Shows.Model -> Bool -> [FormField]
showEditFormFields showModel isStaff =
  [ -- Basic Information Section
    SectionField
      { sfTitle = "BASIC INFORMATION",
        sfFields =
          [ ValidatedTextField
              { vfName = "title",
                vfLabel = "Show Title",
                vfInitialValue = Just showModel.title,
                vfPlaceholder = Just "e.g. Industrial Depths",
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
              { vtName = "description",
                vtLabel = "Description",
                vtInitialValue = Just showModel.description,
                vtRows = 6,
                vtPlaceholder = Just "Describe your show. What kind of music do you play? What's your show's vibe?",
                vtHint = Nothing,
                vtValidation =
                  ValidationRules
                    { vrMinLength = Just 10,
                      vrMaxLength = Just 5000,
                      vrPattern = Nothing,
                      vrRequired = True,
                      vrCustomValidation = Nothing
                    }
              },
            ValidatedTextField
              { vfName = "genre",
                vfLabel = "Genre",
                vfInitialValue = fmap display showModel.genre,
                vfPlaceholder = Just "e.g. Techno, Ambient, Experimental, Hip-Hop",
                vfHint = Just "Primary genre or style of music",
                vfValidation =
                  ValidationRules
                    { vrMinLength = Nothing,
                      vrMaxLength = Just 100,
                      vrPattern = Nothing,
                      vrRequired = False,
                      vrCustomValidation = Nothing
                    }
              }
          ]
      },
    -- Schedule & Settings Section (conditional on isStaff)
    ConditionalField
      { cfCondition = isStaff,
        cfTrueFields =
          [ SectionField
              { sfTitle = "SCHEDULE & SETTINGS",
                sfFields =
                  [ ValidatedSelectField
                      { vsName = "status",
                        vsLabel = "Show Status",
                        vsOptions =
                          [ SelectOption "active" "Active" (showModel.status == Shows.Active) Nothing,
                            SelectOption "inactive" "Inactive" (showModel.status == Shows.Inactive) Nothing
                          ],
                        vsHint = Just "Active shows appear on the shows page",
                        vsValidation = emptyValidation {vrRequired = True}
                      },
                    ValidatedTextField
                      { vfName = "duration_minutes",
                        vfLabel = "Typical Duration (minutes)",
                        vfInitialValue = fmap (Text.pack . show) showModel.durationMinutes,
                        vfPlaceholder = Just "e.g. 120",
                        vfHint = Just "How long is a typical episode?",
                        vfValidation =
                          ValidationRules
                            { vrMinLength = Nothing,
                              vrMaxLength = Nothing,
                              vrPattern = Just "[0-9]+",
                              vrRequired = False,
                              vrCustomValidation = Nothing
                            }
                      }
                  ]
              }
          ],
        cfFalseFields =
          [ -- Hidden fields to preserve existing values for non-staff
            HiddenField
              { hfName = "status",
                hfValue = case showModel.status of
                  Shows.Active -> "active"
                  Shows.Inactive -> "inactive"
              },
            HiddenField
              { hfName = "duration_minutes",
                hfValue = maybe "" (Text.pack . show) showModel.durationMinutes
              }
          ]
      },
    -- Artwork & Branding Section
    SectionField
      { sfTitle = "ARTWORK & BRANDING",
        sfFields =
          [ ValidatedFileField
              { vffName = "logo_file",
                vffLabel = "Logo Image",
                vffAccept = Just "image/jpeg,image/png,image/webp,image/gif",
                vffHint = Just "JPG, PNG, WebP, GIF accepted • Max 10MB • Recommended: 300x300px",
                vffMaxSizeMB = Just 10,
                vffValidation = emptyValidation, -- Optional
                vffButtonText = "🖼️ CHOOSE LOGO IMAGE",
                vffButtonClasses = "bg-purple-600 text-white px-6 py-3 font-bold hover:bg-purple-700 inline-block",
                vffCurrentValue = fmap (\url -> [i|/#{mediaGetUrl}/#{url}|]) showModel.logoUrl
              },
            ValidatedFileField
              { vffName = "banner_file",
                vffLabel = "Banner Image",
                vffAccept = Just "image/jpeg,image/png,image/webp,image/gif",
                vffHint = Just "JPG, PNG, WebP, GIF accepted • Max 10MB • Recommended: 1200x300px",
                vffMaxSizeMB = Just 10,
                vffValidation = emptyValidation, -- Optional
                vffButtonText = "🖼️ CHOOSE BANNER IMAGE",
                vffButtonClasses = "bg-purple-600 text-white px-6 py-3 font-bold hover:bg-purple-700 inline-block",
                vffCurrentValue = fmap (\url -> [i|/#{mediaGetUrl}/#{url}|]) showModel.bannerUrl
              }
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Form Submit Actions (rendered inside <form>)

renderSubmitActions :: Slug -> Lucid.Html ()
renderSubmitActions showSlug = do
  let showBackUrl = showGetUrl showSlug
  Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.button_
        [ Lucid.type_ "submit",
          Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors"
        ]
        "UPDATE SHOW"
      Lucid.a_
        [ Lucid.href_ [i|/#{showBackUrl}|],
          hxGet_ [i|/#{showBackUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline inline-block"
        ]
        "CANCEL"
