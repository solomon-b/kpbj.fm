{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Admin.Shows.New.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (adminShowsGetLink, adminShowsNewPostLink)
import Component.Form.Builder
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xBindClass_, xData_, xModel_, xRef_, xShow_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

adminShowsGetUrl :: Links.URI
adminShowsGetUrl = Links.linkURI $ adminShowsGetLink Nothing Nothing Nothing

adminShowsNewPostUrl :: Links.URI
adminShowsNewPostUrl = Links.linkURI adminShowsNewPostLink

--------------------------------------------------------------------------------

-- | New show form template using FormBuilder
template :: [UserMetadata.UserWithMetadata] -> Lucid.Html ()
template eligibleHosts =
  buildValidatedForm
    FormBuilder
      { fbAction = [i|/#{adminShowsNewPostUrl}|],
        fbMethod = "post",
        fbHeader = Just renderFormHeader,
        fbFields = showFormFields eligibleHosts,
        fbAdditionalContent = [renderSubmitActions],
        fbStyles = defaultFormStyles,
        fbHtmx = Nothing
      }

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: Lucid.Html ()
renderFormHeader =
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "CREATE NEW SHOW"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $
          "Add a new show to the station"
      Lucid.div_ [Lucid.class_ "text-center"] $
        Lucid.a_
          [ Lucid.href_ [i|/#{adminShowsGetUrl}|],
            hxGet_ [i|/#{adminShowsGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "← BACK TO SHOWS"

--------------------------------------------------------------------------------
-- Form Fields Definition

showFormFields :: [UserMetadata.UserWithMetadata] -> [FormField]
showFormFields eligibleHosts =
  [ -- Basic Information Section
    SectionField
      { sfTitle = "BASIC INFORMATION",
        sfFields =
          [ ValidatedTextField
              { vfName = "title",
                vfLabel = "Show Title",
                vfInitialValue = Nothing,
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
                vtInitialValue = Nothing,
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
                vfInitialValue = Nothing,
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
    -- Schedule & Settings Section
    SectionField
      { sfTitle = "SCHEDULE & SETTINGS",
        sfFields =
          [ ValidatedSelectField
              { vsName = "status",
                vsLabel = "Show Status",
                vsOptions =
                  [ SelectOption "active" "Active" True Nothing,
                    SelectOption "inactive" "Inactive" False Nothing
                  ],
                vsHint = Just "Active shows appear on the shows page",
                vsValidation = emptyValidation {vrRequired = True}
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
                vffValidation = emptyValidation,
                vffButtonText = "CHOOSE LOGO IMAGE",
                vffButtonClasses = "bg-purple-600 text-white px-6 py-3 font-bold hover:bg-purple-700 inline-block",
                vffCurrentValue = Nothing
              },
            ValidatedFileField
              { vffName = "banner_file",
                vffLabel = "Banner Image",
                vffAccept = Just "image/jpeg,image/png,image/webp,image/gif",
                vffHint = Just "JPG, PNG, WebP, GIF accepted • Max 10MB • Recommended: 1200x300px",
                vffMaxSizeMB = Just 10,
                vffValidation = emptyValidation,
                vffButtonText = "CHOOSE BANNER IMAGE",
                vffButtonClasses = "bg-purple-600 text-white px-6 py-3 font-bold hover:bg-purple-700 inline-block",
                vffCurrentValue = Nothing
              }
          ]
      },
    -- Hosts Section (admin only - using PlainField for multi-select)
    SectionField
      { sfTitle = "HOSTS",
        sfFields =
          [ PlainField
              { pfHtml = renderHostsMultiSelect eligibleHosts
              }
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Searchable Multi-Select for Hosts

renderHostsMultiSelect :: [UserMetadata.UserWithMetadata] -> Lucid.Html ()
renderHostsMultiSelect eligibleHosts = do
  Lucid.div_ [xData_ "{ search: '' }"] $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Assign Hosts"
    Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mb-2"] "Select one or more hosts for this show. Regular users will be automatically promoted to Host role."

    -- Search input
    Lucid.div_ [Lucid.class_ "mb-2"] $ do
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.placeholder_ "Search by name or email...",
          Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
          xModel_ "search"
        ]

    -- Results container
    Lucid.div_ [Lucid.class_ "bg-gray-100 border-2 border-gray-300"] $ do
      -- Header
      Lucid.div_ [Lucid.class_ "bg-gray-200 border-b border-gray-400 p-3 font-bold text-sm"] $
        Lucid.toHtml ("AVAILABLE HOSTS (" <> show (length eligibleHosts) <> ")")

      -- Scrollable host list
      Lucid.div_ [Lucid.class_ "max-h-64 overflow-y-auto"] $
        mapM_ renderHostOption eligibleHosts

renderHostOption :: UserMetadata.UserWithMetadata -> Lucid.Html ()
renderHostOption user =
  let userId = user.uwmUserId
      displayName = display user.uwmDisplayName
      email = display user.uwmEmail
      roleText = display user.uwmUserRole
      userIdText = display userId
      -- Alpine.js filter condition - check if search matches name or email
      filterCondition =
        [i|search === '' || '#{displayName}'.toLowerCase().includes(search.toLowerCase()) || '#{email}'.toLowerCase().includes(search.toLowerCase())|]
   in Lucid.div_
        [ Lucid.class_ "border-b border-gray-300 p-3 hover:bg-gray-200 cursor-pointer",
          xShow_ filterCondition,
          xBindClass_ "{ 'bg-blue-50': $refs.host_#{userIdText}?.checked }"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "flex items-center"] $ do
            Lucid.input_
              [ Lucid.type_ "checkbox",
                Lucid.name_ "hosts",
                Lucid.id_ [i|host_#{userIdText}|],
                Lucid.value_ userIdText,
                Lucid.class_ "mr-3",
                xRef_ [i|host_#{userIdText}|]
              ]
            Lucid.label_ [Lucid.for_ [i|host_#{userIdText}|], Lucid.class_ "flex-1 cursor-pointer"] $ do
              Lucid.div_ [Lucid.class_ "font-bold"] $ Lucid.toHtml displayName
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] $
                Lucid.toHtml (email <> " • " <> roleText)

--------------------------------------------------------------------------------
-- Form Submit Actions (rendered inside <form>)

renderSubmitActions :: Lucid.Html ()
renderSubmitActions =
  Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.button_
        [ Lucid.type_ "submit",
          Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors"
        ]
        "CREATE SHOW"
      Lucid.a_
        [ Lucid.href_ [i|/#{adminShowsGetUrl}|],
          hxGet_ [i|/#{adminShowsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline"
        ]
        "CANCEL"
