{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Events.New.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardEventsLinks)
import API.Types (DashboardEventsRoutes (..))
import Component.Form.Builder
import Data.String.Interpolate (i)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardEventsGetUrl :: Links.URI
dashboardEventsGetUrl = Links.linkURI $ dashboardEventsLinks.list Nothing

dashboardEventsNewPostUrl :: Links.URI
dashboardEventsNewPostUrl = Links.linkURI dashboardEventsLinks.newPost

--------------------------------------------------------------------------------

-- | New event form template using FormBuilder
template :: UserMetadata.Model -> Lucid.Html ()
template userMeta = do
  buildValidatedForm
    FormBuilder
      { fbAction = [i|/#{dashboardEventsNewPostUrl}|],
        fbMethod = "post",
        fbHeader = Just (renderFormHeader userMeta),
        fbFields = eventFormFields,
        fbAdditionalContent = [renderSubmitActions],
        fbStyles = defaultFormStyles,
        fbHtmx = Nothing
      }

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: UserMetadata.Model -> Lucid.Html ()
renderFormHeader userMeta =
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "NEW EVENT"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          Lucid.strong_ "Organizer: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [Lucid.class_ "text-center"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardEventsGetUrl}|],
            hxGet_ [i|/#{dashboardEventsGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "VIEW EVENTS"

--------------------------------------------------------------------------------
-- Form Fields Definition

eventFormFields :: [FormField]
eventFormFields =
  [ -- Event Details Section
    SectionField
      { sfTitle = "EVENT DETAILS",
        sfFields =
          [ ValidatedTextField
              { vfName = "title",
                vfLabel = "Event Title",
                vfInitialValue = Nothing,
                vfPlaceholder = Just "e.g. KPBJ Spring Concert",
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
            ValidatedTextField
              { vfName = "tags",
                vfLabel = "Event Tags",
                vfInitialValue = Nothing,
                vfPlaceholder = Just "live-music, concert, fundraiser, community",
                vfHint = Just "Comma separated tags",
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
              { vtName = "description",
                vtLabel = "Event Description",
                vtInitialValue = Nothing,
                vtRows = 8,
                vtPlaceholder = Just "Describe your event. Include any special details, what attendees should expect, what to bring, etc.",
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
            ValidatedFileField
              { vffName = "poster_image",
                vffLabel = "Event Poster Image",
                vffAccept = Just "image/*",
                vffHint = Just "Optional poster image for the event. Recommended: 1200x630px or larger.",
                vffMaxSizeMB = Just 10,
                vffValidation = emptyValidation {vrRequired = False},
                vffButtonText = "Choose Poster Image",
                vffButtonClasses = "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700",
                vffCurrentValue = Nothing
              }
          ]
      },
    -- Date & Time Section
    SectionField
      { sfTitle = "DATE & TIME",
        sfFields =
          [ ValidatedDateTimeField
              { vdtName = "starts_at",
                vdtLabel = "Start Date & Time",
                vdtInitialValue = Nothing,
                vdtHint = Nothing,
                vdtValidation = emptyValidation {vrRequired = True}
              },
            ValidatedDateTimeField
              { vdtName = "ends_at",
                vdtLabel = "End Date & Time",
                vdtInitialValue = Nothing,
                vdtHint = Just "Must be after start time",
                vdtValidation =
                  emptyValidation
                    { vrRequired = True,
                      vrCustomValidation = Just "new Date(this.fields.ends_at.value) > new Date(this.fields.starts_at.value)"
                    }
              }
          ]
      },
    -- Location Section
    SectionField
      { sfTitle = "LOCATION",
        sfFields =
          [ ValidatedTextField
              { vfName = "location_name",
                vfLabel = "Venue Name",
                vfInitialValue = Nothing,
                vfPlaceholder = Just "e.g. Shadow Hills Community Center",
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
            ValidatedTextField
              { vfName = "location_address",
                vfLabel = "Address",
                vfInitialValue = Nothing,
                vfPlaceholder = Just "e.g. 1247 Underground Ave, Shadow Hills, CA",
                vfHint = Nothing,
                vfValidation =
                  ValidationRules
                    { vrMinLength = Just 3,
                      vrMaxLength = Just 500,
                      vrPattern = Nothing,
                      vrRequired = True,
                      vrCustomValidation = Nothing
                    }
              }
          ]
      },
    -- Publishing Section
    SectionField
      { sfTitle = "PUBLISHING",
        sfFields =
          [ ValidatedRadioField
              { vrfName = "status",
                vrfLabel = "",
                vrfOptions =
                  [ SelectOption "draft" "Save as Draft" True (Just "Keep private until you're ready to publish"),
                    SelectOption "published" "Publish Immediately" False (Just "Make visible to the public right away")
                  ],
                vrfHint = Nothing,
                vrfValidation = emptyValidation {vrRequired = True}
              }
          ]
      }
  ]

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
        "CREATE EVENT"
      Lucid.a_
        [ Lucid.href_ [i|/#{dashboardEventsGetUrl}|],
          hxGet_ [i|/#{dashboardEventsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline"
        ]
        "CANCEL"
