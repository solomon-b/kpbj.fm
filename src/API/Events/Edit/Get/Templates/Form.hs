{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Events.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventEditPostLink, eventGetLink, eventsGetLink, mediaGetLink)
import Component.Form.Builder
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.EventTags qualified as EventTags
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
eventsGetUrl :: Links.URI
eventsGetUrl = Links.linkURI $ eventsGetLink Nothing Nothing

eventGetUrl :: Events.Id -> Slug -> Links.URI
eventGetUrl eventId slug = Links.linkURI $ eventGetLink eventId slug

eventEditPostUrl :: Events.Id -> Slug -> Links.URI
eventEditPostUrl eventId slug = Links.linkURI $ eventEditPostLink eventId slug

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI mediaGetLink

--------------------------------------------------------------------------------

-- | Format UTCTime to HTML5 datetime-local format (YYYY-MM-DDTHH:MM)
formatDateTimeLocal :: UTCTime -> Text
formatDateTimeLocal = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M"

--------------------------------------------------------------------------------

-- | Event edit template using FormBuilder
template :: Events.Model -> [EventTags.Model] -> UserMetadata.Model -> Lucid.Html ()
template event tags userMeta = do
  let eventId = event.emId
      eventSlug = event.emSlug
      eventBackUrl = eventGetUrl eventId eventSlug
      eventEditUrl = eventEditPostUrl eventId eventSlug
      tagsText = Text.intercalate ", " $ map (\t -> t.etmName) tags

  buildValidatedForm
    FormBuilder
      { fbAction = [i|/#{eventEditUrl}|],
        fbMethod = "post",
        fbHeader = Just (renderFormHeader event userMeta eventBackUrl),
        fbFields = eventEditFormFields event tagsText,
        fbAdditionalContent = [renderSubmitActions eventBackUrl],
        fbStyles = defaultFormStyles
      }

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: Events.Model -> UserMetadata.Model -> Links.URI -> Lucid.Html ()
renderFormHeader event userMeta eventBackUrl = do
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "EDIT EVENT"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          Lucid.strong_ "Event: "
          Lucid.toHtml event.emTitle
          " • "
          Lucid.strong_ "Editor: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [Lucid.class_ "space-x-4"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{eventBackUrl}|],
            hxGet_ [i|/#{eventBackUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "← BACK TO EVENT"
        Lucid.a_
          [ Lucid.href_ [i|/#{eventsGetUrl}|],
            hxGet_ [i|/#{eventsGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "VIEW EVENTS"

--------------------------------------------------------------------------------
-- Form Fields Definition

eventEditFormFields :: Events.Model -> Text -> [FormField]
eventEditFormFields event tagsText =
  [ -- Event Details Section
    SectionField
      { sfTitle = "EVENT DETAILS",
        sfFields =
          [ ValidatedTextField
              { vfName = "title",
                vfLabel = "Event Title",
                vfInitialValue = Just event.emTitle,
                vfPlaceholder = Just "e.g. KPBJ Spring Concert",
                vfHint = Nothing,
                vfValidation =
                  emptyValidation
                    { vrMinLength = Just 3,
                      vrMaxLength = Just 200,
                      vrRequired = True
                    }
              },
            ValidatedTextField
              { vfName = "tags",
                vfLabel = "Event Tags",
                vfInitialValue = Just tagsText,
                vfPlaceholder = Just "live-music, concert, fundraiser, community",
                vfHint = Just "Comma separated tags",
                vfValidation =
                  emptyValidation
                    { vrMaxLength = Just 500
                    }
              },
            ValidatedTextareaField
              { vtName = "description",
                vtLabel = "Event Description",
                vtInitialValue = Just event.emDescription,
                vtRows = 8,
                vtPlaceholder = Just "Describe your event. Include any special details, what attendees should expect, what to bring, etc.",
                vtHint = Nothing,
                vtValidation =
                  emptyValidation
                    { vrMinLength = Just 10,
                      vrMaxLength = Just 5000,
                      vrRequired = True
                    }
              },
            ValidatedFileField
              { vffName = "poster_image",
                vffLabel = "Event Poster Image",
                vffAccept = Just "image/*",
                vffHint = Just "Optional poster image for the event. Recommended: 1200x630px or larger. Leave empty to keep current image.",
                vffMaxSizeMB = Just 10,
                vffValidation = emptyValidation {vrRequired = False},
                vffButtonText = "Choose New Poster Image",
                vffButtonClasses = "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700",
                vffCurrentValue = case event.emPosterImageUrl of
                  Just imageUrl -> Just [i|/#{mediaGetUrl}/#{imageUrl}|]
                  Nothing -> Nothing
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
                vdtInitialValue = Just (formatDateTimeLocal event.emStartsAt),
                vdtHint = Nothing,
                vdtValidation = emptyValidation {vrRequired = True}
              },
            ValidatedDateTimeField
              { vdtName = "ends_at",
                vdtLabel = "End Date & Time",
                vdtInitialValue = Just (formatDateTimeLocal event.emEndsAt),
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
                vfInitialValue = Just event.emLocationName,
                vfPlaceholder = Just "e.g. Shadow Hills Community Center",
                vfHint = Nothing,
                vfValidation =
                  emptyValidation
                    { vrMinLength = Just 3,
                      vrMaxLength = Just 200,
                      vrRequired = True
                    }
              },
            ValidatedTextField
              { vfName = "location_address",
                vfLabel = "Address",
                vfInitialValue = Just event.emLocationAddress,
                vfPlaceholder = Just "e.g. 1247 Underground Ave, Shadow Hills, CA",
                vfHint = Nothing,
                vfValidation =
                  emptyValidation
                    { vrMinLength = Just 3,
                      vrMaxLength = Just 500,
                      vrRequired = True
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
                  [ SelectOption "draft" "Save as Draft" (event.emStatus == Events.Draft) (Just "Keep private until you're ready to publish"),
                    SelectOption "published" "Publish Immediately" (event.emStatus == Events.Published) (Just "Make visible to the public right away")
                  ],
                vrfHint = Nothing,
                vrfValidation = emptyValidation {vrRequired = True}
              }
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Form Submit Actions (rendered inside <form>)

renderSubmitActions :: Links.URI -> Lucid.Html ()
renderSubmitActions eventBackUrl =
  Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.button_
        [ Lucid.type_ "submit",
          Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors"
        ]
        "UPDATE EVENT"
      Lucid.a_
        [ Lucid.href_ [i|/#{eventBackUrl}|],
          hxGet_ [i|/#{eventBackUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline"
        ]
        "CANCEL"
