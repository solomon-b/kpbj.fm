{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Events.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventGetLink, eventsGetLink)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effects.Database.Tables.EventTags qualified as EventTags
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xBindClass_, xData_, xModel_, xOnClick_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
eventsGetUrl :: Links.URI
eventsGetUrl = Links.linkURI $ eventsGetLink Nothing Nothing

eventGetUrl :: Text -> Links.URI
eventGetUrl slug = Links.linkURI $ eventGetLink slug

--------------------------------------------------------------------------------

-- | Format UTCTime to HTML5 datetime-local format (YYYY-MM-DDTHH:MM)
formatDateTimeLocal :: UTCTime -> Text
formatDateTimeLocal = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M"

alpineState :: Events.Model -> Text
alpineState Events.Model {emTitle, emDescription, emStartsAt, emEndsAt, emLocationName, emLocationAddress} =
  let startsAtStr = formatDateTimeLocal emStartsAt
      endsAtStr = formatDateTimeLocal emEndsAt
   in [i|{
  fields: {
    title: { value: `#{emTitle}`, isValid: true },
    tags: { value: '', isValid: true },
    description: { value: `#{emDescription}`, isValid: true },
    startsAt: { value: `#{startsAtStr}`, isValid: true },
    endsAt: { value: `#{endsAtStr}`, isValid: true },
    locationName: { value: `#{emLocationName}`, isValid: true },
    locationAddress: { value: `#{emLocationAddress}`, isValid: true },
  },
  showErrors: false,

  validateAndSubmit(event) {
    this.showErrors = true;

    // Validate all required fields
    this.fields.title.isValid = this.fields.title.value.trim() !== '';
    this.fields.description.isValid = this.fields.description.value.trim() !== '';
    this.fields.startsAt.isValid = this.fields.startsAt.value.trim() !== '';
    this.fields.endsAt.isValid = this.fields.endsAt.value.trim() !== '';
    this.fields.locationName.isValid = this.fields.locationName.value.trim() !== '';
    this.fields.locationAddress.isValid = this.fields.locationAddress.value.trim() !== '';

    // Tags are optional
    this.fields.tags.isValid = true;

    // Check if all fields are valid
    const allFieldsValid = Object.values(this.fields).every(field => field.isValid);

    if (!allFieldsValid) {
      event.preventDefault();
      return false;
    }

    return true;
  }
}|]

--------------------------------------------------------------------------------

-- | Event edit template
template :: Events.Model -> [EventTags.Model] -> UserMetadata.Model -> Lucid.Html ()
template event tags userMeta = do
  let eventSlug = event.emSlug
      eventBackUrl = eventGetUrl eventSlug
      tagsText = Text.intercalate ", " $ map (\t -> t.etmName) tags

  Lucid.div_ [Lucid.class_ "max-w-2xl mx-auto", xData_ (alpineState event)] $ do
    header event userMeta eventBackUrl

    -- Edit Event Form
    Lucid.form_ [Lucid.action_ [i|/events/#{eventSlug}/edit|], Lucid.method_ "post", Lucid.class_ "space-y-8 w-full"] $ do
      -- Event Details
      Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "EVENT DETAILS"

        Lucid.div_ [Lucid.class_ "space-y-6"] $ do
          titleField event
          tagsField tagsText
          descriptionField event

      -- Date & Time
      Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "DATE & TIME"
        Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 gap-6"] $ do
          startsAtField event
          endsAtField event

      -- Location
      Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "LOCATION"

        Lucid.div_ [Lucid.class_ "space-y-6"] $ do
          locationNameField event
          locationAddressField event

      -- Publishing
      Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "PUBLISHING"
        statusField event

      -- Form Actions
      Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
        Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
          submitButton
          cancelButton eventBackUrl

selectedIf :: Bool -> Lucid.Attributes
selectedIf True = Lucid.selected_ "selected"
selectedIf False = mempty

header :: Events.Model -> UserMetadata.Model -> Links.URI -> Lucid.Html ()
header event userMeta eventBackUrl = do
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

titleField :: Events.Model -> Lucid.Html ()
titleField event =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Event Title *"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "title",
        Lucid.required_ "true",
        Lucid.value_ event.emTitle,
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        Lucid.placeholder_ "e.g. KPBJ Spring Concert",
        xModel_ "fields.title.value",
        xBindClass_ "showErrors && !fields.title.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

tagsField :: Text -> Lucid.Html ()
tagsField tagsText =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Event Tags"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "tags",
        Lucid.value_ tagsText,
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        Lucid.placeholder_ "live-music, concert, fundraiser, community",
        xModel_ "fields.tags.value"
      ]
    Lucid.div_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "Comma separated tags"

descriptionField :: Events.Model -> Lucid.Html ()
descriptionField event =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Event Description *"
    Lucid.textarea_
      [ Lucid.name_ "description",
        Lucid.required_ "true",
        Lucid.rows_ "8",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono leading-relaxed",
        Lucid.placeholder_ "Describe your event. Include any special details, what attendees should expect, what to bring, etc.",
        xModel_ "fields.description.value",
        xBindClass_ "showErrors && !fields.description.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]
      (Lucid.toHtml event.emDescription)

startsAtField :: Events.Model -> Lucid.Html ()
startsAtField event =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Start Date & Time *"
    Lucid.input_
      [ Lucid.type_ "datetime-local",
        Lucid.name_ "starts_at",
        Lucid.required_ "true",
        Lucid.value_ (formatDateTimeLocal event.emStartsAt),
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        xModel_ "fields.startsAt.value",
        xBindClass_ "showErrors && !fields.startsAt.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

endsAtField :: Events.Model -> Lucid.Html ()
endsAtField event =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "End Date & Time *"
    Lucid.input_
      [ Lucid.type_ "datetime-local",
        Lucid.name_ "ends_at",
        Lucid.required_ "true",
        Lucid.value_ (formatDateTimeLocal event.emEndsAt),
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        xModel_ "fields.endsAt.value",
        xBindClass_ "showErrors && !fields.endsAt.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

locationNameField :: Events.Model -> Lucid.Html ()
locationNameField event =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Venue Name *"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "location_name",
        Lucid.required_ "true",
        Lucid.value_ event.emLocationName,
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        Lucid.placeholder_ "e.g. Shadow Hills Community Center",
        xModel_ "fields.locationName.value",
        xBindClass_ "showErrors && !fields.locationName.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

locationAddressField :: Events.Model -> Lucid.Html ()
locationAddressField event =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Address *"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "location_address",
        Lucid.required_ "true",
        Lucid.value_ event.emLocationAddress,
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        Lucid.placeholder_ "e.g. 1247 Underground Ave, Shadow Hills, CA",
        xModel_ "fields.locationAddress.value",
        xBindClass_ "showErrors && !fields.locationAddress.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'"
      ]

statusField :: Events.Model -> Lucid.Html ()
statusField event =
  Lucid.div_ [Lucid.class_ "space-y-4"] $ do
    Lucid.div_ $ do
      Lucid.label_ [Lucid.class_ "flex items-center gap-2"] $ do
        Lucid.input_ [Lucid.type_ "radio", Lucid.name_ "status", Lucid.value_ "draft", selectedIf (event.emStatus == Events.Draft)]
        Lucid.span_ [Lucid.class_ "font-bold"] "Save as Draft"
      Lucid.p_ [Lucid.class_ "text-sm text-gray-600 ml-6"] "Keep private until you're ready to publish"

    Lucid.div_ $ do
      Lucid.label_ [Lucid.class_ "flex items-center gap-2"] $ do
        Lucid.input_ [Lucid.type_ "radio", Lucid.name_ "status", Lucid.value_ "published", selectedIf (event.emStatus == Events.Published)]
        Lucid.span_ [Lucid.class_ "font-bold"] "Publish Immediately"
      Lucid.p_ [Lucid.class_ "text-sm text-gray-600 ml-6"] "Make visible to the public right away"

submitButton :: Lucid.Html ()
submitButton =
  Lucid.button_
    [ Lucid.type_ "submit",
      Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors",
      xOnClick_ "validateAndSubmit($event)"
    ]
    "UPDATE EVENT"

cancelButton :: Links.URI -> Lucid.Html ()
cancelButton eventBackUrl =
  Lucid.a_
    [ Lucid.href_ [i|/#{eventBackUrl}|],
      hxGet_ [i|/#{eventBackUrl}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline"
    ]
    "CANCEL"
