{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Events.Slug.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardEventsLinks)
import API.Types (DashboardEventsRoutes (..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Form.Builder
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
dashboardEventsGetUrl :: Links.URI
dashboardEventsGetUrl = Links.linkURI $ dashboardEventsLinks.list Nothing

eventDetailUrl :: Events.Id -> Slug -> Links.URI
eventDetailUrl eventId slug = Links.linkURI $ dashboardEventsLinks.detail eventId slug

eventEditPostUrl :: Events.Id -> Slug -> Links.URI
eventEditPostUrl eventId slug = Links.linkURI $ dashboardEventsLinks.editPost eventId slug

--------------------------------------------------------------------------------

-- | Format UTCTime to HTML5 datetime-local format (YYYY-MM-DDTHH:MM)
formatDateTimeLocal :: UTCTime -> Text
formatDateTimeLocal = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M"

--------------------------------------------------------------------------------

-- | Event edit template using V2 FormBuilder
template :: StorageBackend -> Events.Model -> UserMetadata.Model -> Lucid.Html ()
template backend event userMeta = do
  renderFormHeader event userMeta eventBackUrl
  renderForm config form
  where
    eventId = event.emId
    eventSlug = event.emSlug
    eventBackUrl = eventDetailUrl eventId eventSlug
    eventEditUrl = eventEditPostUrl eventId eventSlug
    imageUrl = maybe "" (buildMediaUrl backend) event.emPosterImageUrl
    startsAtValue = formatDateTimeLocal event.emStartsAt
    endsAtValue = formatDateTimeLocal event.emEndsAt

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = [i|/#{eventEditUrl}|],
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content",
          fcHtmxSwap = Just "innerHTML"
        }

    form :: FormBuilder
    form = do
      -- Event Details Section
      section "EVENT DETAILS" $ do
        textField "title" $ do
          label "Event Title"
          placeholder "e.g. KPBJ Spring Concert"
          value event.emTitle
          required
          minLength 3
          maxLength 200

        textareaField "description" 8 $ do
          label "Event Description"
          placeholder "Describe your event. Include any special details, what attendees should expect, what to bring, etc."
          value event.emDescription
          required
          minLength 10
          maxLength 5000

        imageField "poster_image" $ do
          label "Event Poster Image"
          maxSize 10
          aspectRatio (3, 4)
          currentFile imageUrl

      -- Date & Time Section
      section "DATE & TIME" $ do
        dateTimeField "starts_at" $ do
          label "Start Date & Time"
          value startsAtValue
          required

        dateTimeField "ends_at" $ do
          label "End Date & Time"
          hint "Must be after start time"
          value endsAtValue
          required
          customValidation "new Date(this.fields.ends_at.value) > new Date(this.fields.starts_at.value)" "End time must be after start time"

      -- Location Section
      section "LOCATION" $ do
        textField "location_name" $ do
          label "Venue Name"
          placeholder "e.g. Shadow Hills Community Center"
          value event.emLocationName
          required
          minLength 3
          maxLength 200

        textField "location_address" $ do
          label "Address"
          placeholder "e.g. 1247 Underground Ave, Shadow Hills, CA"
          value event.emLocationAddress
          required
          minLength 3
          maxLength 500

      footerToggle "status" $ do
        offLabel "Draft"
        onLabel "Published"
        offValue "draft"
        onValue "published"
        when (event.emStatus == Events.Published) checked
        hint "Toggle to publish immediately"

      cancelButton [i|/#{eventBackUrl}|] "CANCEL"
      submitButton "UPDATE EVENT"

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: Events.Model -> UserMetadata.Model -> Links.URI -> Lucid.Html ()
renderFormHeader event userMeta eventBackUrl = do
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.textGray800, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "EDIT EVENT"
        Lucid.div_ [class_ $ base ["text-gray-300 dark:text-gray-500", Tokens.textSm]] $ do
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
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "<- BACK TO EVENT"
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardEventsGetUrl}|],
            hxGet_ [i|/#{dashboardEventsGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "VIEW EVENTS"
