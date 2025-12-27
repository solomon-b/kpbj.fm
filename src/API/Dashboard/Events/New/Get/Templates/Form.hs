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
import Design (base, class_)
import Design.Tokens qualified as Tokens
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

-- | New event form template using V2 FormBuilder
template :: UserMetadata.Model -> Lucid.Html ()
template userMeta = do
  renderFormHeader userMeta
  renderForm config form
  where
    postUrl = [i|/#{dashboardEventsNewPostUrl}|]
    cancelUrl = [i|/#{dashboardEventsGetUrl}|]

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = postUrl,
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
          required
          minLength 3
          maxLength 200

        textField "tags" $ do
          label "Event Tags"
          placeholder "live-music, concert, fundraiser, community"
          hint "Comma separated tags"
          maxLength 500

        textareaField "description" 8 $ do
          label "Event Description"
          placeholder "Describe your event. Include any special details, what attendees should expect, what to bring, etc."
          required
          minLength 10
          maxLength 5000

        imageField "poster_image" $ do
          label "Event Poster Image"
          maxSize 10
          aspectRatio (2, 3)

      -- Date & Time Section
      section "DATE & TIME" $ do
        dateTimeField "starts_at" $ do
          label "Start Date & Time"
          required

        dateTimeField "ends_at" $ do
          label "End Date & Time"
          hint "Must be after start time"
          required
          customValidation "new Date(this.fields.ends_at.value) > new Date(this.fields.starts_at.value)" "End time must be after start time"

      -- Location Section
      section "LOCATION" $ do
        textField "location_name" $ do
          label "Venue Name"
          placeholder "e.g. Shadow Hills Community Center"
          required
          minLength 3
          maxLength 200

        textField "location_address" $ do
          label "Address"
          placeholder "e.g. 1247 Underground Ave, Shadow Hills, CA"
          required
          minLength 3
          maxLength 500

      footerToggle "status" $ do
        offLabel "Draft"
        onLabel "Published"
        offValue "draft"
        onValue "published"
        hint "Toggle to publish immediately"

      cancelButton cancelUrl "CANCEL"
      submitButton "CREATE EVENT"

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: UserMetadata.Model -> Lucid.Html ()
renderFormHeader userMeta =
  Lucid.section_ [class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "NEW EVENT"
        Lucid.div_ [class_ $ base ["text-gray-300", Tokens.textSm]] $ do
          Lucid.strong_ "Organizer: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [Lucid.class_ "text-center"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardEventsGetUrl}|],
            hxGet_ [i|/#{dashboardEventsGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "VIEW EVENTS"
