{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.EphemeralUploads.New.Get.Templates.Form
  ( ephemeralUploadForm,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardEphemeralUploadsLinks)
import API.Types
import Component.Form.Builder
import Data.String.Interpolate (i)
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

ephemeralUploadNewPostUrl :: Links.URI
ephemeralUploadNewPostUrl = Links.linkURI dashboardEphemeralUploadsLinks.newPost

ephemeralUploadListUrl :: Links.URI
ephemeralUploadListUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.list Nothing

--------------------------------------------------------------------------------

-- | Ephemeral upload form using FormBuilder
ephemeralUploadForm :: Lucid.Html ()
ephemeralUploadForm = do
  renderForm config form
  where
    postUrl = [i|/#{ephemeralUploadNewPostUrl}|]
    cancelUrl = [i|/#{ephemeralUploadListUrl}|]

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
      -- Ephemeral Upload Details Section
      section "EPHEMERAL UPLOAD DETAILS" $ do
        textField "title" $ do
          label "Title"
          placeholder "Enter a name for this ephemeral clip..."
          hint "A short, descriptive name for this ephemeral upload (e.g., 'Night Ambient Loop')"
          required
          maxLength 200

      -- Audio File Section
      section "AUDIO FILE" $ do
        stagedAudioField "audio_file" "/api/uploads/audio" "ephemeral_audio" $ do
          label "Ephemeral Audio"
          hint "Upload an MP3, WAV, or other audio file. Maximum 50MB."
          maxSize 50

      -- Form Actions
      cancelButton cancelUrl "CANCEL"
      submitButton "UPLOAD"
