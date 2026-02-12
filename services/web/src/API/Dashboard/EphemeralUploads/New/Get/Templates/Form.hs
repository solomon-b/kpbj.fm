{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.EphemeralUploads.New.Get.Templates.Form
  ( ephemeralUploadForm,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardEphemeralUploadsLinks)
import API.Types
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Form.Builder
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

ephemeralUploadNewPostUrl :: Links.URI
ephemeralUploadNewPostUrl = Links.linkURI dashboardEphemeralUploadsLinks.newPost

ephemeralUploadListUrl :: Links.URI
ephemeralUploadListUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.list Nothing

--------------------------------------------------------------------------------

-- | Ephemeral upload form using FormBuilder
ephemeralUploadForm :: Text -> Lucid.Html ()
ephemeralUploadForm uploadUrl = do
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
        plain ephemeralContentNotice

        textField "title" $ do
          label "Title"
          placeholder "Enter a name for this ephemeral clip..."
          hint "A short, descriptive name for this ephemeral upload (e.g., 'Night Ambient Loop')"
          required
          maxLength 200

        textareaField "description" 3 $ do
          label "Description"
          placeholder "Describe this ephemeral clip..."
          hint "At least 80 characters (approximately 2 sentences)."
          required
          minLength 80

      -- Audio File Section
      section "AUDIO FILE" $ do
        stagedAudioField "audio_file" uploadUrl "ephemeral_audio" $ do
          label "Ephemeral Audio"
          hint "Upload an MP3. Maximum 500MB."
          maxSize 500

      -- Form Actions
      cancelButton cancelUrl "CANCEL"
      submitButton "UPLOAD"

-- | Editorial guidelines notice for ephemeral content.
ephemeralContentNotice :: Lucid.Html ()
ephemeralContentNotice =
  Lucid.p_
    [class_ $ base [Tokens.textSm, Tokens.fgMuted, Tokens.mb4]]
    "Ephemeral clips are inferred as coming from the voice of the station. Use your best judgment when submitting. We want to see interesting, creative, historic, or otherwise thought-provoking content."
