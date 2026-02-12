{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.EphemeralUploads.Id.Edit.Get.Templates.Form
  ( ephemeralUploadEditForm,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardEphemeralUploadsLinks)
import API.Types (DashboardEphemeralUploadsRoutes (..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Lucid qualified
import Lucid.Form.Builder
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

ephemeralUploadListUrl :: Links.URI
ephemeralUploadListUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.list Nothing

--------------------------------------------------------------------------------

-- | Ephemeral upload edit form using FormBuilder
ephemeralUploadEditForm :: Text -> EphemeralUploads.Model -> Lucid.Html ()
ephemeralUploadEditForm uploadUrl ephemeralUpload = do
  renderForm config form
  where
    editPostUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.editPost ephemeralUpload.eumId
    postUrl = [i|/#{editPostUrl}|] :: Text
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
      section "EDIT EPHEMERAL UPLOAD" $ do
        plain ephemeralContentNotice

        textField "title" $ do
          label "Title"
          placeholder "Enter a name for this ephemeral clip..."
          hint "A short, descriptive name for this ephemeral upload (e.g., 'Night Ambient Loop')"
          required
          maxLength 200
          value ephemeralUpload.eumTitle

        textareaField "description" 3 $ do
          label "Description"
          placeholder "Describe this ephemeral clip..."
          hint "At least 80 characters (approximately 2 sentences)."
          required
          minLength 80
          value ephemeralUpload.eumDescription

      -- Audio File Section (optional re-upload)
      section "AUDIO FILE" $ do
        stagedAudioField "audio_file" uploadUrl "ephemeral_audio" $ do
          label "Replace Audio (Optional)"
          hint "Upload a new audio file to replace the existing one. Leave empty to keep the current file."
          maxSize 50

      -- Form Actions
      cancelButton cancelUrl "CANCEL"
      submitButton "SAVE CHANGES"

-- | Editorial guidelines notice for ephemeral content.
ephemeralContentNotice :: Lucid.Html ()
ephemeralContentNotice =
  Lucid.p_
    [class_ $ base [Tokens.textSm, Tokens.fgMuted, Tokens.mb4]]
    "Ephemeral clips are inferred as coming from the voice of the station. Use your best judgment when submitting. We want to see interesting, creative, historic, or otherwise thought-provoking content."
