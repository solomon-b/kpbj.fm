{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StationIds.New.Get.Templates.Form
  ( stationIdUploadForm,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStationIdsLinks)
import API.Types
import Component.AudioDurationScript (renderAudioDurationScript)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Lucid qualified
import Lucid.Form.Builder
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

stationIdNewPostUrl :: Links.URI
stationIdNewPostUrl = Links.linkURI dashboardStationIdsLinks.newPost

stationIdListUrl :: Links.URI
stationIdListUrl = Links.linkURI $ dashboardStationIdsLinks.list Nothing

--------------------------------------------------------------------------------

-- | Station ID upload form using FormBuilder
stationIdUploadForm :: Text -> Lucid.Html ()
stationIdUploadForm uploadUrl = do
  renderForm config form
  -- Fills the hidden duration_seconds field once a file is chosen. The break
  -- window subtracts the station ID's length from its budget.
  renderAudioDurationScript "audio_file-input"
  where
    postUrl = [i|/#{stationIdNewPostUrl}|]
    cancelUrl = [i|/#{stationIdListUrl}|]

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
      hidden "duration_seconds" ""

      -- Station ID Details Section
      section "STATION ID DETAILS" $ do
        textField "title" $ do
          label "Title"
          placeholder "Enter a name for this station ID..."
          hint "A short, descriptive name for this station ID (e.g., 'KPBJ Station ID - Male Voice')"
          required
          maxLength 200

      -- Audio File Section
      section "AUDIO FILE" $ do
        stagedAudioField "audio_file" uploadUrl "station_id_audio" $ do
          label "Station ID Audio"
          hint "Upload an MP3, WAV, or other audio file. Maximum 50MB."
          maxSize 50

      -- Form Actions
      cancelButton cancelUrl "CANCEL"
      submitButton "UPLOAD"
