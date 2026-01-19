{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StationIds.New.Get.Templates.Form
  ( stationIdUploadForm,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStationIdsLinks)
import API.Types
import Component.Form.Builder
import Data.String.Interpolate (i)
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

stationIdNewPostUrl :: Links.URI
stationIdNewPostUrl = Links.linkURI dashboardStationIdsLinks.newPost

stationIdListUrl :: Links.URI
stationIdListUrl = Links.linkURI $ dashboardStationIdsLinks.list Nothing

--------------------------------------------------------------------------------

-- | Station ID upload form using FormBuilder
stationIdUploadForm :: Lucid.Html ()
stationIdUploadForm = do
  renderForm config form
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
        stagedAudioField "audio_file" "/api/uploads/audio" "station_id_audio" $ do
          label "Station ID Audio"
          hint "Upload an MP3, WAV, or other audio file. Maximum 50MB."
          maxSize 50

      -- Form Actions
      cancelButton cancelUrl "CANCEL"
      submitButton "UPLOAD"
