{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.Slug.Episode.New.Get.Templates.Form
  ( episodeUploadForm,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardShowsLinks)
import API.Types
import Component.Form.Builder
import Component.TrackListingEditor qualified as TrackListingEditor
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

episodesNewPostUrl :: Slug -> Links.URI
episodesNewPostUrl showSlug = Links.linkURI $ dashboardShowsLinks.episodeNewPost showSlug

dashboardShowDetailUrl :: Shows.Id -> Slug -> Links.URI
dashboardShowDetailUrl showId showSlug = Links.linkURI $ dashboardShowsLinks.detail showId showSlug Nothing

--------------------------------------------------------------------------------

-- | Episode upload form using V2 FormBuilder
episodeUploadForm :: Shows.Model -> [ShowSchedule.UpcomingShowDate] -> UserMetadata.Model -> Lucid.Html ()
episodeUploadForm showModel upcomingDates userMeta = do
  renderForm config form
  renderAudioDurationScript
  where
    postUrl = [i|/#{episodesNewPostUrl (Shows.slug showModel)}|]
    cancelUrl = [i|/#{dashboardShowDetailUrl (Shows.id showModel) (Shows.slug showModel)}|]
    isStaffOrHigher = UserMetadata.isStaffOrHigher userMeta.mUserRole

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
      -- Hidden fields
      hidden "show_id" [i|#{Shows.id showModel}|]
      hidden "duration_seconds" ""

      -- Episode Details Section
      section "EPISODE DETAILS" $ do
        -- Scheduled date field (conditional on upcomingDates)
        if not (null upcomingDates)
          then selectField "scheduled_date" $ do
            label "Scheduled Date"
            hint "Choose when this episode will air"
            addOption "" "-- Select Date --"
            mapM_ (\usd -> addOption (encodeScheduleValue usd) (display usd)) upcomingDates
          else plain $
            Lucid.div_ $ do
              Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Scheduled Date"
              Lucid.div_
                [Lucid.class_ "w-full p-3 border-2 border-yellow-400 bg-yellow-50 font-mono text-sm"]
                "No upcoming scheduled dates"

        textareaField "description" 6 $ do
          label "Episode Description"
          placeholder "Describe what listeners can expect from this episode..."
          hint "Up to 5000 characters"
          required
          maxLength 5000

        textField "tags" $ do
          label "Tags"
          placeholder "industrial, ambient, glitch, experimental (comma separated)"
          hint "Optional. Comma-separated list of genres/themes"
          maxLength 500

      -- Media Files Section
      section "MEDIA FILES" $ do
        stagedAudioField "audio_file" "/api/uploads/audio" "episode_audio" $ do
          label "Episode Audio"
          maxSize 500

        imageField "artwork_file" $ do
          label "Episode Artwork"
          maxSize 5
          aspectRatio (1, 1)

      -- Tracklist Section
      section "TRACKLIST" $
        plain $
          TrackListingEditor.render
            TrackListingEditor.Config
              { TrackListingEditor.editorId = "new-episode-tracks",
                TrackListingEditor.initialTracks = [],
                TrackListingEditor.jsonFieldName = "tracks_json"
              }

      -- Publishing Section
      if isStaffOrHigher
        then do
          footerToggle "status" $ do
            offLabel "Draft"
            onLabel "Published"
            offValue "draft"
            onValue "published"
          footerHint "Published episodes will be visible after their scheduled date"
        else do
          hidden "status" "draft"
      cancelButton cancelUrl "CANCEL"
      submitButton "SUBMIT"

    -- \| Encode schedule slot value as "template_id|scheduled_at" for form submission
    encodeScheduleValue :: ShowSchedule.UpcomingShowDate -> Text.Text
    encodeScheduleValue usd =
      display (ShowSchedule.usdTemplateId usd) <> "|" <> Text.pack (show $ ShowSchedule.usdStartTime usd)

--------------------------------------------------------------------------------
-- Audio Duration Script

-- | Script to extract audio duration from uploaded file.
--
-- This is separate from the track listing editor because it handles
-- the audio file input from the FormBuilder.
renderAudioDurationScript :: Lucid.Html ()
renderAudioDurationScript =
  Lucid.script_
    [i|
(function() {
  const extractAudioDuration = (file) => {
    const isAudio = file.type.startsWith('audio/') || /\\.(mp3|wav|flac|aac|ogg|m4a)$/i.test(file.name);
    if (!isAudio) return;

    const audio = new Audio();
    audio.preload = 'metadata';

    audio.onloadedmetadata = () => {
      const durationField = document.querySelector('input[name="duration_seconds"]');
      if (durationField) {
        durationField.value = Math.round(audio.duration);
      }
      URL.revokeObjectURL(audio.src);
    };

    audio.onerror = () => URL.revokeObjectURL(audio.src);
    audio.src = URL.createObjectURL(file);
  };

  // Audio file duration extraction - listen for the V2 FormBuilder audio field
  const audioContainer = document.querySelector('[data-field-name="audio_file"]');
  if (audioContainer) {
    const audioInput = audioContainer.querySelector('input[type="file"]');
    audioInput?.addEventListener('change', (e) => {
      const file = e.target.files[0];
      if (file) extractAudioDuration(file);
    });
  }
})();
|]
