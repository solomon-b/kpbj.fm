{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.Slug.Episode.New.Get.Templates.Form
  ( episodeUploadForm,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardShowsLinks)
import API.Types
import Component.Form.V2
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

episodesNewPostUrl :: Slug -> Links.URI
episodesNewPostUrl showSlug = Links.linkURI $ dashboardShowsLinks.episodeNewPost showSlug

dashboardShowDetailUrl :: Shows.Id -> Slug -> Links.URI
dashboardShowDetailUrl showId showSlug = Links.linkURI $ dashboardShowsLinks.detail showId showSlug Nothing

--------------------------------------------------------------------------------

-- | Episode upload form using V2 FormBuilder
episodeUploadForm :: Shows.Model -> [ShowSchedule.UpcomingShowDate] -> Lucid.Html ()
episodeUploadForm showModel upcomingDates = do
  renderForm config form
  renderTrackManagementScript
  where
    postUrl = [i|/#{episodesNewPostUrl (Shows.slug showModel)}|]
    cancelUrl = [i|/#{dashboardShowDetailUrl (Shows.id showModel) (Shows.slug showModel)}|]

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

      -- Audio Upload Section
      section "AUDIO FILES" $ do
        audioField "audio_file" $ do
          label "Episode Audio"
          maxSize 500

        imageField "artwork_file" $ do
          label "Episode Image"
          maxSize 5
          aspectRatio (1, 1)

      -- Tracklist Section (custom JavaScript management)
      section "TRACKLIST" $ plain tracklistSection

      -- Publishing Section
      section "PUBLISHING" $ do
        plain $
          Lucid.p_ [Lucid.class_ "text-sm text-gray-600 italic mb-4"] $
            "Note: Published episodes will not be publicly visible until the scheduled date/time. "
              <> "Once the scheduled time has passed, the episode can no longer be edited."

        toggleField "status" $ do
          offLabel "Draft"
          onLabel "Published"
          offValue "draft"
          onValue "published"

      submitButton "SUBMIT"
      cancelButton cancelUrl "CANCEL"

    -- \| Encode schedule slot value as "template_id|scheduled_at" for form submission
    encodeScheduleValue :: ShowSchedule.UpcomingShowDate -> Text.Text
    encodeScheduleValue usd =
      display (ShowSchedule.usdTemplateId usd) <> "|" <> Text.pack (show $ ShowSchedule.usdStartTime usd)

--------------------------------------------------------------------------------
-- Custom HTML Sections

-- | Tracklist section with custom JavaScript management
tracklistSection :: Lucid.Html ()
tracklistSection =
  Lucid.div_ [] $ do
    Lucid.p_
      [Lucid.class_ "text-sm text-gray-600 mb-4"]
      "Add tracks in the order they will be played during the episode."

    Lucid.div_ [Lucid.id_ "tracklist-container"] $ do
      Lucid.div_ [Lucid.class_ "border-2 border-dashed border-gray-400 p-8 text-center text-gray-600"] $ do
        Lucid.button_
          [ Lucid.type_ "button",
            Lucid.id_ "add-track-btn",
            Lucid.class_ "bg-green-600 text-white px-6 py-3 font-bold hover:bg-green-700"
          ]
          "+ ADD TRACK"
        Lucid.div_ [Lucid.class_ "mt-2 text-sm"] "Click to add your first track"

    Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "tracks_json", Lucid.id_ "tracks-json", Lucid.value_ "[]"]

--------------------------------------------------------------------------------
-- Track Management JavaScript

renderTrackManagementScript :: Lucid.Html ()
renderTrackManagementScript =
  Lucid.script_
    [i|
// Track management module (IIFE to avoid global pollution)
(function() {
  // Track HTML template
  const createTrackElement = () => {
    const div = document.createElement('div');
    div.className = 'border border-gray-300 p-4 bg-gray-50 mb-4';
    div.innerHTML = `
      <div class='grid grid-cols-1 md:grid-cols-2 gap-4'>
        <div>
          <label class='block font-bold text-sm mb-1'>Track Title</label>
          <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-title' placeholder='Track title'>
        </div>
        <div>
          <label class='block font-bold text-sm mb-1'>Artist</label>
          <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-artist' placeholder='Artist name'>
        </div>
      </div>
      <div class='flex justify-end mt-4'>
        <button type='button' class='bg-red-600 text-white px-3 py-1 text-xs font-bold hover:bg-red-700' data-action='remove-track'>
          REMOVE
        </button>
      </div>
    `;
    return div;
  };

  // Extract track data from DOM element
  const extractTrackData = (div) => ({
    tiTitle: div.querySelector('.track-title')?.value || '',
    tiArtist: div.querySelector('.track-artist')?.value || ''
  });

  // Update hidden JSON field with current tracks
  const updateTracksJson = () => {
    const trackDivs = document.querySelectorAll('\#tracklist-container .border:not(.border-dashed)');
    const tracks = Array.from(trackDivs).map(extractTrackData);

    const jsonField = document.getElementById('tracks-json');
    if (jsonField) {
      jsonField.value = JSON.stringify(tracks);
    }
  };

  // Add new track
  const addTrack = () => {
    const container = document.getElementById('tracklist-container');
    const addButton = container?.querySelector('.border-dashed');
    if (container && addButton) {
      container.insertBefore(createTrackElement(), addButton);
      updateTracksJson();
    }
  };

  // Remove track
  const removeTrack = (button) => {
    button.closest('.border')?.remove();
    updateTracksJson();
  };

  // Extract and set audio duration
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

  // Initialize immediately (script runs after DOM elements are rendered)
  // Add track button
  document.getElementById('add-track-btn')?.addEventListener('click', addTrack);

  // Track list updates (use event delegation)
  const container = document.getElementById('tracklist-container');
  if (container) {
    container.addEventListener('input', updateTracksJson);
    container.addEventListener('change', updateTracksJson);
    container.addEventListener('click', (e) => {
      if (e.target.dataset.action === 'remove-track') {
        removeTrack(e.target);
      }
    });
  }

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
