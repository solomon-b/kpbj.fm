{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Episode.New.Get.Templates.Form
  ( episodeUploadForm,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodesNewPostLink)
import Component.Form.Builder
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (Day, DayOfWeek (..), UTCTime)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

episodesNewPostUrl :: Slug -> Links.URI
episodesNewPostUrl showSlug = Links.linkURI $ episodesNewPostLink showSlug

--------------------------------------------------------------------------------

-- | Episode upload form using Component.Form.Builder
episodeUploadForm :: Shows.Model -> [ShowSchedule.UpcomingShowDate] -> Lucid.Html ()
episodeUploadForm showModel upcomingDates = do
  buildValidatedForm
    FormBuilder
      { fbAction = [i|/#{episodesNewPostUrl (Shows.slug showModel)}|],
        fbMethod = "post",
        fbHeader = Just (renderFormHeader showModel),
        fbFields = episodeFormFields showModel upcomingDates,
        fbAdditionalContent = [renderSubmitActions, renderTrackManagementScript],
        fbStyles = defaultFormStyles
      }

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: Shows.Model -> Lucid.Html ()
renderFormHeader showModel =
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "UPLOAD EPISODE"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          "Create a new episode for: "
          Lucid.strong_ $ Lucid.toHtml (Shows.title showModel)
      Lucid.div_ [Lucid.class_ "text-center"] $ do
        Lucid.div_ [Lucid.class_ "w-16 h-16 bg-gray-300 mx-auto mb-2 flex items-center justify-center border-2 border-gray-600"] $ do
          Lucid.span_ [Lucid.class_ "text-2xl"] "🎵"

--------------------------------------------------------------------------------
-- Form Fields Definition (The Polynomial Structure!)

episodeFormFields :: Shows.Model -> [ShowSchedule.UpcomingShowDate] -> [FormField]
episodeFormFields showModel upcomingDates =
  [ -- Hidden fields
    HiddenField
      { hfName = "show_id",
        hfValue = [i|#{Shows.id showModel}|]
      },
    HiddenField
      { hfName = "duration_seconds",
        hfValue = ""
      },
    HiddenField
      { hfName = "action",
        hfValue = "publish"
      },
    -- Show Info Section (read-only display)
    PlainField
      { pfHtml = showInfoSection showModel
      },
    -- Episode Details Section
    SectionField
      { sfTitle = "EPISODE DETAILS",
        sfFields =
          [ ValidatedSelectField
              { vsName = "episode_type",
                vsLabel = "Episode Type",
                vsOptions =
                  [ SelectOption "pre-recorded" "Pre-recorded" True Nothing,
                    SelectOption "live" "Live Show" False Nothing,
                    SelectOption "hybrid" "Hybrid (Live + Pre-recorded segments)" False Nothing
                  ],
                vsHint = Nothing,
                vsValidation = emptyValidation {vrRequired = True}
              },
            ValidatedTextField
              { vfName = "title",
                vfLabel = "Episode Title",
                vfInitialValue = Nothing,
                vfPlaceholder = Just "e.g., Industrial Depths #088",
                vfHint = Nothing,
                vfValidation =
                  ValidationRules
                    { vrMinLength = Just 3,
                      vrMaxLength = Just 200,
                      vrPattern = Nothing,
                      vrRequired = True,
                      vrCustomValidation = Nothing
                    }
              },
            ValidatedTextareaField
              { vtName = "description",
                vtLabel = "Episode Description",
                vtInitialValue = Nothing,
                vtRows = 4,
                vtPlaceholder = Just "Describe what listeners can expect from this episode...",
                vtHint = Nothing,
                vtValidation =
                  ValidationRules
                    { vrMinLength = Just 10,
                      vrMaxLength = Just 5000,
                      vrPattern = Nothing,
                      vrRequired = True,
                      vrCustomValidation = Nothing
                    }
              },
            -- Scheduled date field (conditional on upcomingDates)
            ConditionalField
              { cfCondition = not (null upcomingDates),
                cfTrueFields =
                  [ ValidatedSelectField
                      { vsName = "scheduled_date",
                        vsLabel = "Scheduled Date",
                        vsOptions = SelectOption "" "-- Select Date --" False Nothing : map renderUpcomingDateOption upcomingDates,
                        vsHint = Just "Choose when this episode will air",
                        vsValidation = emptyValidation
                      }
                  ],
                cfFalseFields =
                  [ PlainField
                      { pfHtml =
                          Lucid.div_ $ do
                            Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Scheduled Date"
                            Lucid.div_
                              [Lucid.class_ "w-full p-3 border-2 border-yellow-400 bg-yellow-50 font-mono text-sm"]
                              "No upcoming scheduled dates"
                      }
                  ]
              },
            ValidatedTextField
              { vfName = "tags",
                vfLabel = "Tags",
                vfInitialValue = Nothing,
                vfPlaceholder = Just "industrial, ambient, glitch, experimental (comma separated)",
                vfHint = Just "Optional. Comma-separated list of genres/themes",
                vfValidation = emptyValidation {vrMaxLength = Just 500}
              }
          ]
      },
    -- Tracklist Section (custom JavaScript management via PlainField)
    PlainField
      { pfHtml = tracklistSection
      },
    -- Audio Upload Section
    SectionField
      { sfTitle = "AUDIO FILES",
        sfFields =
          [ ValidatedFileField
              { vffName = "audio_file",
                vffLabel = "Main Episode File",
                vffAccept = Just "audio/*",
                vffHint = Just "MP3, WAV, FLAC accepted • Max 500MB",
                vffMaxSizeMB = Just 500,
                vffValidation = emptyValidation {vrRequired = True},
                vffButtonText = "📁 CHOOSE AUDIO FILE",
                vffButtonClasses = "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700 inline-block",
                vffCurrentValue = Nothing -- New episode, no current file
              },
            ValidatedFileField
              { vffName = "artwork_file",
                vffLabel = "Episode Image",
                vffAccept = Just "image/jpeg,image/png",
                vffHint = Just "JPG, PNG accepted • Max 5MB • Recommended: 800x800px",
                vffMaxSizeMB = Just 5,
                vffValidation = emptyValidation, -- Optional
                vffButtonText = "🖼️ CHOOSE IMAGE",
                vffButtonClasses = "bg-purple-600 text-white px-6 py-3 font-bold hover:bg-purple-700 inline-block",
                vffCurrentValue = Nothing -- New episode, no current file
              }
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Helper: Convert UpcomingShowDate to SelectOption

renderUpcomingDateOption :: ShowSchedule.UpcomingShowDate -> SelectOption
renderUpcomingDateOption (ShowSchedule.UpcomingShowDate {usdShowDate = showDate, usdDayOfWeek = dow, usdStartTime = startTime, usdEndTime = endTime}) =
  SelectOption
    { soValue = Text.pack $ Prelude.show showDate,
      soLabel = formatUpcomingDate dow showDate startTime endTime,
      soSelected = False,
      soDescription = Nothing
    }
  where
    formatUpcomingDate :: DayOfWeek -> Day -> UTCTime -> UTCTime -> Text
    formatUpcomingDate d sd st et =
      dayOfWeekName d
        <> ", "
        <> Text.pack (Prelude.show sd)
        <> " ("
        <> display st
        <> " - "
        <> display et
        <> ")"

    dayOfWeekName :: DayOfWeek -> Text
    dayOfWeekName Sunday = "Sunday"
    dayOfWeekName Monday = "Monday"
    dayOfWeekName Tuesday = "Tuesday"
    dayOfWeekName Wednesday = "Wednesday"
    dayOfWeekName Thursday = "Thursday"
    dayOfWeekName Friday = "Friday"
    dayOfWeekName Saturday = "Saturday"

--------------------------------------------------------------------------------
-- Custom HTML Sections (PlainField content)

-- | Show info section (read-only display)
showInfoSection :: Shows.Model -> Lucid.Html ()
showInfoSection showModel =
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "SHOW"
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Show"
    Lucid.div_ [Lucid.class_ "w-full p-3 border-2 border-gray-400 bg-gray-100 font-mono"] $
      Lucid.toHtml (Shows.title showModel)

-- | Tracklist section with custom JavaScript management
-- NOTE: This remains as custom HTML because the Form Builder doesn't support
-- dynamic lists with add/remove functionality yet. This is a good example
-- of using PlainField as an escape hatch for complex interactions.
tracklistSection :: Lucid.Html ()
tracklistSection =
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "TRACKLIST"
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
-- Form Submit Actions (rendered inside <form>)

renderSubmitActions :: Lucid.Html ()
renderSubmitActions =
  Lucid.section_ [Lucid.class_ "bg-gray-100 border-2 border-gray-400 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex justify-end items-center"] $ do
      Lucid.div_ [Lucid.class_ "flex gap-4"] $ do
        Lucid.a_
          [ Lucid.href_ "/host/dashboard",
            Lucid.class_ "bg-gray-600 text-white px-6 py-3 font-bold hover:bg-gray-700"
          ]
          "CANCEL"
        Lucid.button_
          [ Lucid.type_ "submit",
            Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
          ]
          "PUBLISH EPISODE"

--------------------------------------------------------------------------------
-- Track Management JavaScript (injected via fbAdditionalContent)

-- | Render JavaScript for track management
-- This script is exactly like the original from Scripts.hs, but injected
-- as additional form content rather than a separate script tag.
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
      <div class='grid grid-cols-1 md:grid-cols-3 gap-4'>
        <div>
          <label class='block font-bold text-sm mb-1'>Track Title</label>
          <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-title' placeholder='Track title'>
        </div>
        <div>
          <label class='block font-bold text-sm mb-1'>Artist</label>
          <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-artist' placeholder='Artist name'>
        </div>
        <div>
          <label class='block font-bold text-sm mb-1'>Album/Year</label>
          <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-album' placeholder='Album (Year)'>
        </div>
      </div>
      <div class='grid grid-cols-2 md:grid-cols-4 gap-4 mt-4'>
        <div>
          <label class='block font-bold text-sm mb-1'>Duration</label>
          <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-duration' placeholder='4:23'>
        </div>
        <div>
          <label class='block font-bold text-sm mb-1'>Label</label>
          <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-label' placeholder='Record label'>
        </div>
        <div class='flex items-end'>
          <label class='flex items-center text-sm'>
            <input type='checkbox' class='mr-2 track-exclusive'> Exclusive Premiere
          </label>
        </div>
        <div class='flex items-end justify-end'>
          <button type='button' class='bg-red-600 text-white px-3 py-1 text-xs font-bold hover:bg-red-700' data-action='remove-track'>
            REMOVE
          </button>
        </div>
      </div>
    `;
    return div;
  };

  // Extract track data from DOM element
  const extractTrackData = (div) => ({
    tiTitle: div.querySelector('.track-title')?.value || '',
    tiArtist: div.querySelector('.track-artist')?.value || '',
    tiAlbum: div.querySelector('.track-album')?.value || null,
    tiYear: null,
    tiDuration: div.querySelector('.track-duration')?.value || null,
    tiLabel: div.querySelector('.track-label')?.value || null,
    tiIsExclusive: div.querySelector('.track-exclusive')?.checked || false
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
      const durationField = document.getElementById('duration-seconds');
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

  // Audio file duration extraction
  const audioInput = document.getElementById('audio_file-input');
  audioInput?.addEventListener('change', (e) => {
    const file = e.target.files[0];
    if (file) extractAudioDuration(file);
  });
})();
|]
