{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Episode.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodesGetLink, hostDashboardGetLink)
import API.Shows.Slug.Episode.Edit.Get.Templates.Scripts (scripts)
import Component.Form qualified as Form
import Component.Form.Builder qualified as Builder
import Component.Form.Internal (hiddenInput)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI $ hostDashboardGetLink Nothing

episodesIdGetUrl :: Shows.Id -> Episodes.Id -> Slug -> Links.URI
episodesIdGetUrl showId episodeId episodeSlug = Links.linkURI $ episodesGetLink showId episodeId episodeSlug

--------------------------------------------------------------------------------

-- | Check if the episode's scheduled date is in the future (allowing file uploads)
isScheduledInFuture :: UTCTime -> Episodes.Model -> Bool
isScheduledInFuture now episode = case episode.scheduledAt of
  Nothing -> True -- No scheduled date means it's still editable
  Just scheduledAt -> scheduledAt > now

-- | Episode edit template
--
-- The currentTime parameter is used to determine if the scheduled date has passed.
-- If the scheduled date is in the future, file upload fields are shown.
template :: UTCTime -> Shows.Model -> Episodes.Model -> [EpisodeTrack.Model] -> UserMetadata.Model -> Bool -> Lucid.Html ()
template currentTime showModel episode tracks userMeta _isStaff = do
  let showIdText = display showModel.id
      episodeSlug = episode.slug
      episodeIdText = display episode.id
      episodeBackUrl = episodesIdGetUrl showModel.id episode.id episodeSlug
      titleValue = episode.title
      descriptionValue = fromMaybe "" episode.description
      allowFileUpload = isScheduledInFuture currentTime episode

  Builder.buildValidatedForm
    Builder.FormBuilder
      { Builder.fbAction = [i|/shows/#{showIdText}/episodes/#{episodeIdText}/#{display episodeSlug}/edit|],
        Builder.fbMethod = "post",
        Builder.fbHeader = Just (formHeader showModel episode userMeta episodeBackUrl),
        Builder.fbHtmx = Nothing,
        Builder.fbFields =
          [ -- Hidden status field (controlled by toggle)
            Builder.HiddenField
              { Builder.hfName = "status",
                Builder.hfValue = display episode.status
              },
            -- Basic Information Section with validated fields
            Builder.SectionField
              { Builder.sfTitle = "BASIC INFORMATION",
                Builder.sfFields =
                  [ Builder.ValidatedTextField
                      { Builder.vfName = "title",
                        Builder.vfLabel = "Episode Title",
                        Builder.vfInitialValue = Just titleValue,
                        Builder.vfPlaceholder = Just "e.g. Deep Dive into Techno",
                        Builder.vfHint = Just "3-200 characters (URL slug will be auto-generated from title)",
                        Builder.vfValidation =
                          Builder.ValidationRules
                            { Builder.vrMinLength = Just 3,
                              Builder.vrMaxLength = Just 200,
                              Builder.vrPattern = Nothing,
                              Builder.vrRequired = True,
                              vrCustomValidation = Nothing
                            }
                      },
                    Builder.ValidatedTextareaField
                      { Builder.vtName = "description",
                        Builder.vtLabel = "Description",
                        Builder.vtInitialValue = Just descriptionValue,
                        Builder.vtRows = 6,
                        Builder.vtPlaceholder = Just "Describe this episode. What tracks did you play? Any special guests or themes?",
                        Builder.vtHint = Just "Up to 5000 characters",
                        Builder.vtValidation =
                          Builder.ValidationRules
                            { Builder.vrMinLength = Nothing,
                              Builder.vrMaxLength = Just 5000,
                              Builder.vrPattern = Nothing,
                              Builder.vrRequired = False,
                              vrCustomValidation = Nothing
                            }
                      }
                  ]
              },
            -- File Upload Section (only shown if scheduled date is in the future)
            Builder.ConditionalField
              { Builder.cfCondition = allowFileUpload,
                Builder.cfTrueFields =
                  [ Builder.SectionField
                      { Builder.sfTitle = "MEDIA FILES",
                        Builder.sfFields =
                          [ Builder.ValidatedFileField
                              { Builder.vffName = "audio_file",
                                Builder.vffLabel = "Audio File",
                                Builder.vffAccept = Just "audio/*",
                                Builder.vffHint = Just "MP3, WAV, FLAC accepted • Max 500MB • Leave empty to keep current file",
                                Builder.vffMaxSizeMB = Just 500,
                                Builder.vffValidation = Builder.emptyValidation, -- Optional for edits
                                Builder.vffButtonText = "📁 CHOOSE AUDIO FILE",
                                Builder.vffButtonClasses = "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700 inline-block",
                                Builder.vffCurrentValue = episode.audioFilePath
                              },
                            Builder.ValidatedFileField
                              { Builder.vffName = "artwork_file",
                                Builder.vffLabel = "Episode Artwork",
                                Builder.vffAccept = Just "image/jpeg,image/png",
                                Builder.vffHint = Just "JPG, PNG accepted • Max 5MB • Recommended: 800x800px • Leave empty to keep current",
                                Builder.vffMaxSizeMB = Just 5,
                                Builder.vffValidation = Builder.emptyValidation, -- Optional
                                Builder.vffButtonText = "🖼️ CHOOSE IMAGE",
                                Builder.vffButtonClasses = "bg-purple-600 text-white px-6 py-3 font-bold hover:bg-purple-700 inline-block",
                                Builder.vffCurrentValue = episode.artworkUrl
                              }
                          ]
                      }
                  ],
                Builder.cfFalseFields = [] -- No file upload fields when scheduled date has passed
              },
            -- Track Listing Section
            Builder.PlainField
              { Builder.pfHtml = trackListingSection tracks
              }
          ],
        Builder.fbAdditionalContent = [formActions episode],
        Builder.fbStyles = Builder.defaultFormStyles
      }

  scripts

--------------------------------------------------------------------------------
-- HELPER FUNCTIONS
--------------------------------------------------------------------------------

formHeader :: Shows.Model -> Episodes.Model -> UserMetadata.Model -> Links.URI -> Lucid.Html ()
formHeader showModel episode userMeta episodeBackUrl = do
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "EDIT EPISODE"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          Lucid.strong_ "Show: "
          Lucid.toHtml showModel.title
          " • "
          Lucid.strong_ "Episode #"
          Lucid.toHtml (display episode.episodeNumber)
          " • "
          Lucid.strong_ "Editor: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [Lucid.class_ "space-x-4"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{episodeBackUrl}|],
            hxGet_ [i|/#{episodeBackUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "← BACK TO EPISODE"
        Lucid.a_
          [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
            hxGet_ [i|/#{hostDashboardGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "DASHBOARD"

--------------------------------------------------------------------------------
-- FORM SECTIONS
--------------------------------------------------------------------------------

trackListingSection :: [EpisodeTrack.Model] -> Lucid.Html ()
trackListingSection tracks = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 border-b border-gray-800 pb-2"] "TRACK LISTING"
    Lucid.div_ [Lucid.id_ "tracks-container", Lucid.class_ "space-y-4"] $ do
      if null tracks
        then Lucid.p_ [Lucid.class_ "text-gray-600 italic"] "No tracks added yet."
        else forM_ (zip [(0 :: Int) ..] tracks) $ uncurry renderTrackEditor
    addTrackButton

formActions :: Episodes.Model -> Lucid.Html ()
formActions episode = do
  let isPublished = episode.status == Episodes.Published
  Lucid.section_ [Lucid.class_ "bg-gray-100 border-2 border-gray-400 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex flex-col gap-4"] $ do
      -- Publishing note
      Lucid.p_ [Lucid.class_ "text-sm text-gray-600 italic"] $
        "Note: Published episodes will not be publicly visible until the scheduled date/time. "
          <> "Once the scheduled time has passed, the episode can no longer be edited."
      Lucid.div_ [Lucid.class_ "flex justify-end items-center"] $ do
        Lucid.div_ [Lucid.class_ "flex gap-4 items-center"] $ do
          -- Status toggle switch
          Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
            Lucid.span_ [Lucid.class_ "text-sm font-bold text-gray-600"] "Draft"
            Lucid.label_ [Lucid.class_ "relative inline-flex items-center cursor-pointer"] $ do
              Lucid.input_ $
                [ Lucid.type_ "checkbox",
                  Lucid.id_ "status-toggle",
                  Lucid.class_ "sr-only peer"
                ]
                  <> [Lucid.checked_ | isPublished]
              Lucid.div_
                [ Lucid.class_ $
                    "w-11 h-6 bg-gray-300 peer-focus:outline-none peer-focus:ring-2 "
                      <> "peer-focus:ring-blue-300 rounded-full peer "
                      <> "peer-checked:after:translate-x-full peer-checked:after:border-white "
                      <> "after:content-[''] after:absolute after:top-[2px] after:left-[2px] "
                      <> "after:bg-white after:border-gray-300 after:border after:rounded-full "
                      <> "after:h-5 after:w-5 after:transition-all peer-checked:bg-green-600"
                ]
                mempty
            Lucid.span_ [Lucid.class_ "text-sm font-bold text-gray-600"] "Published"
          Lucid.button_
            [ Lucid.type_ "submit",
              Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
            ]
            "SUBMIT"
  -- JavaScript to sync toggle with hidden field
  Lucid.script_
    [i|
(function() {
  const statusToggle = document.getElementById('status-toggle');
  const statusField = document.querySelector('input[name="status"]');
  statusToggle?.addEventListener('change', () => {
    if (statusField) {
      statusField.value = statusToggle.checked ? 'published' : 'draft';
    }
  });
})();
|]

--------------------------------------------------------------------------------
-- TRACK EDITOR COMPONENTS
--------------------------------------------------------------------------------

addTrackButton :: Lucid.Html ()
addTrackButton = do
  Lucid.div_ [Lucid.class_ "mt-4"] $ do
    Lucid.button_
      [ Lucid.type_ "button",
        Lucid.id_ "add-track-btn",
        Lucid.class_ "bg-blue-600 text-white px-6 py-2 font-bold hover:bg-blue-700"
      ]
      "+ ADD TRACK"

--------------------------------------------------------------------------------
-- TRACK EDITOR COMPONENT

renderTrackEditor :: Int -> EpisodeTrack.Model -> Lucid.Html ()
renderTrackEditor idx track = do
  Lucid.div_ [Lucid.class_ "border-2 border-gray-300 p-4 track-item"] $ do
    trackEditorHeader idx
    hiddenInput [i|tracks[#{idx}][id]|] (display track.id)
    Lucid.div_ [Lucid.class_ "grid grid-cols-2 gap-4"] $ do
      trackNumberField idx track
      trackDurationField idx track
      trackTitleField idx track
      trackArtistField idx track
      trackAlbumField idx track
      trackLabelField idx track
      trackYearField idx track
      trackPremiereField idx track

trackEditorHeader :: Int -> Lucid.Html ()
trackEditorHeader idx = do
  Lucid.div_ [Lucid.class_ "flex justify-between items-center mb-3"] $ do
    Lucid.h3_ [Lucid.class_ "font-bold"] $ "Track #" <> Lucid.toHtml (show (idx + 1))
    Lucid.button_
      [ Lucid.type_ "button",
        Lucid.class_ "text-red-600 font-bold hover:text-red-800 remove-track-btn"
      ]
      "✕ REMOVE"

trackNumberField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackNumberField idx track =
  Form.formNumberInput
    Form.FormNumberInputConfig
      { Form.fniName = [i|tracks[#{idx}][track_number]|],
        Form.fniLabel = "Track #",
        Form.fniValue = Just (fromIntegral track.trackNumber),
        Form.fniMin = Just 1,
        Form.fniMax = Nothing,
        Form.fniHint = Nothing,
        Form.fniRequired = False
      }

trackDurationField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackDurationField idx track =
  Form.formTextInput
    Form.FormTextInputConfig
      { Form.ftiName = [i|tracks[#{idx}][duration]|],
        Form.ftiLabel = "Duration",
        Form.ftiValue = track.duration,
        Form.ftiPlaceholder = Just "e.g. 5:42",
        Form.ftiHint = Nothing,
        Form.ftiRequired = False
      }

trackTitleField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackTitleField idx track = do
  Lucid.div_ [Lucid.class_ "col-span-2"] $
    Form.formTextInput
      Form.FormTextInputConfig
        { Form.ftiName = [i|tracks[#{idx}][title]|],
          Form.ftiLabel = "Title",
          Form.ftiValue = Just track.title,
          Form.ftiPlaceholder = Nothing,
          Form.ftiHint = Nothing,
          Form.ftiRequired = True
        }

trackArtistField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackArtistField idx track = do
  Lucid.div_ [Lucid.class_ "col-span-2"] $
    Form.formTextInput
      Form.FormTextInputConfig
        { Form.ftiName = [i|tracks[#{idx}][artist]|],
          Form.ftiLabel = "Artist",
          Form.ftiValue = Just track.artist,
          Form.ftiPlaceholder = Nothing,
          Form.ftiHint = Nothing,
          Form.ftiRequired = True
        }

trackAlbumField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackAlbumField idx track =
  Form.formTextInput
    Form.FormTextInputConfig
      { Form.ftiName = [i|tracks[#{idx}][album]|],
        Form.ftiLabel = "Album",
        Form.ftiValue = track.album,
        Form.ftiPlaceholder = Nothing,
        Form.ftiHint = Nothing,
        Form.ftiRequired = False
      }

trackLabelField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackLabelField idx track =
  Form.formTextInput
    Form.FormTextInputConfig
      { Form.ftiName = [i|tracks[#{idx}][label]|],
        Form.ftiLabel = "Label",
        Form.ftiValue = track.label,
        Form.ftiPlaceholder = Nothing,
        Form.ftiHint = Nothing,
        Form.ftiRequired = False
      }

trackYearField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackYearField idx track =
  Form.formNumberInput
    Form.FormNumberInputConfig
      { Form.fniName = [i|tracks[#{idx}][year]|],
        Form.fniLabel = "Year",
        Form.fniValue = fmap fromIntegral track.year,
        Form.fniMin = Just 1900,
        Form.fniMax = Just 2099,
        Form.fniHint = Nothing,
        Form.fniRequired = False
      }

trackPremiereField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackPremiereField idx track =
  Form.formCheckbox
    Form.FormCheckboxConfig
      { Form.fcName = [i|tracks[#{idx}][is_exclusive_premiere]|],
        Form.fcValue = "true",
        Form.fcLabel = "Exclusive Premiere",
        Form.fcChecked = track.isExclusivePremiere
      }
