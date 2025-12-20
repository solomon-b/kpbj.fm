{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Episodes.Slug.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Edit.Get.Templates.Scripts (scripts)
import API.Links (dashboardLinks, showEpisodesLinks)
import API.Types
import Component.Form qualified as Form
import Component.Form.Builder qualified as Builder
import Component.Form.Internal (hiddenInput)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
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
hostDashboardGetUrl = Links.linkURI dashboardLinks.home

episodeDetailUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeDetailUrl showSlug epNum = Links.linkURI $ showEpisodesLinks.detail showSlug epNum

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
-- The isStaff parameter indicates if the user has staff-level permissions or higher.
template :: UTCTime -> Shows.Model -> Episodes.Model -> [EpisodeTrack.Model] -> UserMetadata.Model -> Bool -> Lucid.Html ()
template currentTime showModel episode tracks userMeta isStaff = do
  let showSlugText = display showModel.slug
      episodeNum = episode.episodeNumber
      episodeNumText = display episodeNum
      episodeBackUrl = episodeDetailUrl showModel.slug episodeNum
      descriptionValue = fromMaybe "" episode.description
      allowFileUpload = isScheduledInFuture currentTime episode
      -- Status can be changed if the scheduled date is in the future OR user is staff/admin
      canChangeStatus = allowFileUpload || isStaff

  Builder.buildValidatedForm
    Builder.FormBuilder
      { Builder.fbAction = [i|/dashboard/episodes/#{showSlugText}/#{episodeNumText}/edit|],
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
                  [ Builder.ValidatedTextareaField
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
                                Builder.vffButtonText = "CHOOSE AUDIO FILE",
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
                                Builder.vffButtonText = "CHOOSE IMAGE",
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
        Builder.fbAdditionalContent = [formActions episode canChangeStatus],
        Builder.fbStyles = Builder.defaultFormStyles
      }

  scripts

--------------------------------------------------------------------------------
-- HELPER FUNCTIONS
--------------------------------------------------------------------------------

formHeader :: Shows.Model -> Episodes.Model -> UserMetadata.Model -> Links.URI -> Lucid.Html ()
formHeader showModel episode userMeta episodeBackUrl = do
  Lucid.section_ [class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "EDIT EPISODE"
        Lucid.div_ [class_ $ base ["text-gray-300", Tokens.textSm]] $ do
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
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "<- BACK TO EPISODE"
        let dashboardUrl = hostDashboardGetUrl
         in Lucid.a_
              [ Lucid.href_ [i|/#{dashboardUrl}|],
                hxGet_ [i|/#{dashboardUrl}|],
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
              ]
              "DASHBOARD"

--------------------------------------------------------------------------------
-- FORM SECTIONS
--------------------------------------------------------------------------------

trackListingSection :: [EpisodeTrack.Model] -> Lucid.Html ()
trackListingSection tracks = do
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6]] $ do
    Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4, "border-b", "border-gray-800", "pb-2"]] "TRACK LISTING"
    Lucid.div_ [Lucid.id_ "tracks-container", class_ $ base ["space-y-4"]] $ do
      if null tracks
        then Lucid.p_ [class_ $ base [Tokens.textGray600, "italic"]] "No tracks added yet."
        else forM_ (zip [(0 :: Int) ..] tracks) $ uncurry renderTrackEditor
    addTrackButton

formActions :: Episodes.Model -> Bool -> Lucid.Html ()
formActions episode canChangeStatus = do
  let isPublished = episode.status == Episodes.Published
      -- Style classes for disabled state
      labelClass =
        if canChangeStatus
          then "relative inline-flex items-center cursor-pointer"
          else "relative inline-flex items-center cursor-not-allowed opacity-50"
      toggleClass =
        if canChangeStatus
          then
            "w-11 h-6 bg-gray-300 peer-focus:outline-none peer-focus:ring-2 "
              <> "peer-focus:ring-blue-300 rounded-full peer "
              <> "peer-checked:after:translate-x-full peer-checked:after:border-white "
              <> "after:content-[''] after:absolute after:top-[2px] after:left-[2px] "
              <> "after:bg-white after:border-gray-300 after:border after:rounded-full "
              <> "after:h-5 after:w-5 after:transition-all peer-checked:bg-green-600"
          else
            "w-11 h-6 bg-gray-300 rounded-full "
              <> "peer-checked:after:translate-x-full peer-checked:after:border-white "
              <> "after:content-[''] after:absolute after:top-[2px] after:left-[2px] "
              <> "after:bg-white after:border-gray-300 after:border after:rounded-full "
              <> "after:h-5 after:w-5 after:transition-all peer-checked:bg-gray-400"
  Lucid.section_ [class_ $ base [Tokens.bgGray100, "border-2", "border-gray-400", Tokens.p6]] $ do
    Lucid.div_ [class_ $ base ["flex", "flex-col", Tokens.gap4]] $ do
      -- Publishing note
      if canChangeStatus
        then
          Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.textGray600, "italic"]] $
            "Note: Published episodes will not be publicly visible until the scheduled date/time. "
              <> "Once the scheduled time has passed, the episode can no longer be edited."
        else
          Lucid.p_ [class_ $ base [Tokens.textSm, "text-yellow-700", "italic"]] $
            "Note: This episode's scheduled date has passed. Status changes are locked. "
              <> "Only staff or admin users can change the status of past episodes."
      Lucid.div_ [class_ $ base ["flex", "justify-end", "items-center"]] $ do
        Lucid.div_ [class_ $ base ["flex", Tokens.gap4, "items-center"]] $ do
          -- Status toggle switch
          Lucid.div_ [class_ $ base ["flex", "items-center", "gap-3"]] $ do
            Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.textGray600]] "Draft"
            Lucid.label_ [Lucid.class_ labelClass] $ do
              Lucid.input_ $
                [ Lucid.type_ "checkbox",
                  Lucid.id_ "status-toggle",
                  Lucid.class_ "sr-only peer"
                ]
                  <> [Lucid.checked_ | isPublished]
                  <> [Lucid.disabled_ "disabled" | not canChangeStatus]
              Lucid.div_ [Lucid.class_ toggleClass] mempty
            Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.textGray600]] "Published"
          Lucid.button_
            [ Lucid.type_ "submit",
              class_ $ base ["bg-blue-600", Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-blue-700"]
            ]
            "SUBMIT"
  -- JavaScript to sync toggle with hidden field (only if enabled)
  Lucid.script_
    [i|
(function() {
  const statusToggle = document.getElementById('status-toggle');
  const statusField = document.querySelector('input[name="status"]');
  if (statusToggle && !statusToggle.disabled) {
    statusToggle.addEventListener('change', () => {
      if (statusField) {
        statusField.value = statusToggle.checked ? 'published' : 'draft';
      }
    });
  }
})();
|]

--------------------------------------------------------------------------------
-- TRACK EDITOR COMPONENTS
--------------------------------------------------------------------------------

addTrackButton :: Lucid.Html ()
addTrackButton = do
  Lucid.div_ [class_ $ base ["mt-4"]] $ do
    Lucid.button_
      [ Lucid.type_ "button",
        Lucid.id_ "add-track-btn",
        class_ $ base ["bg-blue-600", Tokens.textWhite, Tokens.px6, "py-2", Tokens.fontBold, "hover:bg-blue-700"]
      ]
      "+ ADD TRACK"

--------------------------------------------------------------------------------
-- TRACK EDITOR COMPONENT

renderTrackEditor :: Int -> EpisodeTrack.Model -> Lucid.Html ()
renderTrackEditor idx track = do
  Lucid.div_ [class_ $ base ["border-2", "border-gray-300", Tokens.p4, "track-item"]] $ do
    trackEditorHeader idx
    hiddenInput [i|tracks[#{idx}][id]|] (display track.id)
    Lucid.div_ [class_ $ base ["grid", "grid-cols-2", Tokens.gap4]] $ do
      trackNumberField idx track
      trackTitleField idx track
      trackArtistField idx track

trackEditorHeader :: Int -> Lucid.Html ()
trackEditorHeader idx = do
  Lucid.div_ [class_ $ base ["flex", "justify-between", "items-center", "mb-3"]] $ do
    Lucid.h3_ [class_ $ base [Tokens.fontBold]] $ "Track #" <> Lucid.toHtml (show (idx + 1))
    Lucid.button_
      [ Lucid.type_ "button",
        class_ $ base ["text-red-600", Tokens.fontBold, "hover:text-red-800", "remove-track-btn"]
      ]
      "X REMOVE"

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

trackTitleField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackTitleField idx track = do
  Lucid.div_ [class_ $ base ["col-span-2"]] $
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
  Lucid.div_ [class_ $ base ["col-span-2"]] $
    Form.formTextInput
      Form.FormTextInputConfig
        { Form.ftiName = [i|tracks[#{idx}][artist]|],
          Form.ftiLabel = "Artist",
          Form.ftiValue = Just track.artist,
          Form.ftiPlaceholder = Nothing,
          Form.ftiHint = Nothing,
          Form.ftiRequired = True
        }
