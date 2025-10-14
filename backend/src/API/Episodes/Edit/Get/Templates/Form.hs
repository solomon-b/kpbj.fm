{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Episodes.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodesGetLink, hostDashboardGetLink)
import API.Episodes.Edit.Get.Templates.Scripts (scripts)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xBindClass_, xData_, xModel_, xOnBlur_, xOnInput_, xOnSubmit_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI hostDashboardGetLink

episodesIdGetUrl :: Text.Text -> Text.Text -> Links.URI
episodesIdGetUrl showSlug episodeSlug = Links.linkURI $ episodesGetLink showSlug episodeSlug

--------------------------------------------------------------------------------

alpineState :: Text.Text -> Text.Text -> Text
alpineState title description =
  [i|{
  fields: {
    title: { value: `#{title}`, isValid: true },
    description: { value: `#{description}`, isValid: true }
  },
  showErrors: false,

  validateTitle() {
    const trimmed = this.fields.title.value.trim();
    this.fields.title.isValid = trimmed.length >= 3 && trimmed.length <= 200;
  },

  validateDescription() {
    const trimmed = this.fields.description.value.trim();
    this.fields.description.isValid = trimmed.length <= 5000;
  },

  validateAndSubmit(event) {
    this.showErrors = true;

    this.validateTitle();
    this.validateDescription();

    const allFieldsValid = Object.values(this.fields).every(field => field.isValid);

    if (!allFieldsValid) {
      event.preventDefault();
      return false;
    }

    return true;
  }
}|]

--------------------------------------------------------------------------------

-- | Episode edit template
template :: Shows.Model -> Episodes.Model -> [EpisodeTrack.Model] -> UserMetadata.Model -> Bool -> Lucid.Html ()
template showModel episode tracks userMeta isStaff = do
  let showSlug = showModel.slug
      episodeSlug = episode.slug
      episodeBackUrl = episodesIdGetUrl showSlug episodeSlug
      titleValue = episode.title
      descriptionValue = fromMaybe "" episode.description

  formHeader showModel episode userMeta episodeBackUrl

  Lucid.div_ [xData_ (alpineState titleValue descriptionValue)] $ do
    Lucid.form_
      [ Lucid.action_ [i|/shows/#{showSlug}/episodes/#{episodeSlug}/edit|],
        Lucid.method_ "post",
        Lucid.class_ "space-y-8 w-full",
        xOnSubmit_ "validateAndSubmit($event)"
      ]
      $ do
        basicInformationSection showSlug episodeSlug episode
        statusSection isStaff episode
        trackListingSection tracks
        formActions episodeBackUrl

  scripts

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

basicInformationSection :: Text.Text -> Text.Text -> Episodes.Model -> Lucid.Html ()
basicInformationSection _showSlug _episodeSlug _episode = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 border-b border-gray-800 pb-2"] "BASIC INFORMATION"
    Lucid.div_ [Lucid.class_ "space-y-6"] $ do
      titleField
      descriptionField

statusSection :: Bool -> Episodes.Model -> Lucid.Html ()
statusSection isStaff episode = do
  if isStaff
    then staffStatusSection episode
    else nonStaffStatusSection episode

trackListingSection :: [EpisodeTrack.Model] -> Lucid.Html ()
trackListingSection tracks = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 border-b border-gray-800 pb-2"] "TRACK LISTING"
    Lucid.div_ [Lucid.id_ "tracks-container", Lucid.class_ "space-y-4"] $ do
      if null tracks
        then Lucid.p_ [Lucid.class_ "text-gray-600 italic"] "No tracks added yet."
        else forM_ (zip [(0 :: Int) ..] tracks) $ \(idx, track) -> renderTrackEditor idx track
    addTrackButton

formActions :: Links.URI -> Lucid.Html ()
formActions episodeBackUrl = do
  Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.button_
        [ Lucid.type_ "submit",
          Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors"
        ]
        "UPDATE EPISODE"
      Lucid.a_
        [ Lucid.href_ [i|/#{episodeBackUrl}|],
          hxGet_ [i|/#{episodeBackUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline inline-block"
        ]
        "CANCEL"

--------------------------------------------------------------------------------

titleField :: Lucid.Html ()
titleField = do
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2", Lucid.for_ "title"] "Episode Title *"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "title",
        Lucid.id_ "title",
        Lucid.required_ "true",
        xModel_ "fields.title.value",
        xBindClass_ "showErrors && !fields.title.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'",
        xOnInput_ "showErrors && validateTitle()",
        xOnBlur_ "showErrors && validateTitle()",
        Lucid.placeholder_ "e.g. Deep Dive into Techno"
      ]
    Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "3-200 characters (URL slug will be auto-generated from title)"

descriptionField :: Lucid.Html ()
descriptionField = do
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2", Lucid.for_ "description"] "Description"
    Lucid.textarea_
      [ Lucid.name_ "description",
        Lucid.id_ "description",
        Lucid.rows_ "6",
        xModel_ "fields.description.value",
        xBindClass_ "showErrors && !fields.description.isValid ? 'w-full p-3 border-2 border-red-500 font-mono leading-relaxed focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono leading-relaxed focus:border-blue-600'",
        xOnInput_ "showErrors && validateDescription()",
        xOnBlur_ "showErrors && validateDescription()",
        Lucid.placeholder_ "Describe this episode. What tracks did you play? Any special guests or themes?"
      ]
      mempty
    Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "Up to 5000 characters"

staffStatusSection :: Episodes.Model -> Lucid.Html ()
staffStatusSection episode = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 border-b border-gray-800 pb-2"] "STATUS & SCHEDULE"
    Lucid.div_ [Lucid.class_ "space-y-6"] $ do
      statusField episode

nonStaffStatusSection :: Episodes.Model -> Lucid.Html ()
nonStaffStatusSection episode = do
  Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "status", Lucid.value_ (display episode.status)]

statusField :: Episodes.Model -> Lucid.Html ()
statusField episode = do
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Status *"
    Lucid.select_
      [ Lucid.name_ "status",
        Lucid.required_ "true",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono"
      ]
      $ do
        Lucid.option_ [Lucid.value_ "draft", selectedIf (episode.status == Episodes.Draft)] "Draft"
        Lucid.option_ [Lucid.value_ "scheduled", selectedIf (episode.status == Episodes.Scheduled)] "Scheduled"
        Lucid.option_ [Lucid.value_ "published", selectedIf (episode.status == Episodes.Published)] "Published"
        Lucid.option_ [Lucid.value_ "archived", selectedIf (episode.status == Episodes.Archived)] "Archived"
    Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "Only published episodes appear on the show page"

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
--------------------------------------------------------------------------------

renderTrackEditor :: Int -> EpisodeTrack.Model -> Lucid.Html ()
renderTrackEditor idx track = do
  Lucid.div_ [Lucid.class_ "border-2 border-gray-300 p-4 track-item"] $ do
    trackEditorHeader idx
    Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ [i|tracks[#{idx}][id]|], Lucid.value_ (display track.id)]
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
trackNumberField idx track = do
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-1 text-sm"] "Track #"
    Lucid.input_
      [ Lucid.type_ "number",
        Lucid.name_ [i|tracks[#{idx}][track_number]|],
        Lucid.value_ (display track.trackNumber),
        Lucid.min_ "1",
        Lucid.class_ "w-full p-2 border border-gray-400 font-mono text-sm"
      ]

trackDurationField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackDurationField idx track = do
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-1 text-sm"] "Duration"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ [i|tracks[#{idx}][duration]|],
        Lucid.value_ (fromMaybe "" track.duration),
        Lucid.placeholder_ "e.g. 5:42",
        Lucid.pattern_ "[0-9]+:[0-5][0-9]",
        Lucid.title_ "Format: MM:SS (e.g. 5:42)",
        Lucid.class_ "w-full p-2 border border-gray-400 font-mono text-sm"
      ]

trackTitleField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackTitleField idx track = do
  Lucid.div_ [Lucid.class_ "col-span-2"] $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-1 text-sm"] "Title *"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ [i|tracks[#{idx}][title]|],
        Lucid.value_ track.title,
        Lucid.required_ "true",
        Lucid.minlength_ "1",
        Lucid.maxlength_ "200",
        Lucid.class_ "w-full p-2 border border-gray-400 font-mono text-sm"
      ]

trackArtistField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackArtistField idx track = do
  Lucid.div_ [Lucid.class_ "col-span-2"] $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-1 text-sm"] "Artist *"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ [i|tracks[#{idx}][artist]|],
        Lucid.value_ track.artist,
        Lucid.required_ "true",
        Lucid.minlength_ "1",
        Lucid.maxlength_ "200",
        Lucid.class_ "w-full p-2 border border-gray-400 font-mono text-sm"
      ]

trackAlbumField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackAlbumField idx track = do
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-1 text-sm"] "Album"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ [i|tracks[#{idx}][album]|],
        Lucid.value_ (fromMaybe "" track.album),
        Lucid.maxlength_ "200",
        Lucid.class_ "w-full p-2 border border-gray-400 font-mono text-sm"
      ]

trackLabelField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackLabelField idx track = do
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-1 text-sm"] "Label"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ [i|tracks[#{idx}][label]|],
        Lucid.value_ (fromMaybe "" track.label),
        Lucid.maxlength_ "200",
        Lucid.class_ "w-full p-2 border border-gray-400 font-mono text-sm"
      ]

trackYearField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackYearField idx track = do
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-1 text-sm"] "Year"
    Lucid.input_
      [ Lucid.type_ "number",
        Lucid.name_ [i|tracks[#{idx}][year]|],
        Lucid.value_ (maybe "" display track.year),
        Lucid.min_ "1900",
        Lucid.max_ "2099",
        Lucid.class_ "w-full p-2 border border-gray-400 font-mono text-sm"
      ]

trackPremiereField :: Int -> EpisodeTrack.Model -> Lucid.Html ()
trackPremiereField idx track = do
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "flex items-center space-x-2"] $ do
      Lucid.input_
        [ Lucid.type_ "checkbox",
          Lucid.name_ [i|tracks[#{idx}][is_exclusive_premiere]|],
          Lucid.value_ "true",
          if track.isExclusivePremiere then Lucid.checked_ else mempty,
          Lucid.class_ "w-4 h-4"
        ]
      Lucid.span_ [Lucid.class_ "text-sm font-bold"] "Exclusive Premiere"

--------------------------------------------------------------------------------

-- Helper to set selected attribute
selectedIf :: Bool -> Lucid.Attributes
selectedIf True = Lucid.selected_ "selected"
selectedIf False = mempty
