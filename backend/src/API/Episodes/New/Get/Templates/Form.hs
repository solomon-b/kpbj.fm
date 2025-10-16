{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Episodes.New.Get.Templates.Form
  ( episodeUploadForm,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodesNewPostLink)
import API.Episodes.New.Get.Templates.Scripts (renderEpisodeUploadScripts)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (Day, DayOfWeek (..), UTCTime)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Base (makeAttributes)
import Lucid.Extras
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

episodesNewPostUrl :: Text -> Links.URI
episodesNewPostUrl showSlug = Links.linkURI $ episodesNewPostLink showSlug

--------------------------------------------------------------------------------

episodeUploadForm :: Shows.Model -> [ShowSchedule.UpcomingShowDate] -> Lucid.Html ()
episodeUploadForm showModel upcomingDates = do
  formHeader
  episodeForm showModel upcomingDates
  renderEpisodeUploadScripts

formHeader :: Lucid.Html ()
formHeader = do
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "UPLOAD EPISODE"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] "Create a new episode for one of your shows"
      Lucid.div_ [Lucid.class_ "text-center"] $ do
        Lucid.div_ [Lucid.class_ "w-16 h-16 bg-gray-300 mx-auto mb-2 flex items-center justify-center border-2 border-gray-600"] $ do
          Lucid.span_ [Lucid.class_ "text-2xl"] "🎵"

errorSummary :: Lucid.Html ()
errorSummary = do
  Lucid.div_
    [ xShow_ "showErrorSummary",
      Lucid.class_ "bg-red-100 border-2 border-red-500 p-4 mb-4"
    ]
    $ do
      Lucid.div_ [Lucid.class_ "flex items-start"] $ do
        Lucid.div_ [Lucid.class_ "flex-shrink-0"] $ do
          Lucid.span_ [Lucid.class_ "text-2xl"] "⚠️"
        Lucid.div_ [Lucid.class_ "ml-3"] $ do
          Lucid.h3_ [Lucid.class_ "text-sm font-bold text-red-800"] "Please fix the following errors:"
          Lucid.p_ [Lucid.class_ "mt-2 text-sm text-red-700", xText_ "getFirstError()"] ""

showInfoSection :: Shows.Model -> Lucid.Html ()
showInfoSection showModel = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "SHOW"
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Show"
    Lucid.div_ [Lucid.class_ "w-full p-3 border-2 border-gray-400 bg-gray-100 font-mono"] $
      Lucid.toHtml (Shows.title showModel)
    Lucid.input_
      [ Lucid.type_ "hidden",
        Lucid.name_ "show_id",
        Lucid.value_ [i|#{Shows.id showModel}|]
      ]

episodeDetailsSection :: [ShowSchedule.UpcomingShowDate] -> Lucid.Html ()
episodeDetailsSection upcomingDates = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mt-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "EPISODE DETAILS"
    Lucid.div_ [Lucid.class_ "space-y-6"] $ do
      episodeTypeField
      titleField
      descriptionField
      scheduledDateField upcomingDates
      tagsField

audioUploadSection :: Lucid.Html ()
audioUploadSection = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "AUDIO FILES"
    Lucid.div_ [Lucid.class_ "space-y-6"] $ do
      audioFileField
      artworkFileField

formActions :: Lucid.Html ()
formActions = do
  Lucid.section_ [Lucid.class_ "bg-gray-100 border-2 border-gray-400 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex justify-end items-center"] $ do
      Lucid.div_ [Lucid.class_ "flex gap-4"] $ do
        Lucid.a_
          [ Lucid.href_ "/host/dashboard",
            Lucid.class_ "bg-gray-600 text-white px-6 py-3 font-bold hover:bg-gray-700"
          ]
          "CANCEL"
        Lucid.input_
          [ Lucid.type_ "hidden",
            Lucid.name_ "action",
            Lucid.value_ "publish"
          ]
        Lucid.button_
          [ Lucid.type_ "submit",
            Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700",
            xBindDisabled_ "isSubmitting",
            xBindClass_ "isSubmitting ? 'opacity-50 cursor-not-allowed' : ''"
          ]
          $ do
            Lucid.span_ [xShow_ "!isSubmitting"] "PUBLISH EPISODE"
            Lucid.span_ [xShow_ "isSubmitting"] "PUBLISHING..."

episodeForm :: Shows.Model -> [ShowSchedule.UpcomingShowDate] -> Lucid.Html ()
episodeForm showModel upcomingDates = do
  Lucid.form_
    [ Lucid.method_ "post",
      Lucid.action_ [i|/#{episodesNewPostUrl (Shows.slug showModel)}|],
      Lucid.enctype_ "multipart/form-data",
      Lucid.class_ "space-y-8 w-full",
      xData_ "episodeUploadValidator()",
      xOnSubmit_ "validateAndSubmit($event)",
      makeAttributes "novalidate" ""
    ]
    $ do
      errorSummary
      Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "duration_seconds", Lucid.id_ "duration-seconds", Lucid.value_ ""]
      showInfoSection showModel
      episodeDetailsSection upcomingDates
      tracklistSection
      audioUploadSection
      formActions

--------------------------------------------------------------------------------

episodeTypeField :: Lucid.Html ()
episodeTypeField = do
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Type *"
    Lucid.select_
      [ Lucid.name_ "episode_type",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono bg-white",
        Lucid.required_ "required"
      ]
      $ do
        Lucid.option_ [Lucid.value_ "pre-recorded", Lucid.selected_ "selected"] "Pre-recorded"
        Lucid.option_ [Lucid.value_ "live"] "Live Show"
        Lucid.option_ [Lucid.value_ "hybrid"] "Hybrid (Live + Pre-recorded segments)"

titleField :: Lucid.Html ()
titleField = do
  Lucid.div_ [makeAttributes "data-error" "!fields.title.isValid && showErrors"] $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Title *"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "title",
        xModel_ "fields.title.value",
        xBindClass_ "showErrors && !fields.title.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'",
        xOnInput_ "showErrors && validateTitle()",
        xOnBlur_ "showErrors && validateTitle()",
        Lucid.placeholder_ "e.g. Industrial Depths #088",
        Lucid.required_ "required"
      ]
    Lucid.div_ [xShow_ "!fields.title.isValid && showErrors", Lucid.class_ "mt-1 text-sm text-red-600", xText_ "fields.title.error"] ""

descriptionField :: Lucid.Html ()
descriptionField = do
  Lucid.div_ [makeAttributes "data-error" "!fields.description.isValid && showErrors"] $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Description *"
    Lucid.textarea_
      [ Lucid.name_ "description",
        Lucid.rows_ "4",
        xModel_ "fields.description.value",
        xBindClass_ "showErrors && !fields.description.isValid ? 'w-full p-3 border-2 border-red-500 font-mono focus:border-red-600' : 'w-full p-3 border-2 border-gray-400 font-mono focus:border-blue-600'",
        xOnInput_ "showErrors && validateDescription()",
        xOnBlur_ "showErrors && validateDescription()",
        Lucid.placeholder_ "Describe what listeners can expect from this episode...",
        Lucid.required_ "required"
      ]
      mempty
    Lucid.div_ [xShow_ "!fields.description.isValid && showErrors", Lucid.class_ "mt-1 text-sm text-red-600", xText_ "fields.description.error"] ""

scheduledDateField :: [ShowSchedule.UpcomingShowDate] -> Lucid.Html ()
scheduledDateField upcomingDates = do
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Scheduled Date"
    if null upcomingDates
      then do
        Lucid.div_ [Lucid.class_ "w-full p-3 border-2 border-yellow-400 bg-yellow-50 font-mono text-sm"] $ do
          "No upcoming scheduled dates"
      else do
        Lucid.select_
          [ Lucid.name_ "scheduled_date",
            Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono bg-white"
          ]
          $ do
            Lucid.option_ [Lucid.value_ ""] "-- Select Date --"
            mapM_ renderUpcomingDateOption upcomingDates

tagsField :: Lucid.Html ()
tagsField = do
  Lucid.div_ $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Tags"
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.name_ "tags",
        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
        Lucid.placeholder_ "industrial, ambient, glitch, experimental (comma separated)"
      ]

tracklistSection :: Lucid.Html ()
tracklistSection = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "TRACKLIST"
    Lucid.p_ [Lucid.class_ "text-sm text-gray-600 mb-4"] "Add tracks in the order they will be played during the episode."

    Lucid.div_ [Lucid.id_ "tracklist-container", makeAttributes "data-error" "!tracksValid && showErrors"] $ do
      Lucid.div_ [Lucid.class_ "border-2 border-dashed border-gray-400 p-8 text-center text-gray-600"] $ do
        Lucid.button_
          [ Lucid.type_ "button",
            Lucid.id_ "add-track-btn",
            Lucid.class_ "bg-green-600 text-white px-6 py-3 font-bold hover:bg-green-700"
          ]
          "+ ADD TRACK"
        Lucid.div_ [Lucid.class_ "mt-2 text-sm"] "Click to add your first track"

    Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "tracks_json", Lucid.id_ "tracks-json", Lucid.value_ "[]"]
    Lucid.div_ [xShow_ "!tracksValid && showErrors", Lucid.class_ "mt-2 text-sm text-red-600", xText_ "tracksError"] ""

audioFileField :: Lucid.Html ()
audioFileField = do
  Lucid.div_ [makeAttributes "data-error" "!fields.audio_file.isValid && showErrors"] $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Main Episode File *"
    Lucid.div_
      [ xBindClass_ "showErrors && !fields.audio_file.isValid ? 'border-2 border-dashed border-red-500 p-6 text-center' : 'border-2 border-dashed border-gray-400 p-6 text-center'"
      ]
      $ do
        Lucid.input_
          [ Lucid.type_ "file",
            Lucid.name_ "audio_file",
            Lucid.accept_ "audio/*",
            Lucid.class_ "hidden",
            Lucid.id_ "main-file",
            xOnChange_ "handleAudioFileChange()",
            Lucid.required_ "required"
          ]
        Lucid.label_ [Lucid.for_ "main-file", Lucid.class_ "cursor-pointer"] $ do
          Lucid.div_ [Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700 inline-block"] $ do
            "📁 CHOOSE AUDIO FILE"
          Lucid.div_ [Lucid.class_ "mt-2 text-sm text-gray-600"] "MP3, WAV, FLAC accepted • Max 500MB"
          Lucid.div_ [xShow_ "fields.audio_file.fileName", Lucid.class_ "mt-2 text-sm font-bold text-gray-800"] $ do
            Lucid.span_ [xText_ "fields.audio_file.fileName"] ""
            Lucid.span_ [Lucid.class_ "text-gray-600 ml-2"] $ do
              "("
              Lucid.span_ [xText_ "formatFileSize(fields.audio_file.fileSize)"] ""
              ")"
    Lucid.div_ [xShow_ "!fields.audio_file.isValid && showErrors", Lucid.class_ "mt-2 text-sm text-red-600", xText_ "fields.audio_file.error"] ""

artworkFileField :: Lucid.Html ()
artworkFileField = do
  Lucid.div_ [makeAttributes "data-error" "!fields.artwork_file.isValid && showErrors"] $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Image (Optional)"
    Lucid.div_
      [ xBindClass_ "showErrors && !fields.artwork_file.isValid ? 'border-2 border-dashed border-red-500 p-6 text-center' : 'border-2 border-dashed border-gray-400 p-6 text-center'"
      ]
      $ do
        Lucid.input_
          [ Lucid.type_ "file",
            Lucid.name_ "artwork_file",
            Lucid.accept_ "image/*",
            Lucid.class_ "hidden",
            Lucid.id_ "episode-image",
            xOnChange_ "handleArtworkFileChange()"
          ]
        Lucid.label_ [Lucid.for_ "episode-image", Lucid.class_ "cursor-pointer"] $ do
          Lucid.div_ [Lucid.class_ "bg-purple-600 text-white px-6 py-3 font-bold hover:bg-purple-700 inline-block"] $ do
            "🖼️ CHOOSE IMAGE"
          Lucid.div_ [Lucid.class_ "mt-2 text-sm text-gray-600"] "JPG, PNG accepted • Max 5MB • Recommended: 800x800px"
          Lucid.div_ [xShow_ "fields.artwork_file.fileName", Lucid.class_ "mt-2 text-sm font-bold text-gray-800"] $ do
            Lucid.span_ [xText_ "fields.artwork_file.fileName"] ""
            Lucid.span_ [Lucid.class_ "text-gray-600 ml-2"] $ do
              "("
              Lucid.span_ [xText_ "formatFileSize(fields.artwork_file.fileSize)"] ""
              ")"
    Lucid.div_ [xShow_ "!fields.artwork_file.isValid && showErrors", Lucid.class_ "mt-2 text-sm text-red-600", xText_ "fields.artwork_file.error"] ""

-- | Render an upcoming date option for the dropdown
renderUpcomingDateOption :: ShowSchedule.UpcomingShowDate -> Lucid.Html ()
renderUpcomingDateOption (ShowSchedule.UpcomingShowDate {usdShowDate = showDate, usdDayOfWeek = dow, usdStartTime = startTime, usdEndTime = endTime}) = do
  Lucid.option_
    [Lucid.value_ (Text.pack $ Prelude.show showDate)]
    $ Lucid.toHtml
    $ formatUpcomingDate dow showDate startTime endTime
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
