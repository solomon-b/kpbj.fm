{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Episodes.Slug.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Edit.Get.Templates.Scripts (scripts)
import API.Links (apiLinks, showEpisodesLinks)
import API.Types
import Component.Form qualified as Form
import Component.Form.Internal (hiddenInput)
import Component.Form.V2
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

episodeDetailUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeDetailUrl showSlug epNum = Links.linkURI $ showEpisodesLinks.detail showSlug epNum

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

--------------------------------------------------------------------------------

-- | Check if the episode's scheduled date is in the future (allowing file uploads)
isScheduledInFuture :: UTCTime -> Episodes.Model -> Bool
isScheduledInFuture now episode = episode.scheduledAt > now

-- | Episode edit template
--
-- The currentTime parameter is used to determine if the scheduled date has passed.
-- If the scheduled date is in the future, file upload fields are shown.
-- The isStaff parameter indicates if the user has staff-level permissions or higher.
template :: UTCTime -> Shows.Model -> Episodes.Model -> [EpisodeTrack.Model] -> [ShowSchedule.UpcomingShowDate] -> UserMetadata.Model -> Bool -> Lucid.Html ()
template currentTime showModel episode tracks upcomingDates _userMeta isStaff = do
  renderForm config form
  scripts
  where
    showSlugText = display showModel.slug
    episodeNum = episode.episodeNumber
    episodeNumText = display episodeNum
    episodeBackUrl = episodeDetailUrl showModel.slug episodeNum
    descriptionValue = fromMaybe "" episode.description
    -- File uploads allowed if scheduled date is in the future OR user is staff/admin
    allowFileUpload = isScheduledInFuture currentTime episode || isStaff
    -- Status can be changed if the scheduled date is in the future OR user is staff/admin
    canChangeStatus = allowFileUpload || isStaff
    audioUrl = maybe "" (\path -> [i|/#{mediaGetUrl}/#{path}|]) episode.audioFilePath
    artworkUrl = maybe "" (\path -> [i|/#{mediaGetUrl}/#{path}|]) episode.artworkUrl
    isPublished = episode.status == Episodes.Published

    -- \| Encode schedule slot value as "template_id|scheduled_at" for form submission
    encodeScheduleValue :: ShowSchedule.UpcomingShowDate -> Text.Text
    encodeScheduleValue usd =
      display (ShowSchedule.usdTemplateId usd) <> "|" <> Text.pack (show $ ShowSchedule.usdStartTime usd)

    -- \| Encode current episode's schedule value
    encodeCurrentScheduleValue :: Episodes.Model -> Text.Text
    encodeCurrentScheduleValue ep =
      display ep.scheduleTemplateId <> "|" <> Text.pack (show ep.scheduledAt)

    postUrl = [i|/dashboard/episodes/#{showSlugText}/#{episodeNumText}/edit|]

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
      section "BASIC INFORMATION" $ do
        textareaField "description" 6 $ do
          label "Description"
          placeholder "Describe this episode. What tracks did you play? Any special guests or themes?"
          hint "Up to 5000 characters"
          value descriptionValue
          maxLength 5000

      when allowFileUpload $ do
        section "MEDIA FILES" $ do
          audioField "episode_audio" $ do
            label "Episode Audio"
            maxSize 500
            currentFile audioUrl

          imageField "episode_artwork" $ do
            label "Episode Artwork"
            maxSize 5
            aspectRatio (1, 1)
            currentFile artworkUrl

      section "TRACK LISTING" $ do
        plain $ trackListingContent tracks

      -- Schedule slot section (only shown if episode is in future or user is staff)
      when (allowFileUpload || isStaff) $ do
        section "SCHEDULE SLOT" $ do
          if null upcomingDates
            then plain $
              Lucid.div_ $ do
                Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Scheduled Date"
                Lucid.div_
                  [Lucid.class_ "w-full p-3 border-2 border-gray-300 bg-gray-50 font-mono text-sm"]
                  (Lucid.toHtml $ "Current: " <> display episode.scheduledAt)
                Lucid.p_
                  [Lucid.class_ "text-sm text-gray-600 mt-2 italic"]
                  "No other available time slots. To change, cancel and create a new episode."
            else selectField "scheduled_date" $ do
              label "Scheduled Date"
              hint "Choose when this episode will air"
              -- Current slot as first option (preselected)
              addOption (encodeCurrentScheduleValue episode) (display episode.scheduledAt <> " (Current)")
              -- Other available slots
              mapM_ (\usd -> addOption (encodeScheduleValue usd) (display usd)) upcomingDates

      section "PUBLISHING" $ do
        toggleField "status" $ do
          offLabel "Draft"
          onLabel "Published"
          offValue "draft"
          onValue "published"
          when isPublished checked
          unless canChangeStatus disabled
          hint $
            if canChangeStatus
              then "Published episodes will be visible after their scheduled date"
              else "Status locked - scheduled date has passed"

      submitButton "SAVE CHANGES"
      cancelButton [i|/#{episodeBackUrl}|] "CANCEL"

--------------------------------------------------------------------------------
-- Track listing

trackListingContent :: [EpisodeTrack.Model] -> Lucid.Html ()
trackListingContent tracks = do
  Lucid.div_ [Lucid.id_ "tracks-container", class_ $ base ["space-y-4"]] $ do
    if null tracks
      then Lucid.p_ [class_ $ base [Tokens.textGray600, "italic"]] "No tracks added yet."
      else forM_ (zip [(0 :: Int) ..] tracks) $ uncurry renderTrackEditor
  addTrackButton

addTrackButton :: Lucid.Html ()
addTrackButton = do
  Lucid.div_ [class_ $ base ["mt-4"]] $ do
    Lucid.button_
      [ Lucid.type_ "button",
        Lucid.id_ "add-track-btn",
        class_ $ base ["bg-blue-600", Tokens.textWhite, Tokens.px6, "py-2", Tokens.fontBold, "hover:bg-blue-700"]
      ]
      "+ ADD TRACK"

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
