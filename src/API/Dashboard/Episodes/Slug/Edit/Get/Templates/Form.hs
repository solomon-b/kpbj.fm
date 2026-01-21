{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Episodes.Slug.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (showEpisodesLinks)
import API.Types
import Component.Form.Builder
import Component.TrackListingEditor qualified as TrackListingEditor
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.EpisodeTags qualified as EpisodeTags
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

--------------------------------------------------------------------------------

-- | Check if the episode's scheduled date is in the future (allowing file uploads)
isScheduledInFuture :: UTCTime -> Episodes.Model -> Bool
isScheduledInFuture now episode = episode.scheduledAt > now

-- | Episode edit template
--
-- The currentTime parameter is used to determine if the scheduled date has passed.
-- If the scheduled date is in the future, file upload fields are shown.
-- The isStaff parameter indicates if the user has staff-level permissions or higher.
-- The mCurrentSlot parameter contains the current episode's schedule slot formatted
-- as an UpcomingShowDate so it can be displayed consistently with other slots.
template :: StorageBackend -> UTCTime -> Shows.Model -> Episodes.Model -> [EpisodeTrack.Model] -> [EpisodeTags.Model] -> Maybe ShowSchedule.UpcomingShowDate -> [ShowSchedule.UpcomingShowDate] -> UserMetadata.Model -> Bool -> Lucid.Html ()
template backend currentTime showModel episode tracks episodeTags mCurrentSlot upcomingDates _userMeta isStaff = do
  renderForm config form
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
    audioUrl = maybe "" (buildMediaUrl backend) episode.audioFilePath
    artworkUrl = maybe "" (buildMediaUrl backend) episode.artworkUrl
    isPublished = episode.status == Episodes.Published

    -- Encode schedule slot value as "template_id|scheduled_at" for form submission
    encodeScheduleValue :: ShowSchedule.UpcomingShowDate -> Text.Text
    encodeScheduleValue usd =
      display (ShowSchedule.usdTemplateId usd) <> "|" <> Text.pack (show $ ShowSchedule.usdStartTime usd)

    postUrl = [i|/dashboard/episodes/#{showSlugText}/#{episodeNumText}/edit|]

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = postUrl,
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content",
          fcHtmxSwap = Just "innerHTML"
        }

    -- Format current tags as comma-separated string
    currentTagsValue :: Text.Text
    currentTagsValue = Text.intercalate ", " $ map EpisodeTags.etName episodeTags

    form :: FormBuilder
    form = do
      section "EPISODE DETAILS" $ do
        -- Schedule slot section (only shown if episode is in future or user is staff)
        when (allowFileUpload || isStaff) $ do
          section "SCHEDULE SLOT" $ do
            case mCurrentSlot of
              Nothing ->
                -- No template found, show raw scheduled date
                plain $
                  Lucid.div_ $ do
                    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Scheduled Date"
                    Lucid.div_
                      [Lucid.class_ "w-full p-3 border-2 border-gray-300 dark:border-gray-600 bg-gray-50 font-mono text-sm"]
                      (Lucid.toHtml $ "Current: " <> display episode.scheduledAt)
                    Lucid.p_
                      [Lucid.class_ "text-sm text-gray-600 dark:text-gray-400 mt-2 italic"]
                      "Schedule template not found. To change, cancel and create a new episode."
              Just currentSlot ->
                if null upcomingDates
                  then plain $
                    Lucid.div_ $ do
                      Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Scheduled Date"
                      Lucid.div_
                        [Lucid.class_ "w-full p-3 border-2 border-gray-300 dark:border-gray-600 bg-gray-50 font-mono text-sm"]
                        (Lucid.toHtml $ display currentSlot <> " (Current)")
                      Lucid.p_
                        [Lucid.class_ "text-sm text-gray-600 dark:text-gray-400 mt-2 italic"]
                        "No other available time slots. To change, cancel and create a new episode."
                  else selectField "scheduled_date" $ do
                    label "Scheduled Date"
                    hint "Choose when this episode will air"
                    -- Current slot as first option (preselected), rendered same as upcoming dates
                    addOption (encodeScheduleValue currentSlot) (display currentSlot <> " (Current)")
                    -- Other available slots
                    mapM_ (\usd -> addOption (encodeScheduleValue usd) (display usd)) upcomingDates

        textareaField "description" 6 $ do
          label "Description"
          placeholder "Describe this episode. What tracks did you play? Any special guests or themes?"
          hint "Up to 5000 characters"
          value descriptionValue
          maxLength 5000

        textField "tags" $ do
          label "Tags"
          placeholder "industrial, ambient, glitch, experimental (comma separated)"
          hint "Optional. Comma-separated list of genres/themes"
          value currentTagsValue
          maxLength 500

      when allowFileUpload $ do
        section "MEDIA FILES" $ do
          stagedAudioField "episode_audio" "/api/uploads/audio" "episode_audio" $ do
            label "Episode Audio"
            maxSize 500
            currentFile audioUrl

          imageField "episode_artwork" $ do
            label "Episode Artwork"
            maxSize 5
            aspectRatio (1, 1)
            currentFile artworkUrl

      section "TRACK LISTING" $ do
        plain $
          TrackListingEditor.render
            TrackListingEditor.Config
              { TrackListingEditor.editorId = "edit-episode-tracks",
                TrackListingEditor.initialTracks = map TrackListingEditor.fromEpisodeTrack tracks,
                TrackListingEditor.jsonFieldName = "tracks_json"
              }

      footerToggle "status" $ do
        offLabel "Draft"
        onLabel "Published"
        offValue "draft"
        onValue "published"
        when isPublished checked
        unless canChangeStatus disabled

      footerHint $
        if canChangeStatus
          then "Published episodes will be visible after their scheduled date"
          else "Status locked - scheduled date has passed"

      cancelButton [i|/#{episodeBackUrl}|] "CANCEL"
      submitButton "SAVE CHANGES"
