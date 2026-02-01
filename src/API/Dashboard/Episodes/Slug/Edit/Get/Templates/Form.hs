{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Episodes.Slug.Edit.Get.Templates.Form
  ( template,
    EpisodeEditContext (..),
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardEpisodesLinks, rootLink)
import API.Types
import Component.TrackListingEditor qualified as TrackListingEditor
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.EpisodeTags qualified as EpisodeTags
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes (Model, artworkUrl, audioFilePath, description, episodeNumber, scheduledAt)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Form.Builder

--------------------------------------------------------------------------------

-- | Context required to render the episode edit form.
--
-- Groups all the data needed by the template into a single record,
-- making the function signature cleaner and easier to maintain.
data EpisodeEditContext = EpisodeEditContext
  { -- | URL for audio uploads (environment-specific, bypasses Cloudflare in prod)
    eecUploadUrl :: Text,
    -- | Storage backend for building media URLs
    eecBackend :: StorageBackend,
    -- | Current time for determining if uploads are allowed
    eecCurrentTime :: UTCTime,
    -- | The show this episode belongs to
    eecShow :: Shows.Model,
    -- | The episode being edited
    eecEpisode :: Episodes.Model,
    -- | Track listing for the episode
    eecTracks :: [EpisodeTrack.Model],
    -- | Tags associated with the episode
    eecTags :: [EpisodeTags.Model],
    -- | Current schedule slot (if template exists)
    eecCurrentSlot :: Maybe ShowSchedule.UpcomingShowDate,
    -- | Available future schedule slots
    eecUpcomingDates :: [ShowSchedule.UpcomingShowDate],
    -- | Whether the user has staff-level permissions or higher
    eecIsStaff :: Bool
  }

--------------------------------------------------------------------------------

episodeIndexUrl :: Slug -> Text
episodeIndexUrl showSlug = rootLink $ dashboardEpisodesLinks.list showSlug Nothing

--------------------------------------------------------------------------------

-- | Check if the episode's scheduled date is in the future (allowing file uploads)
isScheduledInFuture :: UTCTime -> Episodes.Model -> Bool
isScheduledInFuture now episode = episode.scheduledAt > now

-- | Episode edit template.
--
-- Renders the edit form for an existing episode, including:
--
-- - Episode details (description, tags)
-- - Schedule slot selection (if in future or user is staff)
-- - Media file uploads (audio, artwork) if uploads are allowed
-- - Track listing editor
-- - Publish status toggle
template :: EpisodeEditContext -> Lucid.Html ()
template ctx = do
  renderForm config form
  where
    -- Extract from context
    showModel = ctx.eecShow
    episode = ctx.eecEpisode
    tracks = ctx.eecTracks
    episodeTags = ctx.eecTags
    mCurrentSlot = ctx.eecCurrentSlot
    upcomingDates = ctx.eecUpcomingDates
    isStaff = ctx.eecIsStaff
    uploadUrl = ctx.eecUploadUrl
    backend = ctx.eecBackend
    currentTime = ctx.eecCurrentTime

    -- Derived values
    showSlugText = display showModel.slug
    episodeNum = episode.episodeNumber
    episodeNumText = display episodeNum
    episodeBackUrl = episodeIndexUrl showModel.slug
    descriptionValue = fromMaybe "" episode.description
    -- File uploads allowed if scheduled date is in the future OR user is staff/admin
    allowFileUpload = isScheduledInFuture currentTime episode || isStaff
    audioUrl = maybe "" (buildMediaUrl backend) episode.audioFilePath
    artworkUrl = maybe "" (buildMediaUrl backend) episode.artworkUrl

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
                    Lucid.label_ [class_ $ base ["block", Tokens.fontBold, Tokens.mb2]] "Scheduled Date"
                    Lucid.div_
                      [class_ $ base [Tokens.fullWidth, Tokens.p3, Tokens.border2, Tokens.borderDefault, Tokens.bgAlt, "font-mono", Tokens.textSm]]
                      (Lucid.toHtml $ "Current: " <> display episode.scheduledAt)
                    Lucid.p_
                      [class_ $ base [Tokens.textSm, Tokens.fgMuted, "mt-2", "italic"]]
                      "Schedule template not found. To change, cancel and create a new episode."
              Just currentSlot ->
                if null upcomingDates
                  then plain $
                    Lucid.div_ $ do
                      Lucid.label_ [class_ $ base ["block", Tokens.fontBold, Tokens.mb2]] "Scheduled Date"
                      Lucid.div_
                        [class_ $ base [Tokens.fullWidth, Tokens.p3, Tokens.border2, Tokens.borderDefault, Tokens.bgAlt, "font-mono", Tokens.textSm]]
                        (Lucid.toHtml $ display currentSlot <> " (Current)")
                      Lucid.p_
                        [class_ $ base [Tokens.textSm, Tokens.fgMuted, "mt-2", "italic"]]
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
          stagedAudioField "episode_audio" uploadUrl "episode_audio" $ do
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

      cancelButton episodeBackUrl "CANCEL"
      submitButton "SAVE CHANGES"
