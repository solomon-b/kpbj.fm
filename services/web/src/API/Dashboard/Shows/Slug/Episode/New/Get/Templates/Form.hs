{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.Slug.Episode.New.Get.Templates.Form
  ( episodeUploadForm,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardShowsLinks)
import API.Types
import Component.AudioDurationScript (renderAudioDurationScript)
import Component.TrackListingEditor qualified as TrackListingEditor
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Form.Builder
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

episodesNewPostUrl :: Slug -> Links.URI
episodesNewPostUrl showSlug = Links.linkURI $ dashboardShowsLinks.episodeNewPost showSlug

dashboardShowDetailUrl :: Shows.Id -> Slug -> Links.URI
dashboardShowDetailUrl showId showSlug = Links.linkURI $ dashboardShowsLinks.detail showId showSlug Nothing

--------------------------------------------------------------------------------

-- | Episode upload form using V2 FormBuilder
episodeUploadForm ::
  -- | Upload URL (bypasses Cloudflare in production)
  Text ->
  Shows.Model ->
  [ShowSchedule.UpcomingShowDate] ->
  UserMetadata.Model ->
  Lucid.Html ()
episodeUploadForm uploadUrl showModel upcomingDates _userMeta = do
  renderForm config form
  renderAudioDurationScript "audio_file-input"
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
              Lucid.label_ [class_ $ base ["block", Tokens.fontBold, Tokens.mb2]] "Scheduled Date"
              Lucid.div_
                [class_ $ base [Tokens.fullWidth, Tokens.p3, Tokens.border2, Tokens.warningBg, "font-mono", Tokens.textSm]]
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
        stagedAudioField "audio_file" uploadUrl "episode_audio" $ do
          label "Episode Audio"
          hint "Upload an MP3. Maximum 500MB."
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

      cancelButton cancelUrl "CANCEL"
      submitButton "SUBMIT"

    -- \| Encode schedule slot value as "template_id|scheduled_at" for form submission
    encodeScheduleValue :: ShowSchedule.UpcomingShowDate -> Text.Text
    encodeScheduleValue usd =
      display (ShowSchedule.usdTemplateId usd) <> "|" <> Text.pack (show $ ShowSchedule.usdStartTime usd)
