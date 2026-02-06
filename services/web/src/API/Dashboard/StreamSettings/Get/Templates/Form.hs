{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StreamSettings.Get.Templates.Form
  ( template,
    IcecastStatus (..),
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStreamSettingsLinks)
import API.Types (DashboardStreamSettingsRoutes (..))
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Timezone (utcToPacific)
import Effects.Database.Tables.PlaybackHistory qualified as PlaybackHistory
import Effects.Database.Tables.StreamSettings qualified as StreamSettings
import Lucid qualified
import Lucid.Form.Builder
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Icecast status data parsed from the JSON endpoint.
data IcecastStatus = IcecastStatus
  { isTitle :: Maybe Text,
    isArtist :: Maybe Text,
    isListeners :: Maybe Integer,
    isListenerPeak :: Maybe Integer,
    isServerName :: Maybe Text,
    isServerDescription :: Maybe Text,
    isGenre :: Maybe Text,
    isBitrate :: Maybe Text,
    isStreamStart :: Maybe Text,
    isServerStart :: Maybe Text
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------

-- URL helpers
dashboardStreamSettingsEditPostUrl :: Links.URI
dashboardStreamSettingsEditPostUrl = Links.linkURI dashboardStreamSettingsLinks.editPost

restartIcecastUrl :: Links.URI
restartIcecastUrl = Links.linkURI dashboardStreamSettingsLinks.restartIcecastPost

restartLiquidsoapUrl :: Links.URI
restartLiquidsoapUrl = Links.linkURI dashboardStreamSettingsLinks.restartLiquidsoapPost

--------------------------------------------------------------------------------

-- | Stream settings edit template using FormBuilder
template :: StreamSettings.Model -> Maybe IcecastStatus -> [PlaybackHistory.Model] -> Maybe Text -> Lucid.Html ()
template settings mStatus playbackHistory mError = do
  maybe mempty errorAlert mError

  -- Stream status section (if available)
  statusSection mStatus

  -- Container management section
  containerManagementSection

  -- Playback history section
  playbackHistorySection playbackHistory

  -- Settings form
  renderForm config form
  where
    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = [i|/#{dashboardStreamSettingsEditPostUrl}|],
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content",
          fcHtmxSwap = Just "innerHTML"
        }

    form :: FormBuilder
    form = do
      formTitle "STREAM SETTINGS"
      formSubtitle "Configure the Icecast stream URL and metadata endpoint"

      -- Danger zone section
      dangerZoneSection settings

      submitButton "SAVE SETTINGS"

--------------------------------------------------------------------------------

-- | Danger zone section for stream configuration
dangerZoneSection :: StreamSettings.Model -> FormBuilder
dangerZoneSection settings = plain $ do
  Lucid.div_ [class_ $ base [Tokens.mb6, Tokens.p4, "rounded", "border-2", Tokens.errorBorder, Tokens.errorBg]] $ do
    -- Header
    Lucid.div_ [class_ $ base ["flex", "items-center", "gap-2", Tokens.mb4]] $ do
      Lucid.span_ [class_ $ base [Tokens.errorText, Tokens.fontBold, Tokens.textLg]] "DANGER ZONE"
    Lucid.p_
      [class_ $ base [Tokens.textSm, Tokens.fgMuted, Tokens.mb4]]
      "Changing these settings will affect the live stream. Only modify if you know what you're doing."

    -- Stream URL field
    Lucid.div_ [class_ $ base [Tokens.mb4]] $ do
      Lucid.label_ [class_ $ base [Tokens.fontBold, Tokens.textSm, "block", Tokens.mb2], Lucid.for_ "stream_url"] "Stream URL"
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.name_ "stream_url",
          Lucid.id_ "stream_url",
          Lucid.value_ settings.ssStreamUrl,
          Lucid.placeholder_ "https://example.com/listen/station/radio.mp3",
          Lucid.required_ "required",
          class_ $ base ["w-full", Tokens.p2, Tokens.border2, "rounded", Tokens.bgAlt, "focus:outline-none", "focus:" <> Tokens.errorBorder]
        ]
      Lucid.p_ [class_ $ base [Tokens.textXs, Tokens.fgMuted, "mt-1"]] "The direct URL to the audio stream (MP3 or OGG format)"

    -- Metadata URL field
    Lucid.div_ [] $ do
      Lucid.label_ [class_ $ base [Tokens.fontBold, Tokens.textSm, "block", Tokens.mb2], Lucid.for_ "metadata_url"] "Metadata URL"
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.name_ "metadata_url",
          Lucid.id_ "metadata_url",
          Lucid.value_ settings.ssMetadataUrl,
          Lucid.placeholder_ "https://example.com/api/nowplaying/station",
          Lucid.required_ "required",
          class_ $ base ["w-full", Tokens.p2, Tokens.border2, "rounded", Tokens.bgAlt, "focus:outline-none", "focus:" <> Tokens.errorBorder]
        ]
      Lucid.p_ [class_ $ base [Tokens.textXs, Tokens.fgMuted, "mt-1"]] "The API endpoint that returns current track information in JSON format"

--------------------------------------------------------------------------------

-- | Container management section for restarting services
containerManagementSection :: Lucid.Html ()
containerManagementSection =
  Lucid.div_ [class_ $ base [Tokens.mb6, Tokens.p4, Tokens.bgAlt, "rounded", Tokens.border2]] $ do
    Lucid.h2_ [class_ $ base [Tokens.fontBold, Tokens.textLg, Tokens.mb4]] "CONTAINER MANAGEMENT"
    Lucid.p_
      [class_ $ base [Tokens.textSm, Tokens.fgMuted, Tokens.mb4]]
      "Restart streaming services if needed. This will briefly interrupt the stream."

    Lucid.div_ [class_ $ base ["flex", "flex-wrap", "gap-4"]] $ do
      -- Restart Icecast button
      Lucid.button_
        [ class_ $ base [Tokens.px6, Tokens.py2, Tokens.fontBold, Tokens.border2, Tokens.warningBorder, Tokens.warningText, Tokens.warningBg, "hover:opacity-80"],
          hxPost_ [i|/#{restartIcecastUrl}|],
          hxSwap_ "none",
          hxConfirm_ "Are you sure you want to restart Icecast? This will disconnect all listeners for a few seconds.",
          hxDisabledElt_ "this"
        ]
        "RESTART ICECAST"

      -- Restart Liquidsoap button
      Lucid.button_
        [ class_ $ base [Tokens.px6, Tokens.py2, Tokens.fontBold, Tokens.border2, Tokens.warningBorder, Tokens.warningText, Tokens.warningBg, "hover:opacity-80"],
          hxPost_ [i|/#{restartLiquidsoapUrl}|],
          hxSwap_ "none",
          hxConfirm_ "Are you sure you want to restart Liquidsoap? This will interrupt audio playback for a few seconds.",
          hxDisabledElt_ "this"
        ]
        "RESTART LIQUIDSOAP"

--------------------------------------------------------------------------------

-- | Stream status display section
statusSection :: Maybe IcecastStatus -> Lucid.Html ()
statusSection Nothing =
  Lucid.div_ [class_ $ base [Tokens.mb6, Tokens.p4, Tokens.bgAlt, "rounded", Tokens.border2]] $ do
    Lucid.h2_ [class_ $ base [Tokens.fontBold, Tokens.textLg, Tokens.mb4]] "STREAM STATUS"
    Lucid.div_ [class_ $ base [Tokens.fgMuted, Tokens.textSm]] $
      Lucid.p_ "Unable to connect to stream. Check that Icecast is running and the metadata URL is correct."
statusSection (Just status) =
  Lucid.div_ [class_ $ base [Tokens.mb6, Tokens.p4, Tokens.bgAlt, "rounded", Tokens.border2]] $ do
    Lucid.h2_ [class_ $ base [Tokens.fontBold, Tokens.textLg, Tokens.mb4]] "STREAM STATUS"

    -- Now Playing
    Lucid.div_ [class_ $ base [Tokens.mb4]] $ do
      Lucid.div_ [class_ $ base [Tokens.textXs, Tokens.fgMuted, Tokens.mb2, "uppercase", "tracking-wide"]] "Now Playing"
      Lucid.div_ [class_ $ base [Tokens.fontBold]] $
        Lucid.toHtml $
          fromMaybe "Unknown" status.isTitle

    -- Stats grid
    Lucid.div_ [class_ $ base ["grid", "grid-cols-2", "md:grid-cols-4", "gap-4", Tokens.mb4]] $ do
      statBox "Listeners" $ maybe "0" (Lucid.toHtml . show) status.isListeners
      statBox "Peak" $ maybe "0" (Lucid.toHtml . show) status.isListenerPeak
      statBox "Bitrate" $ maybe "—" Lucid.toHtml status.isBitrate
      statBox "Genre" $ maybe "—" Lucid.toHtml status.isGenre

    -- Stream info
    Lucid.div_ [class_ $ base ["grid", "grid-cols-1", "md:grid-cols-2", "gap-4", Tokens.textSm]] $ do
      infoRow "Server" $ fromMaybe "—" status.isServerName
      infoRow "Description" $ fromMaybe "—" status.isServerDescription
      infoRow "Stream Started" $ maybe "—" formatTimestamp status.isStreamStart
      infoRow "Server Started" $ maybe "—" formatTimestamp status.isServerStart

-- | Stat box component
statBox :: Text -> Lucid.Html () -> Lucid.Html ()
statBox labelText valueHtml =
  Lucid.div_ [class_ $ base [Tokens.p3, Tokens.bgInverse, Tokens.fgInverse, "rounded"]] $ do
    Lucid.div_ [class_ $ base [Tokens.textXs, "opacity-70", Tokens.mb2]] $ Lucid.toHtml labelText
    Lucid.div_ [class_ $ base [Tokens.fontBold, Tokens.textLg]] valueHtml

-- | Info row component
infoRow :: Text -> Text -> Lucid.Html ()
infoRow labelText valueText =
  Lucid.div_ [] $ do
    Lucid.span_ [class_ $ base [Tokens.fgMuted]] $ Lucid.toHtml (labelText <> ": ")
    Lucid.span_ [] $ Lucid.toHtml valueText

-- | Format timestamp for display (just show as-is for now)
formatTimestamp :: Text -> Text
formatTimestamp = id

--------------------------------------------------------------------------------

-- | Playback history section
playbackHistorySection :: [PlaybackHistory.Model] -> Lucid.Html ()
playbackHistorySection [] =
  Lucid.div_ [class_ $ base [Tokens.mb6, Tokens.p4, Tokens.bgAlt, "rounded", Tokens.border2]] $ do
    Lucid.h2_ [class_ $ base [Tokens.fontBold, Tokens.textLg, Tokens.mb4]] "PLAYBACK HISTORY"
    Lucid.div_ [class_ $ base [Tokens.fgMuted, Tokens.textSm]] $
      Lucid.p_ "No playback history recorded yet."
playbackHistorySection history =
  Lucid.div_ [class_ $ base [Tokens.mb6, Tokens.p4, Tokens.bgAlt, "rounded", Tokens.border2]] $ do
    Lucid.h2_ [class_ $ base [Tokens.fontBold, Tokens.textLg, Tokens.mb4]] "PLAYBACK HISTORY"
    Lucid.div_ [class_ $ base ["overflow-x-auto"]] $ do
      Lucid.table_ [class_ $ base ["w-full", Tokens.textSm]] $ do
        Lucid.thead_ $ do
          Lucid.tr_ [class_ $ base ["border-b", Tokens.border2]] $ do
            Lucid.th_ [class_ $ base ["text-left", Tokens.p2, Tokens.fgMuted]] "Started"
            Lucid.th_ [class_ $ base ["text-left", Tokens.p2, Tokens.fgMuted]] "Title"
            Lucid.th_ [class_ $ base ["text-left", Tokens.p2, Tokens.fgMuted]] "Artist"
            Lucid.th_ [class_ $ base ["text-left", Tokens.p2, Tokens.fgMuted]] "Type"
        Lucid.tbody_ $ do
          for_ history $ \entry -> do
            Lucid.tr_ [class_ $ base ["border-b", Tokens.hoverBg]] $ do
              Lucid.td_ [class_ $ base [Tokens.p2, "whitespace-nowrap"]] $
                Lucid.toHtml $
                  formatUTCTime entry.phStartedAt
              Lucid.td_ [class_ $ base [Tokens.p2]] $
                Lucid.toHtml entry.phTitle
              Lucid.td_ [class_ $ base [Tokens.p2]] $
                Lucid.toHtml (fromMaybe "" entry.phArtist)
              Lucid.td_ [class_ $ base [Tokens.p2]] $
                sourceTypeBadge entry.phSourceType

-- | Format UTCTime for display in Pacific time
formatUTCTime :: UTCTime -> Text
formatUTCTime utc = Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (utcToPacific utc)

-- | Badge for source type
sourceTypeBadge :: Text -> Lucid.Html ()
sourceTypeBadge "episode" =
  Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.px3, Tokens.py2, "rounded", Tokens.successBg, Tokens.successText]] "episode"
sourceTypeBadge "ephemeral" =
  Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.px3, Tokens.py2, "rounded", Tokens.infoBg, Tokens.infoText]] "ephemeral"
sourceTypeBadge other =
  Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.px3, Tokens.py2, "rounded", Tokens.bgInverse, Tokens.fgInverse]] $ Lucid.toHtml other

-- | Error alert component
errorAlert :: Text -> Lucid.Html ()
errorAlert message =
  Lucid.div_
    [class_ $ base [Tokens.p4, Tokens.mb4, Tokens.textSm, Tokens.errorText, "rounded-lg", Tokens.errorBg], Lucid.role_ "alert"]
    $ Lucid.toHtml message
