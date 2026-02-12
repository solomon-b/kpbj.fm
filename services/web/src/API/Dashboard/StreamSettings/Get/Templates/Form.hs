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
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Domain.Types.Timezone (utcToPacific)
import Effects.Database.Tables.PlaybackHistory qualified as PlaybackHistory
import Lucid qualified
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
restartIcecastUrl :: Links.URI
restartIcecastUrl = Links.linkURI dashboardStreamSettingsLinks.restartIcecastPost

restartLiquidsoapUrl :: Links.URI
restartLiquidsoapUrl = Links.linkURI dashboardStreamSettingsLinks.restartLiquidsoapPost

--------------------------------------------------------------------------------

-- | Stream settings page template.
--
-- Shows stream status, playback history, and container management controls.
-- Stream URLs are configured via environment variables, not editable here.
template :: Bool -> Maybe IcecastStatus -> [PlaybackHistory.Model] -> Lucid.Html ()
template icecastReachable mStatus playbackHistory = do
  -- Page title
  Lucid.h1_ [class_ $ base [Tokens.fontBold, Tokens.textLg, Tokens.mb6]] "STREAM SETTINGS"

  -- Stream status section
  statusSection icecastReachable mStatus

  -- Playback history section
  playbackHistorySection playbackHistory

  -- Container management section
  containerManagementSection

--------------------------------------------------------------------------------

-- | Container management section for restarting streaming services.
containerManagementSection :: Lucid.Html ()
containerManagementSection =
  Lucid.div_ [class_ $ base [Tokens.mb6, Tokens.p4, Tokens.bgMain, "rounded", "border", Theme.borderMuted]] $ do
    Lucid.h2_ [class_ $ base [Tokens.fontBold, Tokens.textLg, Tokens.mb2]] "CONTAINER MANAGEMENT"
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

-- | Stream status display section.
--
-- Three states: Icecast unreachable (red), reachable but no source (green),
-- reachable with active source (green + full stats).
statusSection :: Bool -> Maybe IcecastStatus -> Lucid.Html ()
statusSection False _ =
  Lucid.div_ [class_ $ base [Tokens.mb6, Tokens.p4, Tokens.bgMain, "rounded", "border", Theme.borderMuted]] $ do
    statusHeader False
    Lucid.div_ [class_ $ base [Tokens.fgMuted, Tokens.textSm]] $
      Lucid.p_ "Unable to connect to stream. Check that Icecast is running and the metadata URL is correct."
statusSection True Nothing =
  Lucid.div_ [class_ $ base [Tokens.mb6, Tokens.p4, Tokens.bgMain, "rounded", "border", Theme.borderMuted]] $ do
    statusHeader True
    Lucid.div_ [class_ $ base [Tokens.fgMuted, Tokens.textSm]] $
      Lucid.p_ "Icecast is running but no active source is connected. Check that Liquidsoap is running."
statusSection True (Just status) =
  Lucid.div_ [class_ $ base [Tokens.mb6, Tokens.p4, Tokens.bgMain, "rounded", "border", Theme.borderMuted]] $ do
    statusHeader True

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

-- | Status section header with green/red indicator dot.
statusHeader :: Bool -> Lucid.Html ()
statusHeader online =
  Lucid.div_ [class_ $ base ["flex", "items-center", "gap-2", Tokens.mb4]] $ do
    statusDot online
    Lucid.h2_ [class_ $ base [Tokens.fontBold, Tokens.textLg]] "STREAM STATUS"

-- | Green/red status indicator dot.
statusDot :: Bool -> Lucid.Html ()
statusDot online =
  Lucid.span_
    [ class_ $ base ["inline-block", "w-3", "h-3", "rounded-full", color]
    ]
    mempty
  where
    color = if online then "bg-[var(--theme-success)]" else "bg-[var(--theme-error)]"

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
  Lucid.div_ [class_ $ base [Tokens.mb6, Tokens.p4, Tokens.bgMain, "rounded", "border", Theme.borderMuted]] $ do
    Lucid.h2_ [class_ $ base [Tokens.fontBold, Tokens.textLg, Tokens.mb4]] "PLAYBACK HISTORY"
    Lucid.div_ [class_ $ base [Tokens.fgMuted, Tokens.textSm]] $
      Lucid.p_ "No playback history recorded yet."
playbackHistorySection history =
  Lucid.div_ [class_ $ base [Tokens.mb6, Tokens.p4, Tokens.bgMain, "rounded", "border", Theme.borderMuted]] $ do
    Lucid.h2_ [class_ $ base [Tokens.fontBold, Tokens.textLg, Tokens.mb4]] "PLAYBACK HISTORY"
    Lucid.div_ [class_ $ base ["overflow-x-auto", "overflow-y-auto", "max-h-[640px]"]] $ do
      Lucid.table_ [class_ $ base ["w-full", Tokens.textSm]] $ do
        Lucid.thead_ [class_ $ base ["sticky", "top-0", Tokens.bgAlt]] $ do
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
