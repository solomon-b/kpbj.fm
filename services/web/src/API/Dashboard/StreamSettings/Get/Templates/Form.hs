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
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.StreamSettings qualified as StreamSettings
import Lucid qualified
import Lucid.Form.Builder
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

-- URL helper
dashboardStreamSettingsEditPostUrl :: Links.URI
dashboardStreamSettingsEditPostUrl = Links.linkURI dashboardStreamSettingsLinks.editPost

--------------------------------------------------------------------------------

-- | Stream settings edit template using FormBuilder
template :: StreamSettings.Model -> Maybe IcecastStatus -> Maybe Text -> Lucid.Html ()
template settings mStatus mError = do
  maybe mempty errorAlert mError

  -- Stream status section (if available)
  statusSection mStatus

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

      -- Stream URL Section
      section "STREAM CONFIGURATION" $ do
        textField "stream_url" $ do
          label "Stream URL"
          placeholder "https://example.com/listen/station/radio.mp3"
          value settings.ssStreamUrl
          required
          hint "The direct URL to the audio stream (MP3 or OGG format)"

        textField "metadata_url" $ do
          label "Metadata URL"
          placeholder "https://example.com/api/nowplaying/station"
          value settings.ssMetadataUrl
          required
          hint "The API endpoint that returns current track information in JSON format"

      -- Info section
      infoSection

      submitButton "SAVE SETTINGS"

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
        Lucid.toHtml $ fromMaybe "Unknown" status.isTitle

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

-- | Error alert component
errorAlert :: Text -> Lucid.Html ()
errorAlert message =
  Lucid.div_
    [class_ $ base [Tokens.p4, Tokens.mb4, Tokens.textSm, Tokens.errorText, "rounded-lg", Tokens.errorBg], Lucid.role_ "alert"]
    $ Lucid.toHtml message

-- | Information section about the stream settings
infoSection :: FormBuilder
infoSection = plain $ do
  Lucid.div_ [class_ $ base [Tokens.bgAlt, Tokens.p4, "rounded", Tokens.mb6]] $ do
    Lucid.h3_ [class_ $ base [Tokens.fontBold, Tokens.mb2, Tokens.textSm]] "About Stream Settings"
    Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, "space-y-2"]] $ do
      Lucid.p_ "These settings configure the web player that appears on the site."
      Lucid.ul_ [class_ $ base ["list-disc", "pl-4", "space-y-1"]] $ do
        Lucid.li_ $ do
          Lucid.strong_ "Stream URL: "
          "The direct link to your Icecast/Shoutcast audio stream"
        Lucid.li_ $ do
          Lucid.strong_ "Metadata URL: "
          "An API endpoint that returns JSON with current track information (used for Now Playing display)"
