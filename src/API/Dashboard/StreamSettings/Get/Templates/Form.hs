{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StreamSettings.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStreamSettingsLinks)
import API.Types (DashboardStreamSettingsRoutes (..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.StreamSettings qualified as StreamSettings
import Lucid qualified
import Lucid.Form.Builder
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helper
dashboardStreamSettingsEditPostUrl :: Links.URI
dashboardStreamSettingsEditPostUrl = Links.linkURI dashboardStreamSettingsLinks.editPost

--------------------------------------------------------------------------------

-- | Stream settings edit template using FormBuilder
template :: StreamSettings.Model -> Maybe Text -> Lucid.Html ()
template settings mError = do
  maybe mempty errorAlert mError
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
