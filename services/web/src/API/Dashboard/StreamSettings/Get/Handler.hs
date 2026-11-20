{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StreamSettings.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.StreamSettings.Get.Templates.Form (IcecastStatus (..), template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Auth.Roles (RoleAuth (..), Satisfies (..))
import App.CustomContext (StreamConfig (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Either (fromRight)
import Data.Has (getter)
import Data.Text qualified as Text
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.PlaybackHistory qualified as PlaybackHistory
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Simple qualified as HTTP

--------------------------------------------------------------------------------

handler ::
  Satisfies 'UserMetadata.Admin RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Stream settings" apiLinks.rootGet $ do
    vd <- action auth.raMetadata
    lift $
      renderDashboardTemplate
        hxRequest
        vd.ssvUserMetadata
        NoShow
        NavStreamSettings
        Nothing
        Nothing
        (template vd.ssvIcecastReachable vd.ssvIcecastStatus vd.ssvRecentHistory)

--------------------------------------------------------------------------------

-- | All data needed to render the stream settings page.
data StreamSettingsViewData = StreamSettingsViewData
  { ssvUserMetadata :: UserMetadata.Model,
    ssvIcecastReachable :: Bool,
    ssvIcecastStatus :: Maybe IcecastStatus,
    ssvRecentHistory :: [PlaybackHistory.Model]
  }

-- | Business logic: fetch icecast status, playback history.
action :: UserMetadata.Model -> ExceptT HandlerError AppM StreamSettingsViewData
action userMetadata = do
  -- 1. Read stream config from environment
  streamCfg <- asks (getter @StreamConfig)

  -- 2. Fetch icecast status
  (icecastReachable, icecastStatus) <- lift $ fetchIcecastStatus streamCfg.scMetadataUrl

  -- 3. Fetch recent playback history
  historyResult <- execQuery (PlaybackHistory.getRecentPlayback 50)
  let recentHistory = fromRight [] historyResult

  pure
    StreamSettingsViewData
      { ssvUserMetadata = userMetadata,
        ssvIcecastReachable = icecastReachable,
        ssvIcecastStatus = icecastStatus,
        ssvRecentHistory = recentHistory
      }

-- | Fetch status from icecast metadata endpoint.
--
-- Returns a pair: whether Icecast responded at all (Bool), and
-- the parsed source status if available (Maybe IcecastStatus).
-- Uses 'httpLBS' with a 5-second timeout to avoid hanging when
-- Icecast is down, and to gracefully handle non-JSON responses.
fetchIcecastStatus :: Text.Text -> AppM (Bool, Maybe IcecastStatus)
fetchIcecastStatus metadataUrl = do
  result <- liftIO $ try @HTTP.HttpException $ do
    request <- HTTP.parseRequest (Text.unpack metadataUrl)
    let request' = HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro 5_000_000) request
    response <- HTTP.httpLBS request'
    pure (HTTP.getResponseBody response)

  case result of
    Left _err -> pure (False, Nothing)
    Right body -> case Aeson.decode body of
      Nothing -> pure (False, Nothing)
      Just json -> pure (True, parseIcecastStatus json)

-- | Parse icecast JSON into our status type.
parseIcecastStatus :: Value -> Maybe IcecastStatus
parseIcecastStatus (Object root) = do
  Object icestats <- KeyMap.lookup "icestats" root
  Object source <- KeyMap.lookup "source" icestats
  pure
    IcecastStatus
      { isTitle = getString "title" source,
        isArtist = getString "artist" source,
        isListeners = getInteger "listeners" source,
        isListenerPeak = getInteger "listener_peak" source,
        isServerName = getString "server_name" source,
        isServerDescription = getString "server_description" source,
        isGenre = getString "genre" source,
        isBitrate = getString "audio_info" source >>= extractBitrate,
        isStreamStart = getString "stream_start" source,
        isServerStart = getString "server_start" icestats
      }
parseIcecastStatus _ = Nothing

-- | Extract a string value from a JSON object.
getString :: Text.Text -> KeyMap.KeyMap Value -> Maybe Text.Text
getString k obj = case KeyMap.lookup (fromText k) obj of
  Just (String s) -> Just s
  _ -> Nothing

-- | Extract an integer value from a JSON object.
getInteger :: Text.Text -> KeyMap.KeyMap Value -> Maybe Integer
getInteger k obj = case KeyMap.lookup (fromText k) obj of
  Just (Number n) -> Just (round n)
  _ -> Nothing

-- | Extract bitrate from audio_info string like "channels=2;samplerate=44100;bitrate=128"
extractBitrate :: Text.Text -> Maybe Text.Text
extractBitrate audioInfo =
  case filter (Text.isPrefixOf "bitrate=") (Text.splitOn ";" audioInfo) of
    (x : _) -> Just $ Text.drop 8 x <> " kbps"
    [] -> Nothing
