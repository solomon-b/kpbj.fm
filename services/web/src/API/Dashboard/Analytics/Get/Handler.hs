{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Analytics.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Analytics.Get.Templates.Page qualified as Templates
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.CustomContext (StreamConfig (..))
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
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
import Data.Foldable (toList)
import Data.Has (getter)
import Data.Int (Int32, Int64)
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Network.HTTP.Client qualified as HTTPClient
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Simple qualified as HTTP

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Analytics" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can access analytics." userMetadata
    vd <- action user userMetadata
    lift $
      renderDashboardTemplate
        hxRequest
        vd.avUserMetadata
        vd.avAllShows
        (listToMaybe vd.avAllShows)
        NavAnalytics
        Nothing
        Nothing
        (Templates.template vd.avCurrentListeners)

--------------------------------------------------------------------------------

-- | All data needed to render the analytics page.
data AnalyticsViewData = AnalyticsViewData
  { avUserMetadata :: UserMetadata.Model,
    avAllShows :: [Shows.Model],
    avCurrentListeners :: Maybe Int64
  }

-- | Business logic: fetch shows and current listener count from Icecast.
action :: User.Model -> UserMetadata.Model -> ExceptT HandlerError AppM AnalyticsViewData
action user userMetadata = do
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult

  currentListeners <- lift fetchCurrentListeners

  pure
    AnalyticsViewData
      { avUserMetadata = userMetadata,
        avAllShows = allShows,
        avCurrentListeners = currentListeners
      }

--------------------------------------------------------------------------------

-- | Fetch the current listener count directly from Icecast.
fetchCurrentListeners :: AppM (Maybe Int64)
fetchCurrentListeners = do
  streamCfg <- asks (getter @StreamConfig)
  result <- liftIO $ try @HTTP.HttpException $ do
    request <- HTTP.parseRequest (Text.unpack streamCfg.scMetadataUrl)
    let request' = HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro 5_000_000) request
    response <- HTTP.httpLBS request'
    pure (HTTP.getResponseBody response)
  case result of
    Left _ -> pure Nothing
    Right body -> pure (fromIntegral @Int32 @Int64 <$> parseListenerCount body)

-- | Parse listener count from Icecast JSON status response.
--
-- Handles both single-source (JSON object) and multi-source (JSON array)
-- responses.
parseListenerCount :: LBS.ByteString -> Maybe Int32
parseListenerCount body = do
  json <- Aeson.decode body
  case json of
    Object root -> do
      Object icestats <- KeyMap.lookup "icestats" root
      source <- case KeyMap.lookup "source" icestats of
        Just (Object src) -> Just src
        Just (Array arr) -> case toList arr of
          (Object src : _) -> Just src
          _ -> Nothing
        _ -> Nothing
      Number n <- KeyMap.lookup (fromText "listeners") source
      pure (round n)
    _ -> Nothing
