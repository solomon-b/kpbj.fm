{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Analytics.Get.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Analytics.Get.Templates.Page qualified as Templates
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
import Data.Has (getter)
import Data.Int (Int32, Int64)
import Data.Text qualified as Text
import Domain.Icecast.Status (parseListenerCount)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
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
  handleHtmlErrors "Analytics" apiLinks.rootGet $ do
    vd <- action auth.raMetadata
    lift $
      renderDashboardTemplate
        hxRequest
        vd.avUserMetadata
        NoShow
        NavAnalytics
        Nothing
        Nothing
        (Templates.template vd.avCurrentListeners)

--------------------------------------------------------------------------------

-- | All data needed to render the analytics page.
data AnalyticsViewData = AnalyticsViewData
  { avUserMetadata :: UserMetadata.Model,
    avCurrentListeners :: Maybe Int64
  }

-- | Business logic: fetch the current listener count from Icecast.
action :: UserMetadata.Model -> ExceptT HandlerError AppM AnalyticsViewData
action userMetadata = do
  currentListeners <- lift fetchCurrentListeners

  pure
    AnalyticsViewData
      { avUserMetadata = userMetadata,
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
