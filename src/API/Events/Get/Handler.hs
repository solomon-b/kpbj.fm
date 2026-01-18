{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Events.Get.Handler where

--------------------------------------------------------------------------------

import API.Events.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has, getter)
import Data.String.Interpolate (i)
import Domain.Types.Cookie (Cookie)
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId)
import Domain.Types.HxRequest (HxRequest (..))
import Domain.Types.HxRequest qualified as HxRequest
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Events qualified as Events
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env,
    Has (Maybe GoogleAnalyticsId) env,
    Has StorageBackend env
  ) =>
  Tracer ->
  Maybe Cookie ->
  -- | @hx-request@ header
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer cookie hxRequest = do
  storageBackend <- asks getter
  getUserInfo cookie >>= \mUserInfo -> do
    let limit = 50
        offset = 0

    result <- execQuerySpan (Events.getPublishedEvents limit offset)
    page <- case result of
      Left err -> do
        Log.logAttention "Failed to fetch events from database" (Aeson.object ["error" .= show err])
        let banner = BannerParams Error "Error" "Failed to load events. Please try again."
        pure (redirectWithBanner [i|/#{rootGetUrl}|] banner)
      Right events ->
        pure (template storageBackend events)

    let hxReq = HxRequest.foldHxReq hxRequest
        mUserMetadata = fmap snd mUserInfo
    renderTemplate hxReq mUserMetadata page
