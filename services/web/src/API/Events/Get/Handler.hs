{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Events.Get.Handler where

--------------------------------------------------------------------------------

import API.Events.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Reader (asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (getter)
import Data.String.Interpolate (i)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..))
import Domain.Types.HxRequest qualified as HxRequest
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Events qualified as Events
import Log qualified
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  -- | @hx-request@ header
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie hxRequest = do
  storageBackend <- asks getter
  getUserInfo cookie >>= \mUserInfo -> do
    let limit = 50
        offset = 0

    result <- execQuery (Events.getPublishedEvents limit offset)
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
