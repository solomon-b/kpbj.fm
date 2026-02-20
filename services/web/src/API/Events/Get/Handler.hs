{-# LANGUAGE OverloadedRecordDot #-}

module API.Events.Get.Handler where

--------------------------------------------------------------------------------

import API.Events.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Functor ((<&>))
import Data.Has (getter)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.HxRequest qualified as HxRequest
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

data EventsViewData = EventsViewData
  { evdStorageBackend :: StorageBackend,
    evdEvents :: [Events.Model]
  }

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  -- | @hx-request@ header
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie hxRequest =
  handleHtmlErrors "Events list" apiLinks.rootGet $ do
    mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
    vd <- action
    let hxReq = HxRequest.foldHxReq hxRequest
    lift $ renderTemplate hxReq mUserInfo (template vd.evdStorageBackend vd.evdEvents)

--------------------------------------------------------------------------------

-- | Business logic: fetch published events.
action :: ExceptT HandlerError AppM EventsViewData
action = do
  storageBackend <- asks getter
  let limit = 50
      offset = 0
  events <- fromRightM throwDatabaseError $ execQuery (Events.getPublishedEvents limit offset)
  pure
    EventsViewData
      { evdStorageBackend = storageBackend,
        evdEvents = events
      }
