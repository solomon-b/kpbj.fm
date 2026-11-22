{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.Slug.Edit.Get.Handler (handler, action, EventEditViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Edit.Get.Templates.Form (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Flash (FlashMessage)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe
import Data.Has (getter)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.EventImages qualified as EventImages
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the event edit form.
data EventEditViewData = EventEditViewData
  { eevUserMetadata :: UserMetadata.Model,
    eevEvent :: Events.Model,
    eevGalleryImages :: [EventImages.Model],
    eevStorageBackend :: StorageBackend
  }

-- | Business logic: fetch event, get storage backend.
action ::
  UserMetadata.Model ->
  Events.Id ->
  ExceptT HandlerError AppM EventEditViewData
action userMetadata eventId = do
  -- 1. Fetch event
  event <- fetchEvent eventId

  -- 2. Fetch existing gallery photos
  galleryImages <-
    fromRightM throwDatabaseError $
      execQuery (EventImages.getByEventId eventId)

  -- 3. Get storage backend for URL construction
  storageBackend <- asks getter

  Log.logInfo "Staff user accessing event edit form" event.emId
  pure
    EventEditViewData
      { eevUserMetadata = userMetadata,
        eevEvent = event,
        eevGalleryImages = galleryImages,
        eevStorageBackend = storageBackend
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Events.Id ->
  Slug ->
  Satisfies 'UserMetadata.Staff RoleAuth ->
  Maybe FlashMessage ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler eventId _urlSlug (Satisfies _ auth) mFlash (foldHxReq -> hxRequest) =
  handleHtmlErrors "Event edit form" apiLinks.rootGet $ do
    vd <- action auth.raMetadata eventId
    let editTemplate = template vd.eevStorageBackend vd.eevEvent vd.eevGalleryImages vd.eevUserMetadata
    lift $
      renderDashboardTemplate
        hxRequest
        mFlash
        vd.eevUserMetadata
        NoShow
        NavEvents
        Nothing
        Nothing
        editTemplate

fetchEvent ::
  Events.Id ->
  ExceptT HandlerError AppM Events.Model
fetchEvent eventId =
  fromMaybeM (throwNotFound "Event") $
    fromRightM throwDatabaseError $
      execTransaction $
        runMaybeT $
          MaybeT $
            HT.statement () (Events.getEventById eventId)
