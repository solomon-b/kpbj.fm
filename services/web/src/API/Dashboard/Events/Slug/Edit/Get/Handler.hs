{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.Slug.Edit.Get.Handler (handler, action, EventEditViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Edit.Get.Templates.Form (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad (unless)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe
import Data.Either (fromRight)
import Data.Has (getter)
import Data.Maybe (listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the event edit form.
data EventEditViewData = EventEditViewData
  { eevUserMetadata :: UserMetadata.Model,
    eevAllShows :: [Shows.Model],
    eevSelectedShow :: Maybe Shows.Model,
    eevEvent :: Events.Model,
    eevStorageBackend :: StorageBackend
  }

-- | Business logic: fetch shows, fetch event, authorize, get storage backend.
action ::
  User.Model ->
  UserMetadata.Model ->
  Events.Id ->
  ExceptT HandlerError AppM EventEditViewData
action user userMetadata eventId = do
  -- 1. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult
      selectedShow = listToMaybe allShows

  -- 2. Fetch event
  event <- fetchEvent eventId

  -- 3. Check authorization: must be staff/admin or the creator
  unless (event.emAuthorId == User.mId user || UserMetadata.isStaffOrHigher userMetadata.mUserRole) $
    throwNotAuthorized "You can only edit events you created or have staff permissions." (Just userMetadata.mUserRole)

  -- 4. Get storage backend for URL construction
  storageBackend <- asks getter

  Log.logInfo "Authorized user accessing event edit form" event.emId
  pure
    EventEditViewData
      { eevUserMetadata = userMetadata,
        eevAllShows = allShows,
        eevSelectedShow = selectedShow,
        eevEvent = event,
        eevStorageBackend = storageBackend
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Events.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler eventId _urlSlug cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Event edit form" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to edit events." userMetadata
    vd <- action user userMetadata eventId
    let editTemplate = template vd.eevStorageBackend vd.eevEvent vd.eevUserMetadata
    lift $
      renderDashboardTemplate
        hxRequest
        vd.eevUserMetadata
        vd.eevAllShows
        vd.eevSelectedShow
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
