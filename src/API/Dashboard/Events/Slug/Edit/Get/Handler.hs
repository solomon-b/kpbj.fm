{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.Slug.Edit.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Edit.Get.Templates.Form (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans.Maybe
import Data.Either (fromRight)
import Data.Has (Has, getter)
import Data.Maybe (listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

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
    Has StorageBackend env
  ) =>
  Tracer ->
  Events.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer eventId _urlSlug cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Event edit form" apiLinks.rootGet $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to edit events." userMetadata

    -- 2. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuerySpan Shows.getAllActiveShows
        else execQuerySpan (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows

    -- 3. Fetch event
    event <- fetchEvent eventId

    -- 4. Get storage backend for URL construction
    storageBackend <- asks getter

    -- 5. Check authorization: must be staff/admin or the creator
    if not (event.emAuthorId == User.mId user || UserMetadata.isStaffOrHigher userMetadata.mUserRole)
      then throwNotAuthorized "You can only edit events you created or have staff permissions."
      else do
        Log.logInfo "Authorized user accessing event edit form" event.emId
        let editTemplate = template storageBackend event userMetadata
        renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavEvents Nothing Nothing editTemplate

fetchEvent ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadThrow m
  ) =>
  Events.Id ->
  m Events.Model
fetchEvent eventId = do
  mResult <-
    execTransactionSpan $
      runMaybeT $
        MaybeT $
          HT.statement () (Events.getEventById eventId)

  case mResult of
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Event"
    Right (Just e) -> pure e
