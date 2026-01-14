{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Profile.Edit.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Profile.Edit.Get.Templates.Form (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth)
import App.Handler.Error (handleHtmlErrors, throwUserSuspended)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Either (fromRight)
import Data.Has (Has, getter)
import Data.Maybe (listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
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
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Profile edit" apiLinks.rootGet $ do
    -- 1. Require authentication
    (user, userMetadata) <- requireAuth cookie

    -- 2. Check not suspended
    when (UserMetadata.isSuspended userMetadata) throwUserSuspended

    -- 3. Get storage backend for URL construction
    storageBackend <- asks getter

    -- 4. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuerySpan Shows.getAllActiveShows
        else execQuerySpan (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows

    renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavSettings Nothing Nothing (template storageBackend user userMetadata)
