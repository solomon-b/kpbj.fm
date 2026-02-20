{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Profile.Edit.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Profile.Edit.Get.Templates.Form (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwUserSuspended)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad (when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Has (getter)
import Data.Maybe (listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Profile edit" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    when (UserMetadata.isSuspended userMetadata) throwUserSuspended
    vd <- action user userMetadata
    lift $ renderDashboardTemplate hxRequest vd.pefUserMetadata vd.pefAllShows vd.pefSelectedShow NavSettings Nothing Nothing (template vd.pefStorageBackend vd.pefUser vd.pefUserMetadata)

--------------------------------------------------------------------------------

-- | All data needed to render the profile edit form.
data ProfileEditFormViewData = ProfileEditFormViewData
  { pefUser :: User.Model,
    pefUserMetadata :: UserMetadata.Model,
    pefAllShows :: [Shows.Model],
    pefSelectedShow :: Maybe Shows.Model,
    pefStorageBackend :: StorageBackend
  }

-- | Business logic: fetch show sidebar and storage backend for profile form.
action ::
  User.Model ->
  UserMetadata.Model ->
  ExceptT HandlerError AppM ProfileEditFormViewData
action user userMetadata = do
  -- 1. Get storage backend for URL construction
  storageBackend <- asks getter

  -- 2. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult
      selectedShow = listToMaybe allShows

  pure
    ProfileEditFormViewData
      { pefUser = user,
        pefUserMetadata = userMetadata,
        pefAllShows = allShows,
        pefSelectedShow = selectedShow,
        pefStorageBackend = storageBackend
      }
