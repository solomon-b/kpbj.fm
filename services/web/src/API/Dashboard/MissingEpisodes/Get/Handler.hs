{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.MissingEpisodes.Get.Handler (handler, action, MissingEpisodesViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.MissingEpisodes.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Maybe (listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Missing episodes" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata
    let pageTemplate = template vd.mevMissingEpisodes
    lift $ renderDashboardTemplate hxRequest vd.mevUserMetadata vd.mevAllShows vd.mevSelectedShow NavMissingEpisodes Nothing Nothing pageTemplate

--------------------------------------------------------------------------------

-- | All data needed to render the missing episodes page.
data MissingEpisodesViewData = MissingEpisodesViewData
  { mevUserMetadata :: UserMetadata.Model,
    mevAllShows :: [Shows.Model],
    mevSelectedShow :: Maybe Shows.Model,
    mevMissingEpisodes :: [ShowSchedule.ShowMissingEpisode]
  }

-- | Business logic: show sidebar, missing episode fetching.
action ::
  User.Model ->
  UserMetadata.Model ->
  ExceptT HandlerError AppM MissingEpisodesViewData
action user userMetadata = do
  -- 1. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult
      selectedShow = listToMaybe allShows

  -- 2. Fetch missing episodes
  missingEpisodes <-
    fromRightM throwDatabaseError $
      execQuery ShowSchedule.getShowsMissingEpisodes

  pure
    MissingEpisodesViewData
      { mevUserMetadata = userMetadata,
        mevAllShows = allShows,
        mevSelectedShow = selectedShow,
        mevMissingEpisodes = missingEpisodes
      }
