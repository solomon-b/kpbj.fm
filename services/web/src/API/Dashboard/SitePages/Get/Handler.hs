{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.SitePages.Get.Handler (handler, action, PageListViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
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
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Site pages list" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata
    lift $
      renderDashboardTemplate
        hxRequest
        vd.plvUserMetadata
        vd.plvAllShows
        vd.plvSelectedShow
        NavSitePages
        Nothing
        Nothing
        (template vd.plvPages)

--------------------------------------------------------------------------------

-- | All data needed to render the site pages list page.
data PageListViewData = PageListViewData
  { plvUserMetadata :: UserMetadata.Model,
    plvAllShows :: [Shows.Model],
    plvSelectedShow :: Maybe Shows.Model,
    plvPages :: [SitePages.Model]
  }

-- | Business logic: fetch shows, fetch pages.
action :: User.Model -> UserMetadata.Model -> ExceptT HandlerError AppM PageListViewData
action user userMetadata = do
  -- 1. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult

  -- 2. Fetch all site pages
  pages <-
    fromRightM throwDatabaseError $
      execQuery SitePages.getAllPages

  pure
    PageListViewData
      { plvUserMetadata = userMetadata,
        plvAllShows = allShows,
        plvSelectedShow = listToMaybe allShows,
        plvPages = pages
      }
