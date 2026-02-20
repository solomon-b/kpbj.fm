{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.New.Get.Handler (handler, action, ShowNewGetViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.New.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the new show form page.
data ShowNewGetViewData = ShowNewGetViewData
  { sngvUserMetadata :: UserMetadata.Model,
    sngvEligibleHosts :: [UserMetadata.UserWithMetadata]
  }

-- | Business logic: fetch eligible hosts.
action ::
  UserMetadata.Model ->
  ExceptT HandlerError AppM ShowNewGetViewData
action userMetadata = do
  -- 1. Fetch all users for the host selection dropdown
  eligibleHosts <-
    fromRightM throwDatabaseError $
      execQuery (UserMetadata.getAllUsersWithPagination 1000 0)

  pure
    ShowNewGetViewData
      { sngvUserMetadata = userMetadata,
        sngvEligibleHosts = eligibleHosts
      }

handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "New show form" apiLinks.rootGet $ do
    -- 1. Require authentication and staff role
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action userMetadata
    lift $ renderDashboardTemplate hxRequest vd.sngvUserMetadata [] Nothing NavShows Nothing Nothing (template vd.sngvEligibleHosts)
