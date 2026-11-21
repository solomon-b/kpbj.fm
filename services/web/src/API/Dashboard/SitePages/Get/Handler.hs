{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.SitePages.Get.Handler (handler, action, PageListViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

handler ::
  Satisfies 'UserMetadata.Staff RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Site pages list" apiLinks.rootGet $ do
    vd <- action auth.raMetadata
    lift $
      renderDashboardTemplate
        hxRequest
        vd.plvUserMetadata
        NoShow
        NavSitePages
        Nothing
        Nothing
        (template vd.plvPages)

--------------------------------------------------------------------------------

-- | All data needed to render the site pages list page.
data PageListViewData = PageListViewData
  { plvUserMetadata :: UserMetadata.Model,
    plvPages :: [SitePages.Model]
  }

-- | Business logic: fetch pages.
action :: UserMetadata.Model -> ExceptT HandlerError AppM PageListViewData
action userMetadata = do
  pages <-
    fromRightM throwDatabaseError $
      execQuery SitePages.getAllPages

  pure
    PageListViewData
      { plvUserMetadata = userMetadata,
        plvPages = pages
      }
