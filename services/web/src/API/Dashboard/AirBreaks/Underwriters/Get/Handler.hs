{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.AirBreaks.Underwriters.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Underwriters.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Flash (FlashMessage)
import Control.Monad.Trans (lift)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/air-breaks/underwriters.
handler ::
  Satisfies 'Staff RoleAuth ->
  Maybe FlashMessage ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (Satisfies _ auth) mFlash (foldHxReq -> hxRequest) =
  handleHtmlErrors "Underwriter list" apiLinks.rootGet $ do
    underwriters <- fromRightM throwDatabaseError $ execQuery Underwriters.getAll
    lift $
      renderDashboardTemplate
        hxRequest
        mFlash
        auth.raMetadata
        NoShow
        NavUnderwriters
        Nothing
        Nothing
        (template underwriters)
