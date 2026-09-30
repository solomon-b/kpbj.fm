{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Underwriters.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Underwriters.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/underwriters.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Underwriter list" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to manage underwriters." userMetadata
    underwriters <- fromRightM throwDatabaseError $ execQuery Underwriters.getAll
    lift $
      renderDashboardTemplate
        hxRequest
        userMetadata
        []
        Nothing
        NavUnderwriters
        Nothing
        Nothing
        (template underwriters)
