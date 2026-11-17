{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.SitePages.Slug.Edit.Get.Handler (handler, action, PageEditViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Slug.Edit.Get.Templates.Form (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

handler ::
  Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler pageSlug cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Edit site page" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action userMetadata pageSlug
    lift $
      renderDashboardTemplate
        hxRequest
        vd.pevUserMetadata
        NoShow
        NavSitePages
        Nothing
        Nothing
        (template vd.pevPage Nothing)

--------------------------------------------------------------------------------

-- | All data needed to render the site page edit form.
data PageEditViewData = PageEditViewData
  { pevUserMetadata :: UserMetadata.Model,
    pevPage :: SitePages.Model
  }

-- | Business logic: fetch page by slug.
action :: UserMetadata.Model -> Text -> ExceptT HandlerError AppM PageEditViewData
action userMetadata pageSlug = do
  page <-
    fromMaybeM (throwNotFound "Site page not found.") $
      fromRightM throwDatabaseError $
        execQuery (SitePages.getPageBySlug pageSlug)

  pure
    PageEditViewData
      { pevUserMetadata = userMetadata,
        pevPage = page
      }
