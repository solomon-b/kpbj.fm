{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.SitePages.Slug.History.Get.Handler (handler, action, PageHistoryViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Slug.History.Get.Templates.Page (template)
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
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execTransaction)
import Effects.Database.Tables.SitePageRevisions qualified as SitePageRevisions
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

handler ::
  Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler pageSlug cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Site page history" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action userMetadata pageSlug
    lift $
      renderDashboardTemplate
        hxRequest
        vd.phvUserMetadata
        NoShow
        NavSitePages
        Nothing
        Nothing
        (template vd.phvPage vd.phvRevisions)

--------------------------------------------------------------------------------

-- | All data needed to render the site page history page.
data PageHistoryViewData = PageHistoryViewData
  { phvUserMetadata :: UserMetadata.Model,
    phvPage :: SitePages.Model,
    phvRevisions :: [SitePageRevisions.RevisionWithEditor]
  }

-- | Business logic: fetch page and revisions in transaction.
action :: UserMetadata.Model -> Text -> ExceptT HandlerError AppM PageHistoryViewData
action userMetadata pageSlug = do
  -- Fetch page and revisions in transaction
  (page, revisions) <-
    fromMaybeM (throwNotFound "Site page not found.") $
      fromRightM throwDatabaseError $
        execTransaction $
          runMaybeT $ do
            p <- MaybeT $ HT.statement () (SitePages.getPageBySlug pageSlug)
            revs <- lift $ HT.statement () (SitePageRevisions.getRevisionsForPage p.spmId)
            pure (p, revs)

  pure
    PageHistoryViewData
      { phvUserMetadata = userMetadata,
        phvPage = page,
        phvRevisions = revisions
      }
