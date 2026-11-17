{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.SitePages.Slug.Revisions.Id.Get.Handler (handler, action, PageRevisionViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Slug.Revisions.Id.Get.Templates.Page (template)
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
  SitePageRevisions.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler pageSlug revisionId cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "View revision" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action userMetadata pageSlug revisionId
    lift $
      renderDashboardTemplate
        hxRequest
        vd.prvUserMetadata
        NoShow
        NavSitePages
        Nothing
        Nothing
        (template vd.prvPage vd.prvRevision)

--------------------------------------------------------------------------------

-- | All data needed to render the site page revision detail page.
data PageRevisionViewData = PageRevisionViewData
  { prvUserMetadata :: UserMetadata.Model,
    prvPage :: SitePages.Model,
    prvRevision :: SitePageRevisions.Model
  }

-- | Business logic: fetch page and revision in transaction.
action ::
  UserMetadata.Model ->
  Text ->
  SitePageRevisions.Id ->
  ExceptT HandlerError AppM PageRevisionViewData
action userMetadata pageSlug revisionId = do
  -- Fetch page and revision in transaction
  (page, revision) <-
    fromMaybeM (throwNotFound "Revision not found.") $
      fromRightM throwDatabaseError $
        execTransaction $
          runMaybeT $ do
            p <- MaybeT $ HT.statement () (SitePages.getPageBySlug pageSlug)
            rev <- MaybeT $ HT.statement () (SitePageRevisions.getRevisionById revisionId)
            -- Verify revision belongs to this page
            if rev.sprPageId == p.spmId
              then pure (p, rev)
              else MaybeT $ pure Nothing

  pure
    PageRevisionViewData
      { prvUserMetadata = userMetadata,
        prvPage = page,
        prvRevision = revision
      }
