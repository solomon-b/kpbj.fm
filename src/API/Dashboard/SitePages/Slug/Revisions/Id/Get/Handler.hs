{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.SitePages.Slug.Revisions.Id.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Slug.Revisions.Id.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans.Maybe
import Data.Either (fromRight)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.SitePageRevisions qualified as SitePageRevisions
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Text ->
  SitePageRevisions.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer pageSlug revisionId cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "View revision" apiLinks.rootGet $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuerySpan Shows.getAllActiveShows
        else execQuerySpan (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult

    -- 3. Fetch page and revision in transaction
    mResult <- execTransactionSpan $ runMaybeT $ do
      page <- MaybeT $ HT.statement () (SitePages.getPageBySlug pageSlug)
      revision <- MaybeT $ HT.statement () (SitePageRevisions.getRevisionById revisionId)
      -- Verify revision belongs to this page
      if revision.sprPageId == page.spmId
        then pure (page, revision)
        else MaybeT $ pure Nothing

    (page, revision) <- case mResult of
      Left err -> throwDatabaseError err
      Right Nothing -> throwNotFound "Revision not found."
      Right (Just result) -> pure result

    -- 4. Render response with diff between revision and current content
    renderDashboardTemplate hxRequest userMetadata allShows Nothing NavSitePages Nothing Nothing (template page revision)
