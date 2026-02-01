{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.SitePages.Slug.History.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Slug.History.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Either (fromRight)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.SitePageRevisions qualified as SitePageRevisions
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as HT
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler pageSlug cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Site page history" apiLinks.rootGet $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Fetch shows for sidebar
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuery Shows.getAllActiveShows
        else execQuery (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult

    -- 3. Fetch page and revisions in transaction
    mResult <- execTransaction $ runMaybeT $ do
      page <- MaybeT $ HT.statement () (SitePages.getPageBySlug pageSlug)
      revisions <- lift $ HT.statement () (SitePageRevisions.getRevisionsForPage page.spmId)
      pure (page, revisions)

    (page, revisions) <- case mResult of
      Left err -> throwDatabaseError err
      Right Nothing -> throwNotFound "Site page not found."
      Right (Just result) -> pure result

    -- 4. Render response
    renderDashboardTemplate hxRequest userMetadata allShows Nothing NavSitePages Nothing Nothing (template page revisions)
