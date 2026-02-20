{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.SitePages.Slug.Revisions.Id.Restore.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Links (dashboardSitePagesLinks, rootLink)
import API.Types (DashboardSitePagesRoutes (..))
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Execute (execTransaction)
import Effects.Database.Tables.SitePageRevisions qualified as SitePageRevisions
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Servant qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

handler ::
  Text ->
  SitePageRevisions.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler pageSlug revisionId cookie _hxRequest =
  handleRedirectErrors "Restore revision" (dashboardSitePagesLinks.historyGet pageSlug) $ do
    (user, _userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to restore revisions." _userMetadata
    action user pageSlug revisionId
    let historyUrl = rootLink $ dashboardSitePagesLinks.historyGet pageSlug
        banner = BannerParams Success "Revision Restored" "The page has been restored to the selected revision."
        redirectUrl = buildRedirectUrl historyUrl banner
    pure $ Servant.addHeader redirectUrl (redirectWithBanner historyUrl banner)

--------------------------------------------------------------------------------

-- | Business logic: fetch page and revision, restore content in transaction.
action ::
  User.Model ->
  Text ->
  SitePageRevisions.Id ->
  ExceptT HandlerError AppM ()
action user pageSlug revisionId = do
  let userId = User.mId user

  updatedPage <-
    fromMaybeM (throwNotFound "Page or revision not found.") $
      fromRightM throwDatabaseError $
        execTransaction $
          runMaybeT $ do
            -- Fetch page and revision
            page <- MaybeT $ HT.statement () (SitePages.getPageBySlug pageSlug)
            revision <- MaybeT $ HT.statement () (SitePageRevisions.getRevisionById revisionId)

            -- Verify revision belongs to this page
            if revision.sprPageId /= page.spmId
              then MaybeT $ pure Nothing
              else do
                -- Create revision with current content (before restore)
                let snapshotInsert =
                      SitePageRevisions.Insert
                        { SitePageRevisions.spriPageId = page.spmId,
                          SitePageRevisions.spriContent = page.spmContent,
                          SitePageRevisions.spriEditSummary = Just "Content before restore",
                          SitePageRevisions.spriCreatedBy = userId
                        }
                _ <- lift $ HT.statement () (SitePageRevisions.insertRevision snapshotInsert)

                -- Update page with restored content
                let updateData =
                      SitePages.Update
                        { SitePages.spuTitle = page.spmTitle, -- Keep title unchanged
                          SitePages.spuContent = revision.sprContent
                        }
                MaybeT $ HT.statement () (SitePages.updatePage page.spmId updateData)

  Log.logInfo "Successfully restored site page revision" updatedPage.spmSlug
