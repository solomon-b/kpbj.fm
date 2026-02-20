{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.SitePages.Slug.Edit.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Slug.Edit.Post.Route (EditForm (..))
import API.Links (dashboardSitePagesLinks, rootLink)
import API.Types (DashboardSitePagesRoutes (..))
import App.Handler.Combinators (requireAuth, requireRight, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Effects.ContentSanitization qualified as Sanitize
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
  Maybe Cookie ->
  Maybe HxRequest ->
  EditForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler pageSlug cookie _hxRequest editForm =
  handleRedirectErrors "Site page update" dashboardSitePagesLinks.list $ do
    (user, _userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to edit site pages." _userMetadata
    wasUpdated <- action user pageSlug editForm
    let listUrl = rootLink dashboardSitePagesLinks.list
        banner =
          if wasUpdated
            then BannerParams Success "Page Updated" "The site page has been updated successfully."
            else BannerParams Info "No Changes" "No changes were detected. The page was not updated."
        redirectUrl = buildRedirectUrl listUrl banner
    pure $ Servant.addHeader redirectUrl (redirectWithBanner listUrl banner)

--------------------------------------------------------------------------------

-- | Business logic: validate, fetch page, update in transaction.
--
-- Returns 'True' if content was changed, 'False' if no changes detected.
action ::
  User.Model ->
  Text ->
  EditForm ->
  ExceptT HandlerError AppM Bool
action user pageSlug editForm = do
  -- 1. Sanitize and validate input
  let sanitizedTitle = Sanitize.sanitizeTitle (efTitle editForm)
      sanitizedContent = Sanitize.sanitizeUserContent (efContent editForm)
      sanitizedSummary = case efEditSummary editForm of
        Just s | not (Text.null (Text.strip s)) -> Just (Sanitize.sanitizeTitle s)
        _ -> Nothing

  validTitle <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 200 sanitizedTitle)
  validContent <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 100000 sanitizedContent)

  -- 2. Fetch page and update in transaction (only if content changed)
  let userId = User.mId user
  (updatedPage, wasUpdated) <-
    fromMaybeM (throwNotFound "Site page not found.") $
      fromRightM throwDatabaseError $
        execTransaction $
          runMaybeT $ do
            -- Fetch current page
            page <- MaybeT $ HT.statement () (SitePages.getPageBySlug pageSlug)

            -- Check if there are actual changes
            let titleChanged = page.spmTitle /= validTitle
                contentChanged = page.spmContent /= validContent
                hasChanges = titleChanged || contentChanged

            if hasChanges
              then do
                -- Create revision with current content before update
                let revisionInsert =
                      SitePageRevisions.Insert
                        { SitePageRevisions.spriPageId = page.spmId,
                          SitePageRevisions.spriContent = page.spmContent,
                          SitePageRevisions.spriEditSummary = sanitizedSummary,
                          SitePageRevisions.spriCreatedBy = userId
                        }
                _ <- lift $ HT.statement () (SitePageRevisions.insertRevision revisionInsert)

                -- Update the page
                let updateData =
                      SitePages.Update
                        { SitePages.spuTitle = validTitle,
                          SitePages.spuContent = validContent
                        }
                updated <- MaybeT $ HT.statement () (SitePages.updatePage page.spmId updateData)
                pure (updated, True)
              else
                -- No changes, return existing page
                pure (page, False)

  if wasUpdated
    then Log.logInfo "Successfully updated site page" updatedPage.spmSlug
    else Log.logInfo "No changes detected for site page" updatedPage.spmSlug
  pure wasUpdated
