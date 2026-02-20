{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.SitePages.Slug.Revisions.Id.Get.Handler (handler, action, PageRevisionViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Slug.Revisions.Id.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe
import Data.Either (fromRight)
import Data.Maybe (listToMaybe)
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
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata pageSlug revisionId
    lift $
      renderDashboardTemplate
        hxRequest
        vd.prvUserMetadata
        vd.prvAllShows
        vd.prvSelectedShow
        NavSitePages
        Nothing
        Nothing
        (template vd.prvPage vd.prvRevision)

--------------------------------------------------------------------------------

-- | All data needed to render the site page revision detail page.
data PageRevisionViewData = PageRevisionViewData
  { prvUserMetadata :: UserMetadata.Model,
    prvAllShows :: [Shows.Model],
    prvSelectedShow :: Maybe Shows.Model,
    prvPage :: SitePages.Model,
    prvRevision :: SitePageRevisions.Model
  }

-- | Business logic: fetch shows, fetch page and revision in transaction.
action ::
  User.Model ->
  UserMetadata.Model ->
  Text ->
  SitePageRevisions.Id ->
  ExceptT HandlerError AppM PageRevisionViewData
action user userMetadata pageSlug revisionId = do
  -- 1. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult

  -- 2. Fetch page and revision in transaction
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
        prvAllShows = allShows,
        prvSelectedShow = listToMaybe allShows,
        prvPage = page,
        prvRevision = revision
      }
