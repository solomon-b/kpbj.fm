{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.SitePages.Slug.Edit.Get.Handler (handler, action, PageEditViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Slug.Edit.Get.Templates.Form (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Database.Tables.User qualified as User
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
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata pageSlug
    lift $
      renderDashboardTemplate
        hxRequest
        vd.pevUserMetadata
        vd.pevAllShows
        vd.pevSelectedShow
        NavSitePages
        Nothing
        Nothing
        (template vd.pevPage Nothing)

--------------------------------------------------------------------------------

-- | All data needed to render the site page edit form.
data PageEditViewData = PageEditViewData
  { pevUserMetadata :: UserMetadata.Model,
    pevAllShows :: [Shows.Model],
    pevSelectedShow :: Maybe Shows.Model,
    pevPage :: SitePages.Model
  }

-- | Business logic: fetch shows, fetch page by slug.
action :: User.Model -> UserMetadata.Model -> Text -> ExceptT HandlerError AppM PageEditViewData
action user userMetadata pageSlug = do
  -- 1. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult

  -- 2. Fetch the page by slug
  page <-
    fromMaybeM (throwNotFound "Site page not found.") $
      fromRightM throwDatabaseError $
        execQuery (SitePages.getPageBySlug pageSlug)

  pure
    PageEditViewData
      { pevUserMetadata = userMetadata,
        pevAllShows = allShows,
        pevSelectedShow = listToMaybe allShows,
        pevPage = page
      }
