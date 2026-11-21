{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationBlog.New.Get.Handler (handler, action, StationBlogNewViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.New.Get.Templates.Form (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

--------------------------------------------------------------------------------

-- | All data needed to render the new blog post form page.
newtype StationBlogNewViewData = StationBlogNewViewData
  { snvdUserMetadata :: UserMetadata.Model
  }

-- | Business logic: build the view data. This page needs no queries.
action ::
  UserMetadata.Model ->
  ExceptT HandlerError AppM StationBlogNewViewData
action userMetadata =
  pure
    StationBlogNewViewData
      { snvdUserMetadata = userMetadata
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Satisfies 'UserMetadata.Staff RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Blog post create form" apiLinks.rootGet $ do
    vd <- action auth.raMetadata
    lift $
      renderDashboardTemplate
        hxRequest
        vd.snvdUserMetadata
        NoShow
        NavStationBlog
        Nothing
        Nothing
        (template vd.snvdUserMetadata)
