{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.NewsletterSubscribers.Bulk.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.NewsletterSubscribers.Bulk.Get.Templates (template)
import API.Links (apiLinks)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Flash (FlashMessage)
import Control.Monad.Trans (lift)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/newsletter-subscribers/bulk.
handler ::
  Satisfies 'Staff RoleAuth ->
  Maybe FlashMessage ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (Satisfies _ auth) mFlash (foldHxReq -> hxRequest) =
  handleHtmlErrors "Newsletter subscribers bulk add form" apiLinks.rootGet $ do
    lift $
      renderDashboardTemplate
        hxRequest
        mFlash
        auth.raMetadata
        NoShow
        NavNewsletterSubscribers
        Nothing
        Nothing
        template
