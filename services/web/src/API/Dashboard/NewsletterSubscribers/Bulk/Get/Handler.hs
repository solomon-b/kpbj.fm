{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.NewsletterSubscribers.Bulk.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.NewsletterSubscribers.Bulk.Get.Templates (template)
import API.Links (apiLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Lucid qualified

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/newsletter-subscribers/bulk.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Newsletter subscribers bulk add form" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can add newsletter subscribers." userMetadata
    lift $
      renderDashboardTemplate
        hxRequest
        userMetadata
        []
        Nothing
        NavNewsletterSubscribers
        Nothing
        Nothing
        template
