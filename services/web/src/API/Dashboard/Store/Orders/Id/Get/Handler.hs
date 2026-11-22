{-# LANGUAGE ViewPatterns #-}

-- | Handler for @GET /dashboard/store/orders/:id@.
--
-- Renders the full order detail page for staff and above.
module API.Dashboard.Store.Orders.Id.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Store.Orders.Id.Get.Templates (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (handleHtmlErrors, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Flash (FlashMessage)
import Control.Monad.Trans (lift)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQueryThrow)
import Effects.Database.Tables.OrderItems qualified as OrderItems
import Effects.Database.Tables.Orders qualified as Orders
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Utils (fromMaybeM)

--------------------------------------------------------------------------------

-- | Servant handler: render the order detail page with dashboard chrome.
handler ::
  Orders.Id ->
  Satisfies 'Staff RoleAuth ->
  Maybe FlashMessage ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler orderId (Satisfies _ auth) mFlash (foldHxReq -> hxRequest) =
  handleHtmlErrors "Store order detail" apiLinks.rootGet $ do
    -- Fetch the order; 404 if not found
    order <- fromMaybeM (throwNotFound "Order") $ lift (execQueryThrow (Orders.getById orderId))

    -- Fetch line items for this order
    items <- lift $ execQueryThrow (OrderItems.getByOrderId orderId)

    lift $
      renderDashboardTemplate
        hxRequest
        mFlash
        auth.raMetadata
        NoShow
        NavStoreOrders
        Nothing
        Nothing
        (template order items)
