{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Store.Orders.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Store.Orders.Get.Templates (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Auth.Roles (RoleAuth (..), Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQueryThrow)
import Effects.Database.Tables.Orders qualified as Orders
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified

--------------------------------------------------------------------------------

-- | Servant handler: render the orders list page with dashboard chrome.
handler ::
  Satisfies 'Staff RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Store orders" apiLinks.rootGet $ do
    -- Fetch all orders (newest first)
    orders <- lift $ execQueryThrow (Orders.listOrders Nothing)

    lift $
      renderDashboardTemplate
        hxRequest
        auth.raMetadata
        NoShow
        NavStoreOrders
        Nothing
        Nothing
        (template orders)
