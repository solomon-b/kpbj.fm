{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Store.Orders.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Store.Orders.Get.Templates (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQueryThrow)
import Effects.Database.Tables.Orders qualified as Orders
import Lucid qualified

--------------------------------------------------------------------------------

-- | Servant handler: render the orders list page with dashboard chrome.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Store orders" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access store orders." userMetadata

    -- Fetch all orders (newest first)
    orders <- lift $ execQueryThrow (Orders.listOrders Nothing)

    lift $
      renderDashboardTemplate
        hxRequest
        userMetadata
        NoShow
        NavStoreOrders
        Nothing
        Nothing
        (template orders)
