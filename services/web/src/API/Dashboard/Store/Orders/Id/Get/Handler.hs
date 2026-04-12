{-# LANGUAGE ViewPatterns #-}

-- | Handler for @GET /dashboard/store/orders/:id@.
--
-- Renders the full order detail page for staff and above.
module API.Dashboard.Store.Orders.Id.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Store.Orders.Id.Get.Templates (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Data.Either (fromRight)
import Data.Maybe (listToMaybe)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery, execQueryThrow)
import Effects.Database.Tables.OrderItems qualified as OrderItems
import Effects.Database.Tables.Orders qualified as Orders
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromMaybeM)

--------------------------------------------------------------------------------

-- | Servant handler: render the order detail page with dashboard chrome.
handler ::
  Orders.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler orderId cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Store order detail" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to view store orders." userMetadata

    -- Fetch the order; 404 if not found
    order <- fromMaybeM (throwNotFound "Order") $ lift (execQueryThrow (Orders.getById orderId))

    -- Fetch line items for this order
    items <- lift $ execQueryThrow (OrderItems.getByOrderId orderId)

    -- Fetch shows for sidebar navigation
    showsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuery Shows.getAllActiveShows
        else execQuery (Shows.getShowsForUser (User.mId user))
    let allShows = fromRight [] showsResult
        selectedShow = listToMaybe allShows

    lift $
      renderDashboardTemplate
        hxRequest
        userMetadata
        allShows
        selectedShow
        NavStoreOrders
        Nothing
        Nothing
        (template order items)
