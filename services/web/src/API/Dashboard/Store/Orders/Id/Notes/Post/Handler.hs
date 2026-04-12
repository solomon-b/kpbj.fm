{-# LANGUAGE OverloadedRecordDot #-}

-- | Handler for @POST /dashboard/store/orders/:id/notes@.
--
-- Updates the internal notes on an order. Staff and above only.
module API.Dashboard.Store.Orders.Id.Notes.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Store.Orders.Id.Get.Templates (renderNotesSection)
import API.Dashboard.Store.Orders.Id.Notes.Post.Route (NotesForm (..))
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleBannerErrors, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Trans (lift)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Execute (execQueryThrow)
import Effects.Database.Tables.Orders qualified as Orders
import Lucid qualified
import Utils (fromMaybeM)

--------------------------------------------------------------------------------

-- | Servant handler: authenticate, update notes, return re-rendered form + OOB banner.
handler ::
  Orders.Id ->
  Maybe Cookie ->
  NotesForm ->
  AppM (Lucid.Html ())
handler orderId cookie form =
  handleBannerErrors "Order notes update" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to update order notes." userMetadata
    lift $ execQueryThrow (Orders.updateNotes orderId form.nfNotes)
    -- Re-fetch order to render updated notes form (outerHTML swap replaces the form)
    updatedOrder <- fromMaybeM (throwNotFound "Order") $ lift (execQueryThrow (Orders.getById orderId))
    pure $ do
      renderNotesSection updatedOrder
      renderBanner Success "Notes Saved" "Order notes updated."
