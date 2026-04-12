{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Store.Orders.Confirmation.Get.Handler where

--------------------------------------------------------------------------------

import API.Store.Orders.Confirmation.Get.Templates (notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQueryThrow)
import Effects.Database.Tables.Orders qualified as Orders
import Lucid qualified

--------------------------------------------------------------------------------

-- | Handler for the order confirmation page.
--
-- Loads the order by its human-readable order number (e.g. "KPBJ-0001"),
-- verifies the provided session_id matches the order's stored Stripe
-- checkout session ID, then renders a thank-you page with a masked email.
-- If the order is not found or the session_id doesn't match, renders a
-- not-found message.
handler ::
  Text ->
  Maybe Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler orderNumber mSessionId cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- fmap snd <$> getUserInfo cookie
  mOrder <- execQueryThrow (Orders.getByOrderNumber orderNumber)
  case mOrder of
    Nothing ->
      renderTemplate hxRequest mUserInfo notFoundTemplate
    Just order ->
      if validSessionId mSessionId order
        then renderTemplate hxRequest mUserInfo (template order)
        else renderTemplate hxRequest mUserInfo notFoundTemplate

-- | Verify that the provided session_id matches the order's stored
-- Stripe checkout session ID. Both must be present and equal.
validSessionId :: Maybe Text -> Orders.Model -> Bool
validSessionId mSessionId order =
  case (mSessionId, order.oStripeCheckoutSessionId) of
    (Just sid, Just storedSid) -> sid == storedSid
    _ -> False
