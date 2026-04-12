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
-- then renders a thank-you page with a masked email address. If no matching
-- order is found, renders a simple not-found message instead.
handler ::
  Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler orderNumber cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- fmap snd <$> getUserInfo cookie
  mOrder <- execQueryThrow (Orders.getByOrderNumber orderNumber)
  case mOrder of
    Nothing ->
      renderTemplate hxRequest mUserInfo notFoundTemplate
    Just order ->
      renderTemplate hxRequest mUserInfo (template order)
