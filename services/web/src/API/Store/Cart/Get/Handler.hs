{-# LANGUAGE ViewPatterns #-}

module API.Store.Cart.Get.Handler where

import API.Store.Cart.Get.Templates (template)
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.HxRequest qualified as HxRequest
import Lucid qualified

-- | Handler for the cart page.
--
-- Simply renders the cart shell template. All cart data is managed
-- client-side by Alpine.js; the validate endpoint provides the
-- server-rendered item fragments.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (HxRequest.foldHxReq -> hxRequest) = do
  mUserInfo <- fmap snd <$> getUserInfo cookie
  renderTemplate hxRequest mUserInfo template
