{-# LANGUAGE ViewPatterns #-}

-- | Handler for the public store checkout page.
module API.Store.Checkout.Get.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import API.Store.Checkout.Get.Templates (template)
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Control.Monad.Reader (asks)
import Data.Has (getter)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Lucid qualified
import Stripe.Types (StripePublishableKey)

--------------------------------------------------------------------------------

-- | Handler for the checkout page.
--
-- Reads the Stripe publishable key from the application context so
-- the template can conditionally load Stripe.js and mount the embedded
-- checkout widget.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- fmap snd <$> getUserInfo cookie
  mStripeKey <- asks (getter @(Maybe StripePublishableKey))
  renderTemplate hxRequest mUserInfo (template mStripeKey)
