{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Store.Products.Id.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (apiLinks, dashboardStoreProductsLinks, rootLink)
import API.Types (DashboardStoreProductsRoutes (..), Routes (..))
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Flash (throwHxRedirect)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Products qualified as Products
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: verify the product exists, then redirect to its edit page.
--
-- The product detail URL exists as a stable redirect target (e.g. after
-- creation).  All editing is done on the /edit sub-page.
handler ::
  Products.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler productId cookie (foldHxReq -> _hxRequest) =
  handleHtmlErrors "Product detail" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to view this page." userMetadata

    -- Verify the product exists; 404 if not
    mProduct <- fromRightM throwDatabaseError $ execQuery (Products.getById productId)
    _product <- case mProduct of
      Nothing -> throwNotFound "Product"
      Just p -> pure p

    -- Redirect to the edit page
    lift $ throwHxRedirect (productEditUrl productId) Nothing

--------------------------------------------------------------------------------

-- | Build the edit URL for a product.
productEditUrl :: Products.Id -> Text
productEditUrl pid = rootLink $ dashboardStoreProductsLinks.editGet pid
