module API.Dashboard.Store.Products.Id.Deactivate.Post.Handler (handler) where

--------------------------------------------------------------------------------

import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Products qualified as Products
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

-- | Servant handler: set product is_active = false, remove row from list.
--
-- Uses Pattern C: empty response removes the row from the products table,
-- plus OOB banner confirming the action.
handler ::
  Products.Id ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler productId cookie =
  handleBannerErrors "Product deactivate" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to deactivate products." userMetadata

    execQuery (Products.deactivateProduct productId) >>= \case
      Left err -> throwDatabaseError err
      Right Nothing -> throwNotFound "Product"
      Right (Just _) -> Log.logInfo "Product deactivated successfully" productId

    pure $ do
      Lucid.toHtmlRaw ("" :: Text)
      renderBanner Success "Product Deactivated" "The product has been deactivated and is no longer visible in the store."
