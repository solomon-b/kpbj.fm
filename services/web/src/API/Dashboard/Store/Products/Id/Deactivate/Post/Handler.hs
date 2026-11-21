module API.Dashboard.Store.Products.Id.Deactivate.Post.Handler (handler) where

--------------------------------------------------------------------------------

import App.Auth.Roles (RoleAuth, Satisfies)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Data.Text (Text)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

-- | Servant handler: set product is_active = false, remove row from list.
--
-- Uses Pattern C: empty response removes the row from the products table,
-- plus OOB banner confirming the action.
handler ::
  Products.Id ->
  Satisfies 'Staff RoleAuth ->
  AppM (Lucid.Html ())
handler productId _ =
  handleBannerErrors "Product deactivate" $ do
    execQuery (Products.deactivateProduct productId) >>= \case
      Left err -> throwDatabaseError err
      Right Nothing -> throwNotFound "Product"
      Right (Just _) -> Log.logInfo "Product deactivated successfully" productId

    pure $ do
      Lucid.toHtmlRaw ("" :: Text)
      renderBanner Success "Product Deactivated" "The product has been deactivated and is no longer visible in the store."
