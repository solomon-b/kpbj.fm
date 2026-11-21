-- | @DELETE /dashboard/air-breaks/promos/:promo_id@.
--
-- Staff delete a show promo in any review status. The response is an empty
-- body, which removes the row, and a success banner.
module API.Dashboard.AirBreaks.Promos.Id.Delete.Handler
  ( handler,
    action,
  )
where

--------------------------------------------------------------------------------

import App.Auth.Roles (RoleAuth, Satisfies)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: run the action, and render.
handler :: BreakItems.Id -> Satisfies 'UserMetadata.Staff RoleAuth -> AppM (Lucid.Html ())
handler promoId _ =
  handleBannerErrors "Show promo delete" $ do
    action promoId
    pure $ do
      mempty
      renderBanner Success "Promo Deleted" "It has stopped airing. Its past plays stay in the record."

-- | Business logic: soft delete the promo.
--
-- An ID of another category is not found, so this route cannot delete a PSA
-- or an underwriting announcement.
action :: BreakItems.Id -> ExceptT HandlerError AppM ()
action promoId = do
  promo <-
    fromMaybeM (throwNotFound "Show promo") $
      fromRightM throwDatabaseError $
        execQuery (BreakItems.getById promoId)
  when (promo.bimCategory /= BreakItems.ShowPromo) $
    throwNotFound "Show promo"

  _ <-
    fromMaybeM (throwNotFound "Show promo") $
      fromRightM throwDatabaseError $
        execQuery (BreakItems.softDeleteBreakItem promo.bimId)
  Log.logInfo "Show promo deleted" promo.bimId
