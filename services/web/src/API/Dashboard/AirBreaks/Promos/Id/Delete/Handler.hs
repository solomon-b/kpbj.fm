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

import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: check the session, run the action, and render.
handler :: BreakItems.Id -> Maybe Cookie -> AppM (Lucid.Html ())
handler promoId cookie =
  handleBannerErrors "Show promo delete" $ do
    (_user, userMetadata) <- requireAuth cookie
    action userMetadata promoId
    pure $ do
      mempty
      renderBanner Success "Promo Deleted" "It has stopped airing. Its past plays stay in the record."

-- | Business logic: check the role, then soft delete the promo.
--
-- An ID of another category is not found, so this route cannot delete a PSA
-- or an underwriting announcement.
action :: UserMetadata.Model -> BreakItems.Id -> ExceptT HandlerError AppM ()
action userMetadata promoId = do
  requireStaffNotSuspended "You do not have permission to manage show promos." userMetadata

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
