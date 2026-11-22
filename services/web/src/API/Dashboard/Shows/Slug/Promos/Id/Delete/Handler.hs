-- | @DELETE /dashboard/shows/:show_slug/promos/:promo_id@.
--
-- A host withdraws a promo of a show they host. The response is an empty
-- body, which removes the row, and a success banner.
module API.Dashboard.Shows.Slug.Promos.Id.Delete.Handler
  ( handler,
    action,
  )
where

--------------------------------------------------------------------------------

import App.Auth.Roles (RoleAuth, Satisfies)
import App.Handler.Combinators (authorizeShow)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (unless)
import Control.Monad.Trans.Except (ExceptT)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: check access to the show, run the action, and render.
handler :: Slug -> BreakItems.Id -> Satisfies 'UserMetadata.Host RoleAuth -> AppM (Lucid.Html ())
handler showSlug promoId host =
  handleBannerErrors "Show promo withdraw" $ do
    showModel <- authorizeShow host showSlug
    action showModel promoId
    pure $ do
      mempty
      renderBanner Success "Promo Withdrawn" "It will not air."

-- | Business logic: soft delete a promo of a show that 'authorizeShow'
-- checked.
--
-- The promo must belong to that show. So a host cannot withdraw the promo of
-- another show when they put their own show in the URL, and each page acts
-- only on its own rows.
action ::
  Shows.Model ->
  BreakItems.Id ->
  ExceptT HandlerError AppM ()
action showModel promoId = do
  promo <-
    fromMaybeM (throwNotFound "Show promo") $
      fromRightM throwDatabaseError $
        execQuery (BreakItems.getById promoId)

  unless (promo.bimCategory == BreakItems.ShowPromo && promo.bimShowId == Just (Shows.id showModel)) $
    throwNotFound "Show promo"

  _ <-
    fromMaybeM (throwNotFound "Show promo") $
      fromRightM throwDatabaseError $
        execQuery (BreakItems.softDeleteBreakItem promo.bimId)
  Log.logInfo "Show promo withdrawn" promo.bimId
