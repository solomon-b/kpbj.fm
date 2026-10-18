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

import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT)
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: check the session, run the action, and render.
handler :: Slug -> BreakItems.Id -> Maybe Cookie -> AppM (Lucid.Html ())
handler showSlug promoId cookie =
  handleBannerErrors "Show promo withdraw" $ do
    (user, userMetadata) <- requireAuth cookie
    action user userMetadata showSlug promoId
    pure $ do
      mempty
      renderBanner Success "Promo Withdrawn" "It will not air."

-- | Business logic: check that the user may withdraw this promo, then soft
-- delete it.
--
-- The access check reads the show from the promo row, not from the URL. So a
-- host cannot withdraw the promo of another show when they put their own show
-- in the URL. The URL show must also match, so each page acts only on its own
-- rows.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  BreakItems.Id ->
  ExceptT HandlerError AppM ()
action user userMetadata showSlug promoId = do
  promo <-
    fromMaybeM (throwNotFound "Show promo") $
      fromRightM throwDatabaseError $
        execQuery (BreakItems.getById promoId)

  promoShowId <- case (promo.bimCategory, promo.bimShowId) of
    (BreakItems.ShowPromo, Just showId) -> pure showId
    _ -> throwNotFound "Show promo"

  promoShow <-
    fromMaybeM (throwNotFound "Show") $
      fromRightM throwDatabaseError $
        execQuery (Shows.getShowById promoShowId)

  requireShowHostOrStaff (User.mId user) promoShow.slug userMetadata

  when (promoShow.slug /= showSlug) $
    throwNotFound "Show promo"

  _ <-
    fromMaybeM (throwNotFound "Show promo") $
      fromRightM throwDatabaseError $
        execQuery (BreakItems.softDeleteBreakItem promo.bimId)
  Log.logInfo "Show promo withdrawn" promo.bimId
