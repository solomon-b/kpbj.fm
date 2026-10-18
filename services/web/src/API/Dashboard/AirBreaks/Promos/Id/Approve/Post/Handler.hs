-- | @POST /dashboard/air-breaks/promos/:promo_id/approve@.
--
-- Staff approve a pending show promo, so it can air. The response is the
-- re-rendered row and a success banner.
module API.Dashboard.AirBreaks.Promos.Id.Approve.Post.Handler
  ( handler,
    action,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Promos.Get.Handler (fetchShowsById)
import API.Dashboard.AirBreaks.Promos.Get.Templates (renderPromoRow)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has qualified as Has
import Domain.Types.Cookie (Cookie)
import Domain.Types.StorageBackend (StorageBackend)
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
  handleBannerErrors "Show promo approve" $ do
    (_user, userMetadata) <- requireAuth cookie
    approved <- action userMetadata promoId
    showsById <- fetchShowsById [approved]
    backend <- asks (Has.getter @StorageBackend)
    pure $ do
      renderPromoRow backend showsById approved
      -- Today's break plan is built once, so a promo that staff approve today
      -- can air from tomorrow at the earliest.
      renderBanner Success "Promo Approved" "It can air from tomorrow, or from its first air date if that is later."

-- | Business logic: check the role, then approve the promo.
--
-- The query changes only a live promo that is pending. Any other ID is not
-- found, which includes a promo that another staff member reviewed first.
action :: UserMetadata.Model -> BreakItems.Id -> ExceptT HandlerError AppM BreakItems.Model
action userMetadata promoId = do
  requireStaffNotSuspended "You do not have permission to review show promos." userMetadata
  approved <-
    fromMaybeM (throwNotFound "Pending show promo") $
      fromRightM throwDatabaseError $
        execQuery (BreakItems.approvePromo promoId)
  Log.logInfo "Show promo approved" approved.bimId
  pure approved
