-- | @POST /dashboard/air-breaks/promos/:promo_id/reject@.
--
-- Staff reject a pending show promo with a note to the host. The response is
-- the re-rendered row and a success banner.
module API.Dashboard.AirBreaks.Promos.Id.Reject.Post.Handler
  ( handler,
    action,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Promos.Get.Handler (fetchShowsById)
import API.Dashboard.AirBreaks.Promos.Get.Templates (renderPromoRow)
import API.Dashboard.AirBreaks.Promos.Id.Reject.Post.Route (RejectForm (..))
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has qualified as Has
import Data.Text qualified as Text
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
handler :: BreakItems.Id -> Maybe Cookie -> RejectForm -> AppM (Lucid.Html ())
handler promoId cookie form =
  handleBannerErrors "Show promo reject" $ do
    (_user, userMetadata) <- requireAuth cookie
    rejected <- action userMetadata promoId form
    showsById <- fetchShowsById [rejected]
    backend <- asks (Has.getter @StorageBackend)
    pure $ do
      renderPromoRow backend showsById rejected
      renderBanner Success "Promo Rejected" "The host can see your note on their promos page."

-- | Business logic: check the role and the note, then reject the promo.
--
-- The host needs to know what to change, so the note is required. The query
-- changes only a live promo that is pending. Any other ID is not found.
action :: UserMetadata.Model -> BreakItems.Id -> RejectForm -> ExceptT HandlerError AppM BreakItems.Model
action userMetadata promoId form = do
  requireStaffNotSuspended "You do not have permission to review show promos." userMetadata

  let note = Text.strip form.rfNote
  when (Text.null note) $
    throwValidationError "Write a note that tells the host what to change."

  rejected <-
    fromMaybeM (throwNotFound "Pending show promo") $
      fromRightM throwDatabaseError $
        execQuery (BreakItems.rejectPromo promoId note)
  Log.logInfo "Show promo rejected" rejected.bimId
  pure rejected
