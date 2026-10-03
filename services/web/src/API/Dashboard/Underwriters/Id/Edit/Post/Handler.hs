module API.Dashboard.Underwriters.Id.Edit.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.Underwriters.Form (UnderwriterForm (..))
import API.Dashboard.Underwriters.Get.Templates.Page (renderTableBody)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Handler for POST /dashboard/underwriters/:underwriter_id/edit.
handler ::
  Underwriters.Id ->
  Maybe Cookie ->
  UnderwriterForm ->
  AppM (Lucid.Html ())
handler underwriterId cookie form =
  handleBannerErrors "Underwriter rename" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to manage underwriters." userMetadata
    underwriters <- action underwriterId form
    pure $ do
      renderTableBody underwriters
      renderBanner Success "Saved" "The underwriter list is updated."

-- | Rename the underwriter and return the new list.
action :: Underwriters.Id -> UnderwriterForm -> ExceptT HandlerError AppM [Underwriters.Model]
action underwriterId form = do
  let name = Text.strip form.uwfName
  when (Text.null name) $ throwValidationError "A name is required."
  _ <-
    fromMaybeM (throwNotFound "Underwriter") $
      fromRightM throwDatabaseError $
        execQuery (Underwriters.renameUnderwriter underwriterId name)
  fromRightM throwDatabaseError $ execQuery Underwriters.getAll
