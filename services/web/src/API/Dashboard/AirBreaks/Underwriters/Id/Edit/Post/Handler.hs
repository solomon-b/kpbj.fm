module API.Dashboard.AirBreaks.Underwriters.Id.Edit.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Underwriters.Form (UnderwriterForm (..))
import API.Dashboard.AirBreaks.Underwriters.Get.Templates.Page (renderTableBody)
import App.Auth.Roles (RoleAuth, Satisfies)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text qualified as Text
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Handler for POST /dashboard/air-breaks/underwriters/:underwriter_id/edit.
handler ::
  Underwriters.Id ->
  Satisfies 'Staff RoleAuth ->
  UnderwriterForm ->
  AppM (Lucid.Html ())
handler underwriterId _ form =
  handleBannerErrors "Underwriter rename" $ do
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
