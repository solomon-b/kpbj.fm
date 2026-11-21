module API.Dashboard.AirBreaks.Underwriters.New.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Underwriters.Form (UnderwriterForm (..))
import API.Dashboard.AirBreaks.Underwriters.Get.Templates.Page (renderTableBody)
import App.Auth.Roles (RoleAuth, Satisfies)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwHandlerFailure, throwValidationError)
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

-- | Handler for POST /dashboard/air-breaks/underwriters/new.
handler ::
  Satisfies 'Staff RoleAuth ->
  UnderwriterForm ->
  AppM (Lucid.Html ())
handler _ form =
  handleBannerErrors "Underwriter create" $ do
    underwriters <- action form
    pure $ do
      renderTableBody underwriters
      renderBanner Success "Saved" "The underwriter list is updated."

-- | Add the underwriter and return the new list.
action :: UnderwriterForm -> ExceptT HandlerError AppM [Underwriters.Model]
action form = do
  let name = Text.strip form.uwfName
  when (Text.null name) $ throwValidationError "A name is required."
  _ <-
    fromMaybeM (throwHandlerFailure "Failed to create the underwriter.") $
      fromRightM throwDatabaseError $
        execQuery (Underwriters.insertUnderwriter name)
  fromRightM throwDatabaseError $ execQuery Underwriters.getAll
