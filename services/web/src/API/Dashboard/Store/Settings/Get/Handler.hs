{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Store.Settings.Get.Handler (handler, action, SettingsViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Store.Settings.Get.Templates (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwHandlerFailure)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.StoreSettings qualified as StoreSettings
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the store settings page.
data SettingsViewData = SettingsViewData
  { svdUserMetadata :: UserMetadata.Model,
    svdSettings :: StoreSettings.Model
  }

-- | Business logic: fetch store settings.
action ::
  UserMetadata.Model ->
  ExceptT HandlerError AppM SettingsViewData
action userMetadata = do
  -- Fetch store settings (single-row table, id = 1)
  settingsResult <- fromRightM throwDatabaseError $ execQuery StoreSettings.getSettings
  settings <- case settingsResult of
    Just s -> pure s
    Nothing -> throwHandlerFailure "Store settings row not found (expected id = 1)"

  pure
    SettingsViewData
      { svdUserMetadata = userMetadata,
        svdSettings = settings
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Store settings" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "You do not have permission to access store settings." userMetadata
    vd <- action userMetadata
    lift $
      renderDashboardTemplate
        hxRequest
        vd.svdUserMetadata
        NoShow
        NavStoreSettings
        Nothing
        Nothing
        (template vd.svdSettings)
