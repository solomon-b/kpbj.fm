{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.StreamSettings.Edit.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.StreamSettings.Edit.Post.Route (EditForm (..))
import API.Links (dashboardStreamSettingsLinks, rootLink)
import API.Types (DashboardStreamSettingsRoutes (..))
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.StreamSettings qualified as StreamSettings
import Effects.Database.Tables.User qualified as User
import Log qualified
import Lucid qualified
import Servant qualified

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  EditForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler cookie _hxRequest editForm =
  handleRedirectErrors "Stream settings update" dashboardStreamSettingsLinks.get $ do
    -- 1. Require authentication and admin role
    (user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can update stream settings." userMetadata

    -- 2. Update stream settings
    let userId = User.mId user
        updateData =
          StreamSettings.Update
            { StreamSettings.ssuStreamUrl = efStreamUrl editForm,
              StreamSettings.ssuMetadataUrl = efMetadataUrl editForm
            }

    updateResult <- execQuery (StreamSettings.updateStreamSettings userId updateData)

    case updateResult of
      Left err -> throwDatabaseError err
      Right Nothing -> throwNotFound "Stream settings not found in database."
      Right (Just _) -> do
        Log.logInfo "Successfully updated stream settings" (efStreamUrl editForm)
        let listUrl = rootLink dashboardStreamSettingsLinks.get
            banner = BannerParams Success "Settings Updated" "Stream settings have been updated successfully."
            redirectUrl = buildRedirectUrl listUrl banner
        pure $ Servant.addHeader redirectUrl (redirectWithBanner listUrl banner)
