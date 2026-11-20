{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Users.Suspend.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Get.Templates.Page (renderUserRow)
import API.Dashboard.Users.Suspend.Post.Route (SuspendForm (..))
import App.Auth.Roles (RoleAuth (..), Satisfies (..))
import App.Handler.Error (HandlerError, handleBannerErrors)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime, getCurrentTime)
import Effects.Database.Class (runDBTransaction)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

-- | Result of attempting to suspend a user.
data SuspendResult
  = SuspendSuccess UserMetadata.UserWithMetadata
  | TargetUserNotFound User.Id
  | SuspendFailed HSQL.UsageError

-- | Business logic: execute suspension.
action ::
  User.Id ->
  Text ->
  ExceptT HandlerError AppM SuspendResult
action targetUserId reason = do
  Log.logInfo "Suspending user" (Aeson.object ["targetUserId" .= display targetUserId, "reason" .= reason])

  result <- runDBTransaction $ runMaybeT $ do
    _ <- MaybeT $ TRX.statement () (UserMetadata.getUserWithMetadataById targetUserId)
    _ <- MaybeT $ TRX.statement () (UserMetadata.suspendUser targetUserId reason)
    -- Fetch the updated user after suspension
    MaybeT $ TRX.statement () (UserMetadata.getUserWithMetadataById targetUserId)

  pure $ case result of
    Left err -> SuspendFailed err
    Right Nothing -> TargetUserNotFound targetUserId
    Right (Just updatedUser) -> SuspendSuccess updatedUser

handler ::
  User.Id ->
  Satisfies 'UserMetadata.Admin RoleAuth ->
  SuspendForm ->
  AppM (Lucid.Html ())
handler targetUserId (Satisfies _ auth) SuspendForm {..} =
  handleBannerErrors "User suspend" $ do
    result <- action targetUserId sfReason
    now <- liftIO getCurrentTime
    renderSuspendResult (User.mId auth.raUser) now result

--------------------------------------------------------------------------------

-- | Render the appropriate HTML response based on suspension result.
renderSuspendResult :: (Log.MonadLog m) => User.Id -> UTCTime -> SuspendResult -> m (Lucid.Html ())
renderSuspendResult viewerId now = \case
  SuspendSuccess updatedUser -> do
    Log.logInfo "User suspended successfully" (Aeson.object ["userId" .= display updatedUser.uwmUserId, "email" .= display updatedUser.uwmEmail])
    pure $ do
      -- Return the updated row (will replace the old row)
      -- Viewer is Admin since this is an admin-only action
      renderUserRow viewerId UserMetadata.Admin now updatedUser
      -- Also send an OOB success banner
      renderBanner Warning "User Suspended" (display updatedUser.uwmEmail <> " has been suspended. They will see a warning banner and cannot perform host actions.")
  TargetUserNotFound uid -> do
    Log.logInfo "User not found during suspend" (Aeson.object ["userId" .= display uid])
    pure $ renderBanner Error "Suspend Failed" "User not found or already suspended."
  SuspendFailed err -> do
    Log.logInfo "Database error during suspension" (Aeson.object ["error" .= show err])
    pure $ renderBanner Error "Suspend Failed" "Failed to suspend user. Please try again."
