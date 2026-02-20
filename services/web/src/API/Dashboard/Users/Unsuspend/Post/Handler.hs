{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Users.Unsuspend.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Get.Templates.Page (renderUserRow)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text.Display (display)
import Data.Time (UTCTime, getCurrentTime)
import Domain.Types.Cookie (Cookie (..))
import Effects.Database.Class (runDBTransaction)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

-- | Result of attempting to unsuspend a user.
data UnsuspendResult
  = UnsuspendSuccess UserMetadata.UserWithMetadata
  | TargetUserNotFound User.Id
  | UnsuspendFailed HSQL.UsageError

-- | Business logic: execute unsuspension.
action ::
  User.Id ->
  ExceptT HandlerError AppM UnsuspendResult
action targetUserId = do
  Log.logInfo "Unsuspending user" (Aeson.object ["targetUserId" .= display targetUserId])

  result <- runDBTransaction $ runMaybeT $ do
    _ <- MaybeT $ TRX.statement () (UserMetadata.getUserWithMetadataById targetUserId)
    _ <- MaybeT $ TRX.statement () (UserMetadata.unsuspendUser targetUserId)
    -- Fetch the updated user after unsuspension
    MaybeT $ TRX.statement () (UserMetadata.getUserWithMetadataById targetUserId)

  pure $ case result of
    Left err -> UnsuspendFailed err
    Right Nothing -> TargetUserNotFound targetUserId
    Right (Just updatedUser) -> UnsuspendSuccess updatedUser

handler ::
  User.Id ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler targetUserId cookie =
  handleBannerErrors "User unsuspend" $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only admins can unsuspend users." userMetadata
    result <- action targetUserId
    now <- liftIO getCurrentTime
    renderUnsuspendResult (User.mId user) now result

--------------------------------------------------------------------------------

-- | Render the appropriate HTML response based on unsuspension result.
renderUnsuspendResult :: (Log.MonadLog m) => User.Id -> UTCTime -> UnsuspendResult -> m (Lucid.Html ())
renderUnsuspendResult viewerId now = \case
  UnsuspendSuccess updatedUser -> do
    Log.logInfo "User unsuspended successfully" (Aeson.object ["userId" .= display updatedUser.uwmUserId, "email" .= display updatedUser.uwmEmail])
    pure $ do
      -- Return the updated row (will replace the old row)
      -- Viewer is Admin since this is an admin-only action
      renderUserRow viewerId UserMetadata.Admin now updatedUser
      -- Also send an OOB success banner
      renderBanner Success "User Unsuspended" (display updatedUser.uwmDisplayName <> "'s suspension has been lifted. They can now use the site normally.")
  TargetUserNotFound uid -> do
    Log.logInfo "User not found during unsuspend" (Aeson.object ["userId" .= display uid])
    pure $ renderBanner Error "Unsuspend Failed" "User not found or not suspended."
  UnsuspendFailed err -> do
    Log.logInfo "Database error during unsuspension" (Aeson.object ["error" .= show err])
    pure $ renderBanner Error "Unsuspend Failed" "Failed to unsuspend user. Please try again."
