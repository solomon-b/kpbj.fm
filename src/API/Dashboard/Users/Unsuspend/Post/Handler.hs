{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Users.Unsuspend.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Get.Templates.Page (renderUserRow)
import App.Common (AuthorizationCheck (..), checkAdminAuthorization, getUserInfo)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Text.Display (display)
import Data.Time (UTCTime, getCurrentTime)
import Domain.Types.Cookie (Cookie (..))
import Effects.Database.Class (MonadDB, runDBTransaction)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

-- | Result of attempting to unsuspend a user
data UnsuspendResult
  = UnsuspendSuccess UserMetadata.UserWithMetadata
  | TargetUserNotFound User.Id
  | UserNotSuspended User.Id
  | UnsuspendFailed HSQL.UsageError

-- | Execute user unsuspension with database operations
executeUnsuspension ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m
  ) =>
  User.Id ->
  m UnsuspendResult
executeUnsuspension targetUserId = do
  result <- runDBTransaction $ runMaybeT $ do
    _ <- MaybeT $ TRX.statement () (UserMetadata.getUserWithMetadataById targetUserId)
    _ <- MaybeT $ TRX.statement () (UserMetadata.unsuspendUser targetUserId)
    -- Fetch the updated user after unsuspension
    MaybeT $ TRX.statement () (UserMetadata.getUserWithMetadataById targetUserId)

  pure $ case result of
    Left err ->
      UnsuspendFailed err
    Right Nothing ->
      TargetUserNotFound targetUserId
    Right (Just updatedUser) ->
      UnsuspendSuccess updatedUser

-- | Render the appropriate HTML response based on unsuspension result
renderUnsuspendResult :: (Log.MonadLog m) => UTCTime -> UnsuspendResult -> m (Lucid.Html ())
renderUnsuspendResult now = \case
  UnsuspendSuccess updatedUser -> do
    Log.logInfo "User unsuspended successfully" (Aeson.object ["userId" .= display updatedUser.uwmUserId, "email" .= display updatedUser.uwmEmail])
    pure $ do
      -- Return the updated row (will replace the old row)
      renderUserRow now updatedUser
      -- Also send an OOB success banner
      renderBanner Success "User Unsuspended" (display updatedUser.uwmDisplayName <> "'s suspension has been lifted. They can now use the site normally.")
  TargetUserNotFound uid -> do
    Log.logInfo "User not found during unsuspend" (Aeson.object ["userId" .= display uid])
    pure $ renderBanner Error "Unsuspend Failed" "User not found or not suspended."
  UserNotSuspended uid -> do
    Log.logInfo "User not suspended" (Aeson.object ["userId" .= display uid])
    pure $ renderBanner Error "Unsuspend Failed" "User is not currently suspended."
  UnsuspendFailed err -> do
    Log.logInfo "Database error during unsuspension" (Aeson.object ["error" .= show err])
    pure $ renderBanner Error "Unsuspend Failed" "Failed to unsuspend user. Please try again."

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  User.Id ->
  Maybe Cookie ->
  m (Lucid.Html ())
handler _tracer targetUserId cookie = do
  userInfo <- getUserInfo cookie

  case checkAdminAuthorization userInfo of
    Unauthorized -> do
      Log.logInfo_ "Unsuspend failed: Unauthorized"
      pure $ renderBanner Error "Unsuspend Failed" "Unauthorized"
    Authorized -> do
      Log.logInfo "Unsuspending user" (Aeson.object ["targetUserId" .= display targetUserId])
      now <- liftIO getCurrentTime
      result <- executeUnsuspension targetUserId
      renderUnsuspendResult now result
