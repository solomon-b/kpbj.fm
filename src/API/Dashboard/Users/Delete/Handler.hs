{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Users.Delete.Handler where

--------------------------------------------------------------------------------

import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (handleBannerErrors)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.EmailAddress (EmailAddress)
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
handler _tracer targetUserId cookie =
  handleBannerErrors "User delete" $ do
    -- Require admin authentication
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can delete users." userMetadata

    -- Execute deletion
    executeUserDeletion targetUserId >>= renderDeleteResult

--------------------------------------------------------------------------------

-- | Result of attempting to delete a user
data DeleteResult
  = DeleteSuccess User.Id EmailAddress
  | TargetUserNotFound User.Id
  | DeleteFailed HSQL.UsageError
  deriving stock (Show, Eq)

-- | Execute user deletion with database operations
executeUserDeletion ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m
  ) =>
  User.Id ->
  m DeleteResult
executeUserDeletion targetUserId = do
  result <- runDBTransaction $ runMaybeT $ do
    userWithMeta <- MaybeT $ TRX.statement () (UserMetadata.getUserWithMetadataById targetUserId)
    void $ MaybeT $ TRX.statement () (UserMetadata.softDeleteUser targetUserId)
    pure userWithMeta.uwmEmail

  pure $ case result of
    Left err ->
      DeleteFailed err
    Right Nothing ->
      TargetUserNotFound targetUserId
    Right (Just email) ->
      DeleteSuccess targetUserId email

-- | Render the appropriate HTML response based on deletion result
renderDeleteResult :: (Log.MonadLog m) => DeleteResult -> m (Lucid.Html ())
renderDeleteResult = \case
  DeleteSuccess uid email -> do
    Log.logInfo "User soft deleted successfully" (Aeson.object ["userId" .= display uid])
    -- Return empty to remove the row + OOB success banner
    pure $ do
      mempty
      renderBanner Success "User Deleted" ("User " <> display email <> " has been successfully deleted. They can no longer authenticate.")
  TargetUserNotFound uid -> do
    Log.logInfo "User already deleted or not found during delete" (Aeson.object ["userId" .= display uid])
    pure $ renderBanner Error "Delete Failed" "User not found."
  DeleteFailed err -> do
    Log.logInfo "Database error" (Aeson.object ["error" .= show err])
    pure $ renderBanner Error "Delete Failed" "Failed to delete user. Please try again."
