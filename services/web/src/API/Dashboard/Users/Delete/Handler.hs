{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Users.Delete.Handler where

--------------------------------------------------------------------------------

import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (void)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Class (runDBTransaction)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  User.Id ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler targetUserId cookie =
  handleBannerErrors "User delete" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can delete users." userMetadata
    result <- action targetUserId
    renderDeleteResult result

--------------------------------------------------------------------------------

-- | Result of attempting to delete a user
data DeleteResult
  = DeleteSuccess User.Id EmailAddress
  | TargetUserNotFound User.Id
  deriving stock (Show, Eq)

-- | Business logic: fetch user, soft delete.
action ::
  User.Id ->
  ExceptT HandlerError AppM DeleteResult
action targetUserId = do
  -- Execute deletion
  result <- runDBTransaction $ runMaybeT $ do
    userWithMeta <- MaybeT $ TRX.statement () (UserMetadata.getUserWithMetadataById targetUserId)
    void $ MaybeT $ TRX.statement () (UserMetadata.softDeleteUser targetUserId)
    pure userWithMeta.uwmEmail

  case result of
    Left err -> throwDatabaseError err
    Right Nothing -> pure $ TargetUserNotFound targetUserId
    Right (Just email) -> pure $ DeleteSuccess targetUserId email

--------------------------------------------------------------------------------

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
