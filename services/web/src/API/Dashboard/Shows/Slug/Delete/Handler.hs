{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Shows.Slug.Delete.Handler where

--------------------------------------------------------------------------------

import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (handleBannerErrors)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (void)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Class (runDBTransaction)
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Pool qualified as HSQL
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  Slug ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler targetSlug cookie =
  handleBannerErrors "Show delete" $ do
    -- Require admin authentication
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can delete shows." userMetadata

    -- Execute deletion
    executeShowDeletion targetSlug >>= renderDeleteResult

--------------------------------------------------------------------------------

-- | Result of attempting to delete a show.
data DeleteResult
  = DeleteSuccess Shows.Id Text
  | TargetShowNotFound Slug
  | DeleteFailed HSQL.UsageError
  deriving stock (Show, Eq)

-- | Execute show deletion with database operations.
executeShowDeletion ::
  Slug ->
  AppM DeleteResult
executeShowDeletion targetSlug = do
  result <- runDBTransaction $ runMaybeT $ do
    showRec <- MaybeT $ TRX.statement () (Shows.getShowBySlug targetSlug)
    void $ MaybeT $ TRX.statement () (Shows.softDeleteShow showRec.id)
    pure (showRec.id, showRec.title)

  pure $ case result of
    Left err ->
      DeleteFailed err
    Right Nothing ->
      TargetShowNotFound targetSlug
    Right (Just (showId, showTitle)) ->
      DeleteSuccess showId showTitle

-- | Render the appropriate HTML response based on deletion result.
renderDeleteResult :: (Log.MonadLog m) => DeleteResult -> m (Lucid.Html ())
renderDeleteResult = \case
  DeleteSuccess sid showTitle -> do
    Log.logInfo "Show soft deleted successfully" (Aeson.object ["showId" .= display sid])
    -- Return empty to remove the row + OOB success banner
    pure $ do
      mempty
      renderBanner Success "Show Deleted" ("Show \"" <> showTitle <> "\" has been successfully deleted.")
  TargetShowNotFound slug -> do
    Log.logInfo "Show already deleted or not found during delete" (Aeson.object ["slug" .= display slug])
    pure $ renderBanner Error "Delete Failed" "Show not found."
  DeleteFailed err -> do
    Log.logInfo "Database error" (Aeson.object ["error" .= show err])
    pure $ renderBanner Error "Delete Failed" "Failed to delete show. Please try again."
