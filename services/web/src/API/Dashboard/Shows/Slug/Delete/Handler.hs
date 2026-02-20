{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Shows.Slug.Delete.Handler where

--------------------------------------------------------------------------------

import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (void)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Class (runDBTransaction)
import Effects.Database.Tables.Shows qualified as Shows
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
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can delete shows." userMetadata
    (_, showTitle) <- action targetSlug
    pure $ do
      mempty
      renderBanner Success "Show Deleted" ("Show \"" <> showTitle <> "\" has been successfully deleted.")

-- | Business logic: look up show, soft-delete it.
--
-- Returns the deleted show's ID and title on success.
action ::
  Slug ->
  ExceptT HandlerError AppM (Shows.Id, Text)
action targetSlug = do
  result <- runDBTransaction $ runMaybeT $ do
    showRec <- MaybeT $ TRX.statement () (Shows.getShowBySlug targetSlug)
    void $ MaybeT $ TRX.statement () (Shows.softDeleteShow showRec.id)
    pure (showRec.id, showRec.title)

  case result of
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just success) -> do
      Log.logInfo "Show soft deleted successfully" (Aeson.object ["showId" .= display (fst success)])
      pure success
