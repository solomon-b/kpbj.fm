{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Shows.Slug.Delete.Handler where

--------------------------------------------------------------------------------

import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (void)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Domain.Types.Timezone (LocalTime (..), utcToPacific)
import Effects.Clock (currentSystemTime)
import Effects.Database.Class (runDBTransaction)
import Effects.Database.Tables.Episodes qualified as Episodes
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
  today <- localDay . utcToPacific <$> currentSystemTime
  result <- runDBTransaction $ runMaybeT $ do
    showRec <- MaybeT $ TRX.statement () (Shows.getShowBySlug targetSlug)
    void $ MaybeT $ TRX.statement () (Shows.softDeleteShow showRec.id)
    -- A deleted show must not keep a claim on a time slot. Closing the windows
    -- here means a later restore brings the show back with no schedule, so it
    -- cannot land on a slot another show has taken since. Upcoming episodes are
    -- detached in the same statement, so none is left on a closed window where
    -- the playout query cannot see it.
    detached <- lift $ TRX.statement () (Episodes.closeSchedulesAndDetachEpisodes showRec.id today)
    pure (showRec.id, showRec.title, length detached)

  case result of
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just (showId, showTitle, detachedCount)) -> do
      Log.logInfo
        "Show soft deleted successfully"
        (Aeson.object ["showId" .= display showId, "detachedEpisodes" .= detachedCount])
      pure (showId, showTitle)
