{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.EphemeralUploads.Id.Unflag.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Get.Templates.Page (renderEphemeralUploadRow)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (getter)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Class (runDBTransaction)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Hasql.Pool qualified as HSQL
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

-- | All data needed to render the unflag result row + banner (Pattern D).
data UploadUnflagViewData = UploadUnflagViewData
  { uuvBackend :: StorageBackend,
    uuvResult :: UnflagResult
  }

-- | Business logic: execute unflag.
action ::
  EphemeralUploads.Id ->
  ExceptT HandlerError AppM UploadUnflagViewData
action targetId = do
  -- 1. Execute the unflag operation
  Log.logInfo "Unflagging ephemeral upload" (Aeson.object ["targetId" .= display targetId])
  backend <- asks getter
  result <- lift $ executeUnflag targetId

  pure UploadUnflagViewData {uuvBackend = backend, uuvResult = result}

-- | Servant handler: thin glue running action and rendering row + banner.
handler ::
  EphemeralUploads.Id ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler targetId cookie =
  handleBannerErrors "Ephemeral upload unflag" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can unflag ephemeral uploads." userMetadata
    vd <- action targetId
    lift $ renderUnflagResult vd.uuvBackend vd.uuvResult

--------------------------------------------------------------------------------

-- | Result of attempting to unflag an ephemeral upload.
data UnflagResult
  = UnflagSuccess EphemeralUploads.EphemeralUploadWithCreator
  | TargetNotFound EphemeralUploads.Id
  | UnflagFailed HSQL.UsageError

-- | Execute unflag operation with database transaction.
executeUnflag ::
  EphemeralUploads.Id ->
  AppM UnflagResult
executeUnflag targetId = do
  result <- runDBTransaction $ runMaybeT $ do
    _ <- MaybeT $ TRX.statement () (EphemeralUploads.unflagEphemeralUpload targetId)
    MaybeT $ TRX.statement () (EphemeralUploads.getEphemeralUploadWithCreatorById targetId)

  pure $ case result of
    Left err ->
      UnflagFailed err
    Right Nothing ->
      TargetNotFound targetId
    Right (Just updated) ->
      UnflagSuccess updated

-- | Render the appropriate HTML response based on unflag result.
renderUnflagResult :: (Log.MonadLog m) => StorageBackend -> UnflagResult -> m (Lucid.Html ())
renderUnflagResult backend = \case
  UnflagSuccess updated -> do
    Log.logInfo "Ephemeral upload unflagged" (Aeson.object ["id" .= display updated.euwcId])
    pure $ do
      renderEphemeralUploadRow backend True updated
      renderBanner Success "Upload Unflagged" (display updated.euwcTitle <> " has been unflagged and is now visible to all users.")
  TargetNotFound uid -> do
    Log.logInfo "Ephemeral upload not found during unflag" (Aeson.object ["id" .= display uid])
    pure $ renderBanner Error "Unflag Failed" "Ephemeral upload not found."
  UnflagFailed err -> do
    Log.logInfo "Database error during unflag" (Aeson.object ["error" .= show err])
    pure $ renderBanner Error "Unflag Failed" "Failed to unflag ephemeral upload. Please try again."
