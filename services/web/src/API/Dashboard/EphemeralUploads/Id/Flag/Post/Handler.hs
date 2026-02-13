{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.EphemeralUploads.Id.Flag.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Get.Templates.Page (renderEphemeralUploadRow)
import API.Dashboard.EphemeralUploads.Id.Flag.Post.Route (FlagForm (..))
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleBannerErrors, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (getter)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Class (runDBTransaction)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.User qualified as User
import Hasql.Pool qualified as HSQL
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  EphemeralUploads.Id ->
  Maybe Cookie ->
  FlagForm ->
  AppM (Lucid.Html ())
handler targetId cookie FlagForm {..} =
  handleBannerErrors "Ephemeral upload flag" $ do
    -- 1. Require staff authentication
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can flag ephemeral uploads." userMetadata

    -- 2. Parse and validate reason
    case EphemeralUploads.parseFlagReason ffReason of
      Nothing ->
        throwValidationError "Invalid flag reason."
      Just reason -> do
        -- 3. Execute the flag operation
        Log.logInfo "Flagging ephemeral upload" (Aeson.object ["targetId" .= display targetId, "reason" .= display reason])
        backend <- asks getter
        result <- executeFlag targetId (User.mId user) reason
        renderFlagResult backend result

--------------------------------------------------------------------------------

-- | Result of attempting to flag an ephemeral upload.
data FlagResult
  = FlagSuccess EphemeralUploads.EphemeralUploadWithCreator
  | TargetNotFound EphemeralUploads.Id
  | FlagFailed HSQL.UsageError

-- | Execute flag operation with database transaction.
executeFlag ::
  EphemeralUploads.Id ->
  User.Id ->
  EphemeralUploads.FlagReason ->
  AppM FlagResult
executeFlag targetId flaggerId reason = do
  result <- runDBTransaction $ runMaybeT $ do
    _ <- MaybeT $ TRX.statement () (EphemeralUploads.flagEphemeralUpload targetId flaggerId reason)
    MaybeT $ TRX.statement () (EphemeralUploads.getEphemeralUploadWithCreatorById targetId)

  pure $ case result of
    Left err ->
      FlagFailed err
    Right Nothing ->
      TargetNotFound targetId
    Right (Just updated) ->
      FlagSuccess updated

-- | Render the appropriate HTML response based on flag result.
renderFlagResult :: (Log.MonadLog m) => StorageBackend -> FlagResult -> m (Lucid.Html ())
renderFlagResult backend = \case
  FlagSuccess updated -> do
    Log.logInfo "Ephemeral upload flagged" (Aeson.object ["id" .= display updated.euwcId, "reason" .= show updated.euwcFlagReason])
    pure $ do
      renderEphemeralUploadRow backend True updated
      renderBanner Warning "Upload Flagged" (display updated.euwcTitle <> " has been flagged and hidden from regular users.")
  TargetNotFound uid -> do
    Log.logInfo "Ephemeral upload not found during flag" (Aeson.object ["id" .= display uid])
    pure $ renderBanner Error "Flag Failed" "Ephemeral upload not found."
  FlagFailed err -> do
    Log.logInfo "Database error during flag" (Aeson.object ["error" .= show err])
    pure $ renderBanner Error "Flag Failed" "Failed to flag ephemeral upload. Please try again."
