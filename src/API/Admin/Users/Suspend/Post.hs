{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Admin.Users.Suspend.Post where

--------------------------------------------------------------------------------

import API.Admin.Users.Get.Templates.Page (renderUserRow)
import App.Common (AuthorizationCheck (..), checkAdminAuthorization, getUserInfo)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Effects.Database.Class (MonadDB, runDBTransaction)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Hasql.Pool qualified as HSQL
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded qualified as FormUrlEncoded

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /admin/users/:id/suspend"
    ( "admin"
        :> "users"
        :> Servant.Capture "id" User.Id
        :> "suspend"
        :> Servant.Header "Cookie" Cookie
        :> Servant.ReqBody '[Servant.FormUrlEncoded] SuspendForm
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Form data for suspending a user
data SuspendForm = SuspendForm
  { sfReason :: Text
  }
  deriving stock (Generic, Show)

instance FormUrlEncoded.FromForm SuspendForm where
  fromForm f =
    SuspendForm
      <$> FormUrlEncoded.parseUnique "reason" f

--------------------------------------------------------------------------------

-- | Result of attempting to suspend a user
data SuspendResult
  = SuspendSuccess UserMetadata.UserWithMetadata
  | TargetUserNotFound User.Id
  | UserAlreadySuspended User.Id
  | SuspendFailed HSQL.UsageError

-- | Execute user suspension with database operations
executeSuspension ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m
  ) =>
  User.Id ->
  Text ->
  m SuspendResult
executeSuspension targetUserId reason = do
  result <- runDBTransaction $ runMaybeT $ do
    _ <- MaybeT $ TRX.statement () (UserMetadata.getUserWithMetadataById targetUserId)
    _ <- MaybeT $ TRX.statement () (UserMetadata.suspendUser targetUserId reason)
    -- Fetch the updated user after suspension
    MaybeT $ TRX.statement () (UserMetadata.getUserWithMetadataById targetUserId)

  pure $ case result of
    Left err ->
      SuspendFailed err
    Right Nothing ->
      TargetUserNotFound targetUserId
    Right (Just updatedUser) ->
      SuspendSuccess updatedUser

-- | Render the appropriate HTML response based on suspension result
renderSuspendResult :: (Log.MonadLog m) => SuspendResult -> m (Lucid.Html ())
renderSuspendResult = \case
  SuspendSuccess updatedUser -> do
    Log.logInfo "User suspended successfully" (Aeson.object ["userId" .= display updatedUser.uwmUserId, "email" .= display updatedUser.uwmEmail])
    pure $ do
      -- Return the updated row (will replace the old row)
      renderUserRow updatedUser
      -- Also send an OOB success banner
      renderBanner Warning "User Suspended" (display updatedUser.uwmEmail <> " has been suspended. They will see a warning banner and cannot perform host actions.")
  TargetUserNotFound uid -> do
    Log.logInfo "User not found during suspend" (Aeson.object ["userId" .= display uid])
    pure $ renderBanner Error "Suspend Failed" "User not found or already suspended."
  UserAlreadySuspended uid -> do
    Log.logInfo "User already suspended" (Aeson.object ["userId" .= display uid])
    pure $ renderBanner Error "Suspend Failed" "User is already suspended."
  SuspendFailed err -> do
    Log.logInfo "Database error during suspension" (Aeson.object ["error" .= show err])
    pure $ renderBanner Error "Suspend Failed" "Failed to suspend user. Please try again."

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
  SuspendForm ->
  m (Lucid.Html ())
handler _tracer targetUserId cookie SuspendForm {..} = do
  userInfo <- getUserInfo cookie

  case checkAdminAuthorization userInfo of
    Unauthorized -> do
      Log.logInfo_ "Suspend failed: Unauthorized"
      pure $ renderBanner Error "Suspend Failed" "Unauthorized"
    Authorized -> do
      Log.logInfo "Suspending user" (Aeson.object ["targetUserId" .= display targetUserId, "reason" .= sfReason])
      result <- executeSuspension targetUserId sfReason
      renderSuspendResult result
