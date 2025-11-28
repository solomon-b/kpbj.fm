{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Admin.Users.Suspend.Post where

--------------------------------------------------------------------------------

import API.Admin.Users.Get.Templates.Page (renderUserRow)
import App.Common (AuthorizationCheck (..), checkAdminAuthorization, getUserInfo)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.String.Interpolate (i)
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
import Lucid.Base qualified as LucidBase
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
      renderSuccessBanner (display updatedUser.uwmEmail)
  TargetUserNotFound uid -> do
    Log.logInfo "User not found during suspend" (Aeson.object ["userId" .= display uid])
    pure $ renderErrorBanner "User not found or already suspended."
  UserAlreadySuspended uid -> do
    Log.logInfo "User already suspended" (Aeson.object ["userId" .= display uid])
    pure $ renderErrorBanner "User is already suspended."
  SuspendFailed err -> do
    Log.logInfo "Database error during suspension" (Aeson.object ["error" .= show err])
    pure $ renderErrorBanner "Failed to suspend user. Please try again."

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
      pure $ renderErrorBanner "Unauthorized"
    Authorized -> do
      Log.logInfo "Suspending user" (Aeson.object ["targetUserId" .= display targetUserId, "reason" .= sfReason])
      result <- executeSuspension targetUserId sfReason
      renderSuspendResult result

--------------------------------------------------------------------------------
-- Template Helpers

renderSuccessBanner :: Text -> Lucid.Html ()
renderSuccessBanner email =
  Lucid.div_
    [ Lucid.id_ "success-banner-container",
      LucidBase.makeAttributes "hx-swap-oob" "true"
    ]
    $ do
      Lucid.div_
        [ Lucid.id_ "success-banner",
          Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-4 mb-6 w-full"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
            Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
              Lucid.span_ [Lucid.class_ "text-2xl"] [i||]
              Lucid.div_ $ do
                Lucid.h3_ [Lucid.class_ "font-bold text-yellow-800"] "User Suspended"
                Lucid.p_ [Lucid.class_ "text-sm text-yellow-700"] $ do
                  Lucid.toHtml email
                  " has been suspended. They will see a warning banner and cannot perform host actions."
            Lucid.button_
              [ Lucid.onclick_ "this.closest('#success-banner').remove()",
                Lucid.class_ "text-yellow-600 hover:text-yellow-800 font-bold text-xl"
              ]
              [i||]

renderErrorBanner :: Text -> Lucid.Html ()
renderErrorBanner errorMsg =
  Lucid.div_
    [ Lucid.id_ "success-banner-container",
      LucidBase.makeAttributes "hx-swap-oob" "true"
    ]
    $ do
      Lucid.div_
        [ Lucid.id_ "success-banner",
          Lucid.class_ "bg-red-100 border-2 border-red-600 p-4 mb-6 w-full"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
            Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
              Lucid.span_ [Lucid.class_ "text-2xl"] [i||]
              Lucid.div_ $ do
                Lucid.h3_ [Lucid.class_ "font-bold text-red-800"] "Suspend Failed"
                Lucid.p_ [Lucid.class_ "text-sm text-red-700"] $ Lucid.toHtml errorMsg
            Lucid.button_
              [ Lucid.onclick_ "this.closest('#success-banner').remove()",
                Lucid.class_ "text-red-600 hover:text-red-800 font-bold text-xl"
              ]
              [i||]
