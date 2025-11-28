{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Admin.Users.Unsuspend.Post where

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

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /admin/users/:id/unsuspend"
    ( "admin"
        :> "users"
        :> Servant.Capture "id" User.Id
        :> "unsuspend"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

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
renderUnsuspendResult :: (Log.MonadLog m) => UnsuspendResult -> m (Lucid.Html ())
renderUnsuspendResult = \case
  UnsuspendSuccess updatedUser -> do
    Log.logInfo "User unsuspended successfully" (Aeson.object ["userId" .= display updatedUser.uwmUserId, "email" .= display updatedUser.uwmEmail])
    pure $ do
      -- Return the updated row (will replace the old row)
      renderUserRow updatedUser
      -- Also send an OOB success banner
      renderSuccessBanner (display updatedUser.uwmDisplayName)
  TargetUserNotFound uid -> do
    Log.logInfo "User not found during unsuspend" (Aeson.object ["userId" .= display uid])
    pure $ renderErrorBanner "User not found or not suspended."
  UserNotSuspended uid -> do
    Log.logInfo "User not suspended" (Aeson.object ["userId" .= display uid])
    pure $ renderErrorBanner "User is not currently suspended."
  UnsuspendFailed err -> do
    Log.logInfo "Database error during unsuspension" (Aeson.object ["error" .= show err])
    pure $ renderErrorBanner "Failed to unsuspend user. Please try again."

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
      pure $ renderErrorBanner "Unauthorized"
    Authorized -> do
      Log.logInfo "Unsuspending user" (Aeson.object ["targetUserId" .= display targetUserId])
      result <- executeUnsuspension targetUserId
      renderUnsuspendResult result

--------------------------------------------------------------------------------
-- Template Helpers

renderSuccessBanner :: Text -> Lucid.Html ()
renderSuccessBanner displayName =
  Lucid.div_
    [ Lucid.id_ "success-banner-container",
      LucidBase.makeAttributes "hx-swap-oob" "true"
    ]
    $ do
      Lucid.div_
        [ Lucid.id_ "success-banner",
          Lucid.class_ "bg-green-100 border-2 border-green-600 p-4 mb-6 w-full"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
            Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
              Lucid.span_ [Lucid.class_ "text-2xl"] [i||]
              Lucid.div_ $ do
                Lucid.h3_ [Lucid.class_ "font-bold text-green-800"] "User Unsuspended"
                Lucid.p_ [Lucid.class_ "text-sm text-green-700"] $ do
                  Lucid.toHtml displayName
                  "'s suspension has been lifted. They can now use the site normally."
            Lucid.button_
              [ Lucid.onclick_ "this.closest('#success-banner').remove()",
                Lucid.class_ "text-green-600 hover:text-green-800 font-bold text-xl"
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
                Lucid.h3_ [Lucid.class_ "font-bold text-red-800"] "Unsuspend Failed"
                Lucid.p_ [Lucid.class_ "text-sm text-red-700"] $ Lucid.toHtml errorMsg
            Lucid.button_
              [ Lucid.onclick_ "this.closest('#success-banner').remove()",
                Lucid.class_ "text-red-600 hover:text-red-800 font-bold text-xl"
              ]
              [i||]
