{-# LANGUAGE OverloadedRecordDot #-}

module API.Admin.Users.Delete where

--------------------------------------------------------------------------------

import API.Admin.Users.Delete.Templates.Success (renderSuccessBanner)
import App.Common (AuthorizationCheck (..), checkAdminAuthorization, getUserInfo)
import Control.Monad (void)
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
import Domain.Types.EmailAddress (EmailAddress)
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
    "DELETE /admin/users/:id"
    ( "admin"
        :> "users"
        :> Servant.Capture "id" User.Id
        :> Servant.Header "Cookie" Cookie
        :> Servant.Delete '[HTML] (Lucid.Html ())
    )

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
    pure $ renderSuccessBanner (display email)
  TargetUserNotFound uid -> do
    Log.logInfo "User already deleted or not found during delete" (Aeson.object ["userId" .= display uid])
    pure $ renderSimpleErrorBanner "User not found."
  DeleteFailed err -> do
    Log.logInfo "Database error" (Aeson.object ["error" .= show err])
    pure $ renderSimpleErrorBanner "Failed to delete user. Please try again."

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
      Log.logInfo_ "Delete failed: Unauthorized"
      pure $ renderSimpleErrorBanner "Unauthorized"
    Authorized -> do
      result <- executeUserDeletion targetUserId
      renderDeleteResult result

-- Helper for error banners
renderSimpleErrorBanner :: Text -> Lucid.Html ()
renderSimpleErrorBanner errorMsg =
  Lucid.div_
    [ Lucid.id_ "error-banner-container",
      LucidBase.makeAttributes "hx-swap-oob" "true"
    ]
    $ do
      Lucid.div_
        [ Lucid.id_ "error-banner",
          Lucid.class_ "bg-red-100 border-2 border-red-600 p-4 mb-6 w-full"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
            Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
              Lucid.span_ [Lucid.class_ "text-2xl"] "⚠️"
              Lucid.div_ $ do
                Lucid.h3_ [Lucid.class_ "font-bold text-red-800"] "Delete Failed"
                Lucid.p_ [Lucid.class_ "text-sm text-red-700"] $ Lucid.toHtml errorMsg
            Lucid.button_
              [ Lucid.onclick_ "this.closest('#error-banner').remove()",
                Lucid.class_ "text-red-600 hover:text-red-800 font-bold text-xl"
              ]
              "✕"
