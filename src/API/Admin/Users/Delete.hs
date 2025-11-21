{-# LANGUAGE OverloadedRecordDot #-}

module API.Admin.Users.Delete where

--------------------------------------------------------------------------------

import App.Common (getUserInfo)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
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
handler _tracer _targetUserId cookie = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo_ "Delete failed: No user session"
      pure $ renderSimpleErrorBanner "You must be logged in to delete users."
    Just (_user, userMeta) ->
      if not (UserMetadata.isAdmin userMeta.mUserRole)
        then do
          Log.logInfo_ "Delete failed: Not admin"
          pure $ renderSimpleErrorBanner "You must be an admin to delete users."
        else do
          -- TODO: Implement user deletion
          -- This requires careful consideration of:
          -- 1. Cascade deletes (episodes, blog posts, show hosts, etc.)
          -- 2. Soft delete vs hard delete
          -- 3. Data retention policies
          -- 4. Audit logging
          --
          -- For now, return a "not implemented" message
          Log.logInfo "User deletion not yet implemented" ()
          pure $ renderSimpleErrorBanner "User deletion feature is not yet implemented. Please contact a system administrator."

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
