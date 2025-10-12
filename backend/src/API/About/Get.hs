{-# LANGUAGE ViewPatterns #-}

module API.About.Get where

--------------------------------------------------------------------------------

import API.About.Get.Templates (template)
import App.Auth qualified as Auth
import App.Common (renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Has (Has)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /about"
    ( "about"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
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
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer (coerce -> cookie) (foldHxReq -> hxRequest) = do
  Auth.userLoginState cookie >>= \case
    Auth.IsNotLoggedIn ->
      renderTemplate hxRequest Nothing template
    Auth.IsLoggedIn user ->
      execQuerySpan (UserMetadata.getUserMetadata (User.mId user)) >>= \case
        Left err -> do
          Log.logAttention "database error" (Aeson.object ["message" .= show err])
          renderTemplate hxRequest Nothing template
        Right Nothing -> do
          Log.logAttention "No user metadata found" ()
          renderTemplate hxRequest Nothing template
        Right (Just userMetadata) ->
          renderTemplate hxRequest (Just userMetadata) template
