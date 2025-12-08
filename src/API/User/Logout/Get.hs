module API.User.Logout.Get where

--------------------------------------------------------------------------------

import App.Auth qualified as Auth
import App.Errors (InternalServerError (..), throwErr)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Class (MonadDB)
import Effects.Database.Tables.ServerSessions qualified as Session
import Hasql.Pool qualified
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant qualified

--------------------------------------------------------------------------------

handler ::
  ( Log.MonadLog m,
    MonadReader env m,
    Has Hasql.Pool.Pool env,
    Has OTEL.Tracer env,
    MonadDB m,
    MonadThrow m,
    MonadCatch m,
    MonadUnliftIO m
  ) =>
  OTEL.Tracer ->
  Auth.Authz ->
  m
    ( Servant.Headers
        '[ Servant.Header "HX-Redirect" Text
         ]
        Servant.NoContent
    )
handler _tracer Auth.Authz {..} = do
  Auth.expireServerSession (Session.dSessionId $ Session.toDomain authzSession) >>= \case
    Left err -> do
      liftIO $ print err
      throwErr $ InternalServerError $ Text.pack $ show err
    Right _ ->
      pure $ Servant.addHeader "/" Servant.NoContent
