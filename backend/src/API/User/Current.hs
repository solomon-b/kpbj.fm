module API.User.Current where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text.Display (display)
import Domain.Types.User (User)
import Errors (throw401')
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant.Auth.Server qualified as SAS
import Tracing (handlerSpan)

--------------------------------------------------------------------------------

handler ::
  ( Log.MonadLog m,
    MonadThrow m,
    MonadReader env m,
    Has OTEL.Tracer env,
    MonadCatch m,
    MonadUnliftIO m
  ) =>
  SAS.AuthResult User ->
  m User
handler (SAS.Authenticated user) =
  handlerSpan "/user/register" () display $ pure user
handler _ = throw401'
