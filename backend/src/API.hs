module API where

--------------------------------------------------------------------------------

import API.MailingList
import API.SplashPage
import API.User
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Database.Class (MonadDB)
import Hasql.Pool qualified as HSQL
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant ((:<|>) (..), (:>))
import Servant qualified
import Servant.Auth.Server qualified as Servant.Auth

--------------------------------------------------------------------------------

type API =
  SplashPageAPI
    :<|> "mailing-list" :> MailingListAPI
    :<|> "user" :> UserAPI

server ::
  ( MonadReader env m,
    Has HSQL.Pool env,
    Has Servant.Auth.JWTSettings env,
    Has OTEL.Tracer env,
    Log.MonadLog m,
    MonadDB m,
    MonadIO m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Servant.ServerT API m
server = splashPageHandler :<|> mailingListHandler :<|> userHandler
