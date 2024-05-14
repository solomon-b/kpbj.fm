module API.User where

--------------------------------------------------------------------------------

import API.User.Current qualified as Current
import API.User.Login (Login)
import API.User.Login qualified as Login
import API.User.Register
import API.User.Register qualified as Register
import Auth qualified
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Domain.Types.User
import Effects.Database.Class (MonadDB)
import Effects.Database.Queries.User
import Effects.Database.Tables.User qualified as User
import Effects.Database.Utils
import Errors (throw403')
import Log qualified
import OpenTelemetry.Trace qualified as OTEL
import Servant ((:<|>) (..), (:>))
import Servant qualified
import Servant.Auth (Auth)
import Servant.Auth qualified
import Servant.Auth.Server qualified as SAS

--------------------------------------------------------------------------------
-- Route

type UserAPI =
  Servant.Get '[Servant.JSON] [User]
    :<|> Servant.Capture "id" User.Id :> Servant.Get '[Servant.JSON] User
    :<|> Auth '[Servant.Auth.JWT, Servant.Auth.BasicAuth] User :> "current" :> Servant.Get '[Servant.JSON] User
    :<|> "register" :> Servant.ReqBody '[Servant.JSON] Register :> Servant.Post '[Servant.JSON] Auth.JWTToken
    :<|> "login" :> Servant.ReqBody '[Servant.JSON] Login :> Servant.Post '[Servant.JSON] Auth.JWTToken

--------------------------------------------------------------------------------
-- Handler

userHandler ::
  ( MonadReader env m,
    Has SAS.JWTSettings env,
    Has OTEL.Tracer env,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Servant.ServerT UserAPI m
userHandler = usersHandler :<|> userProfileHandler :<|> Current.handler :<|> Register.handler :<|> Login.handler

usersHandler ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  m [User]
usersHandler = fmap parseModel <$> execQuerySpanThrowMessage "Failed to query users table" selectUsersQuery

userProfileHandler ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  User.Id ->
  m User
userProfileHandler uid =
  execQuerySpanThrowMessage "Failed to query users table" (selectUserQuery uid) >>= \case
    Nothing -> throw403'
    Just user -> pure $ parseModel user
