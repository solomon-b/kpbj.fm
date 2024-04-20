module API.User where

--------------------------------------------------------------------------------

import API.User.Current qualified as Current
import API.User.Login (Login)
import API.User.Login qualified as Login
import API.User.Register
import API.User.Register qualified as Register
import Auth qualified
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Database.Queries.User qualified as User
import Database.Tables.User qualified as User
import Database.Utils
import Domain.Types.User
import Hasql.Pool qualified as HSQL
import Log qualified
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
    Has HSQL.Pool env,
    Has SAS.JWTSettings env,
    MonadError Servant.ServerError m,
    Log.MonadLog m,
    MonadIO m
  ) =>
  Servant.ServerT UserAPI m
userHandler = usersHandler :<|> userProfileHandler :<|> Current.handler :<|> Register.handler :<|> Login.handler

usersHandler :: (MonadReader env m, Has HSQL.Pool env, MonadError Servant.ServerError m, Log.MonadLog m, MonadIO m) => m [User]
usersHandler = fmap parseModel <$> User.getUsers

userProfileHandler :: (MonadReader env m, Has HSQL.Pool env, MonadError Servant.ServerError m, Log.MonadLog m, MonadIO m) => User.Id -> m User
userProfileHandler uid =
  User.getUser uid >>= \case
    Nothing -> Servant.throwError Servant.err403
    Just user -> pure $ parseModel user
