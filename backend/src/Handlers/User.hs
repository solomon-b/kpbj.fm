module Handlers.User where

--------------------------------------------------------------------------------

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Effects.User (User)
import Effects.User qualified as User
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

--------------------------------------------------------------------------------
-- Handler

userHandler :: (MonadReader env m, Has HSQL.Pool env, MonadError Servant.ServerError m, Log.MonadLog m, MonadIO m) => Servant.ServerT UserAPI m
userHandler = usersHandler :<|> userProfileHandler :<|> currentUserHandler

usersHandler :: (MonadReader env m, Has HSQL.Pool env, MonadError Servant.ServerError m, Log.MonadLog m, MonadIO m) => m [User]
usersHandler = User.getUsers

userProfileHandler :: (MonadReader env m, Has HSQL.Pool env, MonadError Servant.ServerError m, Log.MonadLog m, MonadIO m) => User.Id -> m User
userProfileHandler = User.getUser

currentUserHandler :: (MonadReader env m, Has HSQL.Pool env, MonadError Servant.ServerError m, Log.MonadLog m, MonadIO m) => SAS.AuthResult User -> m User
currentUserHandler (SAS.Authenticated user) = pure user
currentUserHandler _ = Servant.throwError Servant.err401