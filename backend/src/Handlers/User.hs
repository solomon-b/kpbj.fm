module Handlers.User where

--------------------------------------------------------------------------------

import Control.Monad (unless)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Has (Has)
import Data.Text.Encoding qualified as Text.Encoding
import Deriving.Aeson qualified as Deriving
import Effects.Auth qualified
import Effects.User (User (..))
import Effects.User qualified as User
import GHC.Generics (Generic)
import Hasql.Pool qualified as HSQL
import Log qualified
import Servant ((:<|>) (..), (:>))
import Servant qualified
import Servant.Auth (Auth)
import Servant.Auth qualified
import Servant.Auth.Server qualified as SAS
import Text.Email.Validate qualified as Email

--------------------------------------------------------------------------------
-- Route

type UserAPI =
  Servant.Get '[Servant.JSON] [User]
    :<|> Servant.Capture "id" User.Id :> Servant.Get '[Servant.JSON] User
    :<|> Auth '[Servant.Auth.JWT, Servant.Auth.BasicAuth] User :> "current" :> Servant.Get '[Servant.JSON] User
    :<|> "register" :> Servant.ReqBody '[Servant.JSON] UserRegistration :> Servant.Post '[Servant.JSON] Effects.Auth.JWTToken
    :<|> "login" :> Servant.ReqBody '[Servant.JSON] UserLogin :> Servant.Post '[Servant.JSON] Effects.Auth.JWTToken

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
userHandler = usersHandler :<|> userProfileHandler :<|> currentUserHandler :<|> registerHandler :<|> loginHandler

usersHandler :: (MonadReader env m, Has HSQL.Pool env, MonadError Servant.ServerError m, Log.MonadLog m, MonadIO m) => m [User]
usersHandler = User.getUsers

userProfileHandler :: (MonadReader env m, Has HSQL.Pool env, MonadError Servant.ServerError m, Log.MonadLog m, MonadIO m) => User.Id -> m User
userProfileHandler uid =
  User.getUser uid >>= \case
    Nothing -> Servant.throwError Servant.err403
    Just user -> pure user

currentUserHandler :: (MonadReader env m, Has HSQL.Pool env, MonadError Servant.ServerError m, Log.MonadLog m, MonadIO m) => SAS.AuthResult User -> m User
currentUserHandler (SAS.Authenticated user) = pure user
currentUserHandler _ = Servant.throwError Servant.err401

data UserRegistration = UserRegistration
  { urEmail :: User.EmailAddress,
    urPassword :: User.Password,
    urDisplayName :: User.DisplayName
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "ur", Deriving.CamelToSnake]] UserRegistration

registerHandler ::
  ( MonadReader env m,
    Has HSQL.Pool env,
    Has SAS.JWTSettings env,
    MonadError Servant.ServerError m,
    Log.MonadLog m,
    MonadIO m
  ) =>
  UserRegistration ->
  m Effects.Auth.JWTToken
registerHandler UserRegistration {..} = do
  unless (Email.isValid $ Text.Encoding.encodeUtf8 $ coerce urEmail) $ Servant.throwError $ Servant.err401 {Servant.errBody = "Invalid Email Address"}
  User.getUserByEmail urEmail >>= \case
    Just _ -> do
      Log.logInfo "Email address is already registered" urEmail
      Servant.throwError $ Servant.err401 {Servant.errBody = "Email address is already registered"}
    Nothing -> do
      Log.logInfo "Registering New User" urEmail
      uid <- User.insertUser (urEmail, urPassword, urDisplayName, User.IsNotAdmin)
      User.getUser uid >>= \case
        Nothing ->
          Servant.throwError Servant.err500
        Just user ->
          Effects.Auth.generateJWTToken user

data UserLogin = UserLogin
  { ulEmail :: User.EmailAddress,
    ulPassword :: User.Password
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "ul", Deriving.CamelToSnake]] UserLogin

loginHandler ::
  ( MonadReader env m,
    Has HSQL.Pool env,
    Has SAS.JWTSettings env,
    MonadError Servant.ServerError m,
    Log.MonadLog m,
    MonadIO m
  ) =>
  UserLogin ->
  m Effects.Auth.JWTToken
loginHandler UserLogin {..} = do
  unless (Email.isValid $ Text.Encoding.encodeUtf8 $ coerce ulEmail) $ Servant.throwError $ Servant.err403 {Servant.errBody = "Invalid Email Address"}
  User.getUserByCredential ulEmail ulPassword >>= \case
    Just user -> do
      Log.logInfo "Login Attempt" ulEmail
      Effects.Auth.generateJWTToken user
    Nothing -> do
      Log.logInfo "Invalid Credentials" ulEmail
      Servant.throwError $ Servant.err401 {Servant.errBody = "Invalid Credentials."}
