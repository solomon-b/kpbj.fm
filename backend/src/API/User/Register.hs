module API.User.Register where

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
import Effects.User qualified as User
import GHC.Generics (Generic)
import Hasql.Pool qualified as HSQL
import Log qualified
import Servant qualified
import Servant.Auth.Server qualified as SAS
import Text.Email.Validate qualified as Email

--------------------------------------------------------------------------------

data Register = Register
  { urEmail :: User.EmailAddress,
    urPassword :: User.Password,
    urDisplayName :: User.DisplayName
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "ur", Deriving.CamelToSnake]] Register

handler ::
  ( MonadReader env m,
    Has HSQL.Pool env,
    Has SAS.JWTSettings env,
    MonadError Servant.ServerError m,
    Log.MonadLog m,
    MonadIO m
  ) =>
  Register ->
  m Effects.Auth.JWTToken
handler Register {..} = do
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
