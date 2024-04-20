module API.User.Login where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad (unless)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Has (Has)
import Data.Text.Encoding qualified as Text.Encoding
import Database.Queries.User qualified as User
import Database.Utils
import Deriving.Aeson qualified as Deriving
import Domain.Types.Email
import Domain.Types.Password
import Domain.Types.User (User)
import GHC.Generics (Generic)
import Hasql.Pool qualified as HSQL
import Log qualified
import Servant qualified
import Servant.Auth.Server qualified
import Text.Email.Validate qualified as Email

--------------------------------------------------------------------------------

data Login = Login
  { ulEmail :: EmailAddress,
    ulPassword :: Password
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "ul", Deriving.CamelToSnake]] Login

handler ::
  ( MonadReader env m,
    Has HSQL.Pool env,
    Has Servant.Auth.Server.JWTSettings env,
    MonadError Servant.ServerError m,
    Log.MonadLog m,
    MonadIO m
  ) =>
  Login ->
  m Auth.JWTToken
handler Login {..} = do
  unless (Email.isValid $ Text.Encoding.encodeUtf8 $ coerce ulEmail) $ Servant.throwError $ Servant.err403 {Servant.errBody = "Invalid Email Address"}
  User.getUserByCredential ulEmail ulPassword >>= \case
    Just user -> do
      Log.logInfo "Login Attempt" ulEmail
      Auth.generateJWTToken $ parseModel @_ @User user
    Nothing -> do
      Log.logInfo "Invalid Credentials" ulEmail
      Servant.throwError $ Servant.err401 {Servant.errBody = "Invalid Credentials."}
