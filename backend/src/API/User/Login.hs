module API.User.Login where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Has (Has)
import Data.Text.Encoding qualified as Text.Encoding
import Database.Class (MonadDB)
import Database.Queries.User
import Database.Utils
import Deriving.Aeson qualified as Deriving
import Domain.Types.Email
import Domain.Types.Password
import Domain.Types.User (User)
import Errors (throw401, throw403)
import GHC.Generics (Generic)
import Log qualified
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
    Has Servant.Auth.Server.JWTSettings env,
    Log.MonadLog m,
    MonadDB m,
    MonadThrow m
  ) =>
  Login ->
  m Auth.JWTToken
handler Login {..} = do
  unless (Email.isValid $ Text.Encoding.encodeUtf8 $ coerce ulEmail) $ throw403 "Invalid Email Address"
  execQuerySpanThrowMessage "Failed to query users table" (selectUserByCredentialQuery ulEmail ulPassword) >>= \case
    Just user -> do
      Log.logInfo "Login Attempt" ulEmail
      Auth.generateJWTToken $ parseModel @_ @User user
    Nothing -> do
      Log.logInfo "Invalid Credentials" ulEmail
      throw401 "Invalid Credentials."
