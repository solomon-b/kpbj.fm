module API.User.Register where

--------------------------------------------------------------------------------

import Auth qualified
import Control.Monad (unless)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Has (Has)
import Data.Text.Encoding qualified as Text.Encoding
import Database.Class (MonadDB)
import Database.Queries.User
import Database.Utils
import Deriving.Aeson qualified as Deriving
import Domain.Types.AdminStatus
import Domain.Types.DisplayName
import Domain.Types.Email
import Domain.Types.Password
import Domain.Types.User
import GHC.Generics (Generic)
import Log qualified
import Servant qualified
import Servant.Auth.Server qualified as SAS
import Text.Email.Validate qualified as Email

--------------------------------------------------------------------------------

data Register = Register
  { urEmail :: EmailAddress,
    urPassword :: Password,
    urDisplayName :: DisplayName
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "ur", Deriving.CamelToSnake]] Register

handler ::
  ( MonadReader env m,
    Has SAS.JWTSettings env,
    MonadError Servant.ServerError m,
    Log.MonadLog m,
    MonadDB m
  ) =>
  Register ->
  m Auth.JWTToken
handler Register {..} = do
  unless (Email.isValid $ Text.Encoding.encodeUtf8 $ coerce urEmail) $ Servant.throwError $ Servant.err401 {Servant.errBody = "Invalid Email Address"}
  execQuerySpanThrowMessage "Failed to query users table" (selectUserByEmailQuery urEmail) >>= \case
    Just _ -> do
      Log.logInfo "Email address is already registered" urEmail
      Servant.throwError $ Servant.err401 {Servant.errBody = "Email address is already registered"}
    Nothing -> do
      Log.logInfo "Registering New User" urEmail
      uid <- insertUser (urEmail, urPassword, urDisplayName, IsNotAdmin)
      execQuerySpanThrowMessage "Failed to query users table" (selectUserQuery uid) >>= \case
        Nothing ->
          Servant.throwError Servant.err500
        Just user ->
          Auth.generateJWTToken $ parseModel @_ @User user
