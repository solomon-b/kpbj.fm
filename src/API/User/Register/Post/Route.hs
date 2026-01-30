module API.User.Register.Post.Route where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Password.Argon2 (Argon2, Password, PasswordHash, mkPassword)
import Data.Text (Text)
import Data.Text.Display (Display)
import Data.Text.Display.Generic (RecordInstance (..))
import Deriving.Aeson qualified as Deriving
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import GHC.Generics (Generic)
import OrphanInstances.OneRow ()
import OrphanInstances.Password ()
import OrphanInstances.Servant ()
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded qualified as FormUrlEncoded

--------------------------------------------------------------------------------

data Register = Register
  { urEmail :: EmailAddress,
    urPassword :: Password,
    urDisplayName :: DisplayName,
    urFullName :: FullName,
    urNewsletter :: Maybe Bool
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance Register)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "ur", Deriving.CamelToSnake]] Register

instance FormUrlEncoded.FromForm Register where
  fromForm f =
    Register
      <$> FormUrlEncoded.parseUnique "email" f
      <*> fmap mkPassword (FormUrlEncoded.parseUnique "password" f)
      <*> FormUrlEncoded.parseUnique "displayName" f
      <*> FormUrlEncoded.parseUnique "fullName" f
      <*> parseCheckbox (FormUrlEncoded.parseMaybe @Text "newsletter" f)

parseCheckbox :: Either Text (Maybe Text) -> Either Text (Maybe Bool)
parseCheckbox = \case
  Left _ -> Right Nothing
  Right (Just _) -> Right (Just True)
  Right Nothing -> Right Nothing

data RegisterParsed = RegisterParsed
  { urpEmail :: EmailAddress,
    urpPassword :: PasswordHash Argon2,
    urpDisplayName :: DisplayName,
    urpFullName :: FullName
  }

--------------------------------------------------------------------------------

-- | "POST /user/register"
type Route =
  "user"
    :> "register"
    :> Servant.RemoteHost
    :> Servant.Header "User-Agent" Text
    :> Servant.ReqBody '[Servant.FormUrlEncoded] Register
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "Set-Cookie" Text, Servant.Header "HX-Redirect" Text] Servant.NoContent)
