module API.User.Login.Post.Route where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (isJust)
import Data.Password.Argon2 (Password, mkPassword)
import Data.Text (Text)
import Data.Text.Display (Display (..), RecordInstance (..))
import Deriving.Aeson qualified as Deriving
import GHC.Generics (Generic)
import OrphanInstances.Password ()
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as FormUrlEncoded

--------------------------------------------------------------------------------

-- | The login form.
--
-- The email is the raw text. The handler parses it, so an invalid address
-- gets the "invalid credentials" answer and not a 400 from the form decoder.
data Login = Login
  { ulEmail :: Text,
    ulPassword :: Password,
    ulRememberMe :: Bool
  }
  deriving stock (Generic)
  deriving (Display) via (RecordInstance Login)
  deriving
    (FromJSON, ToJSON)
    via Deriving.CustomJSON '[Deriving.FieldLabelModifier '[Deriving.StripPrefix "ul", Deriving.CamelToSnake]] Login

instance FormUrlEncoded.FromForm Login where
  fromForm f =
    Login
      <$> FormUrlEncoded.parseUnique "email" f
      <*> fmap mkPassword (FormUrlEncoded.parseUnique "password" f)
      <*> fmap isJust (FormUrlEncoded.parseMaybe @Text "remember" f)

--------------------------------------------------------------------------------

-- | "POST /user/login"
type Route =
  "user"
    :> "login"
    :> Servant.RemoteHost
    :> Servant.Header "User-Agent" Text
    :> Servant.ReqBody '[Servant.FormUrlEncoded] Login
    :> Servant.QueryParam "redirect" Text
    :> Servant.PostAccepted '[HTML] (Servant.Headers '[Servant.Header "Set-Cookie" Text, Servant.Header "HX-Redirect" Text] Servant.NoContent)
