module API.User.ResetPassword.Post.Route where

--------------------------------------------------------------------------------

import Data.Password.Argon2 (Password, mkPassword)
import Data.Text (Text)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.PasswordResetTokens (Token (..))
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded qualified as FormUrlEncoded

--------------------------------------------------------------------------------

-- | Form data for password reset submission
data ResetPasswordForm = ResetPasswordForm
  { rpfToken :: Token,
    rpfPassword :: Password,
    rpfPasswordConfirm :: Password
  }
  deriving stock (Generic, Show)

instance FormUrlEncoded.FromForm ResetPasswordForm where
  fromForm f =
    ResetPasswordForm
      <$> fmap Token (FormUrlEncoded.parseUnique "token" f)
      <*> fmap mkPassword (FormUrlEncoded.parseUnique "password" f)
      <*> fmap mkPassword (FormUrlEncoded.parseUnique "passwordConfirm" f)

--------------------------------------------------------------------------------

-- | "POST /user/reset-password"
type Route =
  "user"
    :> "reset-password"
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.ReqBody '[Servant.FormUrlEncoded] ResetPasswordForm
    :> Servant.Post
         '[HTML]
         ( Servant.Headers
             '[ Servant.Header "HX-Redirect" Text
              ]
             (Lucid.Html ())
         )
