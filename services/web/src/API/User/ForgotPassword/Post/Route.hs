module API.User.ForgotPassword.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.HxRequest (HxRequest)
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as FormUrlEncoded

--------------------------------------------------------------------------------

-- | Form data for forgot password request
newtype ForgotPasswordForm = ForgotPasswordForm
  { fpfEmail :: Text
  }
  deriving stock (Generic, Show)

instance FromForm ForgotPasswordForm where
  fromForm f =
    ForgotPasswordForm
      <$> FormUrlEncoded.parseUnique "email" f

--------------------------------------------------------------------------------

-- | "POST /user/forgot-password"
type Route =
  "user"
    :> "forgot-password"
    :> Servant.RemoteHost
    :> Servant.Header "User-Agent" Text
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.ReqBody '[Servant.FormUrlEncoded] ForgotPasswordForm
    :> Servant.Post '[HTML] (Lucid.Html ())
