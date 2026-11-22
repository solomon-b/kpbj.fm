module API.User.Register.Get.Route where

--------------------------------------------------------------------------------

import App.Flash (ReadFlash)
import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /user/register"
type Route =
  "user"
    :> "register"
    :> ReadFlash
    :> Servant.Header "HX-Request" Text
    :> Servant.QueryParam "emailAddress" EmailAddress
    :> Servant.QueryParam "displayName" DisplayName
    :> Servant.QueryParam "fullName" FullName
    :> Servant.Get '[HTML] (Lucid.Html ())
