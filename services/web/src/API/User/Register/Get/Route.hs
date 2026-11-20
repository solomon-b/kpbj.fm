module API.User.Register.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.FullName (FullName)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /user/register"
--
-- The @emailAddress@ parameter fills the form again after a registration that
-- failed. It is the text that the user typed, so it can be an invalid address.
type Route =
  "user"
    :> "register"
    :> Servant.Header "HX-Request" Text
    :> Servant.QueryParam "emailAddress" Text
    :> Servant.QueryParam "displayName" DisplayName
    :> Servant.QueryParam "fullName" FullName
    :> Servant.Get '[HTML] (Lucid.Html ())
