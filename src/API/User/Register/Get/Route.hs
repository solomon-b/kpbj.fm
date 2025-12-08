module API.User.Register.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.FullName (FullName)
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /user/register"
    ( "user"
        :> "register"
        :> Servant.Header "HX-Request" Text
        :> Servant.QueryParam "emailAddress" EmailAddress
        :> Servant.QueryParam "displayName" DisplayName
        :> Servant.QueryParam "fullName" FullName
        :> Servant.Get '[HTML] (Lucid.Html ())
    )
