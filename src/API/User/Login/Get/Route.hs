module API.User.Login.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /user/login"
    ( "user"
        :> "login"
        :> Servant.Header "HX-Current-Url" Text
        :> Servant.Header "HX-Request" Text
        :> Servant.QueryParam "redirect" Text
        :> Servant.QueryParam "email" EmailAddress
        :> Servant.Get '[HTML] (Lucid.Html ())
    )
