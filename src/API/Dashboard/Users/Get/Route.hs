module API.Dashboard.Users.Get.Route where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.Filter (Filter)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.UserSortBy (UserSortBy)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /dashboard/users"
    ( "dashboard"
        :> "users"
        :> Servant.QueryParam "page" Int64
        :> Servant.QueryParam "q" (Filter Text)
        :> Servant.QueryParam "role" (Filter UserMetadata.UserRole)
        :> Servant.QueryParam "sort" (Filter UserSortBy)
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )
