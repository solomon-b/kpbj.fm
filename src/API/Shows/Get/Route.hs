module API.Shows.Get.Route where

--------------------------------------------------------------------------------

import Domain.Types.Cookie (Cookie)
import Domain.Types.Filter (Filter)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.PageNumber (PageNumber)
import Domain.Types.Search (Search)
import Domain.Types.ShowSortBy (ShowSortBy)
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Observability qualified as Observability
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows"
    ( "shows"
        :> Servant.QueryParam "page" PageNumber
        :> Servant.QueryParam "tag" (Filter ShowTags.Id)
        :> Servant.QueryParam "status" (Filter Shows.Status)
        :> Servant.QueryParam "search" (Filter Search)
        :> Servant.QueryParam "sortBy" (Filter ShowSortBy)
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )
