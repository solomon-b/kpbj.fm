module API.Dashboard.Profile.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Multipart (Mem, MultipartData, MultipartForm)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "POST /dashboard/profile/edit"
type Route =
    "dashboard"
      :> "profile"
      :> "edit"
      :> Servant.Header "Cookie" Cookie
      :> Servant.Header "HX-Request" HxRequest
      :> MultipartForm Mem (MultipartData Mem)
      :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
