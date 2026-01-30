module API.Dashboard.SitePages.Slug.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..), parseMaybe, parseUnique)

--------------------------------------------------------------------------------

-- | "POST /dashboard/site-pages/:slug/edit"
type Route =
  "dashboard"
    :> "site-pages"
    :> Servant.Capture "slug" Text
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.ReqBody '[Servant.FormUrlEncoded] EditForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))

-- | Form data for editing a site page.
data EditForm = EditForm
  { efTitle :: Text,
    efContent :: Text,
    efEditSummary :: Maybe Text
  }
  deriving stock (Show, Eq)

instance FromForm EditForm where
  fromForm form = do
    efTitle <- parseUnique "title" form
    efContent <- parseUnique "content" form
    efEditSummary <- parseMaybe "edit_summary" form
    pure EditForm {..}
