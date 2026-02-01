module API.Dashboard.StreamSettings.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..), parseUnique)

--------------------------------------------------------------------------------

-- | "POST /dashboard/stream-settings/edit"
type Route =
  "dashboard"
    :> "stream-settings"
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> Servant.Header "HX-Request" HxRequest
    :> Servant.ReqBody '[Servant.FormUrlEncoded] EditForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))

-- | Form data for editing stream settings.
data EditForm = EditForm
  { efStreamUrl :: Text,
    efMetadataUrl :: Text
  }
  deriving stock (Show, Eq)

instance FromForm EditForm where
  fromForm form = do
    efStreamUrl <- parseUnique "stream_url" form
    efMetadataUrl <- parseUnique "metadata_url" form
    pure EditForm {..}
