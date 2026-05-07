module API.Dashboard.NewsletterSubscribers.Bulk.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import GHC.Generics (Generic)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

-- | Form data for bulk-adding newsletter subscribers.
newtype BulkForm = BulkForm
  { bfEmails :: Text
  }
  deriving stock (Generic, Show)

instance FromForm BulkForm where
  fromForm form = BulkForm <$> Form.parseUnique @Text "emails" form

--------------------------------------------------------------------------------

-- | "POST /dashboard/newsletter-subscribers/bulk"
type Route =
  "dashboard"
    :> "newsletter-subscribers"
    :> "bulk"
    :> Servant.Header "Cookie" Cookie
    :> Servant.ReqBody '[Servant.FormUrlEncoded] BulkForm
    :> Servant.Post
         '[HTML]
         (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
