module API.Newsletter.Subscribe.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Domain.Types.EmailAddress (EmailAddress, mkEmailAddress)
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

-- | Form submitted by the homepage newsletter signup.
newtype SubscribeForm = SubscribeForm
  { sfEmail :: EmailAddress
  }
  deriving stock (Generic, Show)

instance FromForm SubscribeForm where
  fromForm :: Form.Form -> Either Text SubscribeForm
  fromForm form = do
    raw <- Form.parseUnique @Text "email" form
    pure SubscribeForm {sfEmail = mkEmailAddress raw}

--------------------------------------------------------------------------------

-- | "POST /api/newsletter/subscribe"
type Route =
  "api"
    :> "newsletter"
    :> "subscribe"
    :> Servant.ReqBody '[Servant.FormUrlEncoded] SubscribeForm
    :> Servant.Post '[HTML] (Lucid.Html ())
