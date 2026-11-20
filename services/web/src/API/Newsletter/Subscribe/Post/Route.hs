module API.Newsletter.Subscribe.Post.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

-- | Form submitted by the homepage newsletter signup.
--
-- The email is the raw text. The handler parses it, so an invalid address
-- gets a banner and not a 400 from the form decoder.
newtype SubscribeForm = SubscribeForm
  { sfEmail :: Text
  }
  deriving stock (Generic, Show)

instance FromForm SubscribeForm where
  fromForm :: Form.Form -> Either Text SubscribeForm
  fromForm form = SubscribeForm <$> Form.parseUnique @Text "email" form

--------------------------------------------------------------------------------

-- | "POST /api/newsletter/subscribe"
type Route =
  "api"
    :> "newsletter"
    :> "subscribe"
    :> Servant.ReqBody '[Servant.FormUrlEncoded] SubscribeForm
    :> Servant.Post '[HTML] (Lucid.Html ())
