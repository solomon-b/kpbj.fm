-- | Route definition for inbound Mailchimp webhook deliveries.
module API.Webhooks.Mailchimp.Post.Route where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.Text (Text)
import Mailchimp.Servant.FormBody (FormBody)
import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | @POST /api/webhooks/mailchimp?key=<secret>@
--
-- Mailchimp posts events as @application/x-www-form-urlencoded@ with
-- bracket-indexed keys (for example @data[email]@). The standard Servant
-- form decoder cannot parse that nesting, so we capture the raw bytes via
-- 'FormBody' and parse them with "Mailchimp.Webhook" inside the handler.
--
-- Authentication is the @?key=<secret>@ query parameter; Mailchimp does not
-- sign webhook bodies.
type Route =
  "api"
    :> "webhooks"
    :> "mailchimp"
    :> Servant.QueryParam "key" Text
    :> Servant.ReqBody '[FormBody] ByteString
    :> Servant.PostNoContent
