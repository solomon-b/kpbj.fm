-- | Route definition for the Mailchimp webhook validation probe.
module API.Webhooks.Mailchimp.Get.Route where

--------------------------------------------------------------------------------

import Servant ((:>))
import Servant qualified

--------------------------------------------------------------------------------

-- | @GET /api/webhooks/mailchimp@
--
-- Mailchimp validates a webhook URL by issuing a request against it before
-- accepting the registration. Returning 'Servant.NoContent' satisfies that
-- check without exposing any payload. WAI/Warp will route HEAD requests to
-- this handler automatically.
type Route =
  "api"
    :> "webhooks"
    :> "mailchimp"
    :> Servant.Get '[Servant.JSON] Servant.NoContent
