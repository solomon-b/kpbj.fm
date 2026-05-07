-- | Handler for @GET /api/webhooks/mailchimp@.
--
-- Mailchimp validates a webhook URL on registration; returning a 200 here
-- (with no body) is enough for that probe to succeed.
module API.Webhooks.Mailchimp.Get.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import App.Monad (AppM)
import Log qualified
import Servant qualified

--------------------------------------------------------------------------------

-- | Acknowledge Mailchimp's URL-validation probe.
handler :: AppM Servant.NoContent
handler = do
  Log.logInfo_ "Mailchimp webhook validation probe received"
  pure Servant.NoContent
