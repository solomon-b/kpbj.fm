{-# LANGUAGE DataKinds #-}

module API.Dashboard.AirBreaks.Plan.Get.Route where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | "GET /dashboard/air-breaks/plan"
--
-- The day timeline replaced the break plan page. This route redirects old
-- links there, with the same @day@ parameter.
type Route =
  "dashboard"
    :> "air-breaks"
    :> "plan"
    :> Servant.QueryParam "day" Text
    :> Servant.Verb 'Servant.GET 301 '[HTML] (Servant.Headers '[Servant.Header "Location" Text] Servant.NoContent)
