{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.AirBreaks.Plan.Get.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardTimelineLink)
import App.Monad (AppM)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/air-breaks/plan.
--
-- Redirects to the day timeline.
handler ::
  Maybe Text ->
  AppM (Servant.Headers '[Servant.Header "Location" Text] Servant.NoContent)
handler mDay =
  pure $ Servant.addHeader [i|/#{Links.linkURI (dashboardTimelineLink mDay)}|] Servant.NoContent
