module API.Dashboard.Underwriting.Plan.Rebuild.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (dashboardUnderwritingLinks, rootLink)
import API.Types (DashboardUnderwritingRoutes (..))
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Effects.BreakPlan qualified as BreakPlan
import Effects.Clock (currentSystemTime)
import Servant qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Handler for POST /dashboard/underwriting/plan/rebuild.
--
-- Rebuilds today's plan for the breaks still to come. Past breaks keep their
-- plan, so the delivery record stays true to what aired.
handler ::
  Maybe Cookie ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler cookie =
  handleRedirectErrors "Break plan rebuild" planLink $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to manage underwriting." userMetadata
    now <- lift currentSystemTime
    fromRightM throwDatabaseError $ lift $ BreakPlan.rebuildToday now
    let flash = FlashMessage Success "Plan Rebuilt" "Breaks from now until midnight have a new plan."
    pure $
      Servant.addHeader (rootLink planLink) $
        Servant.addHeader (flashCookie (Just flash)) Servant.NoContent
  where
    planLink = dashboardUnderwritingLinks.plan Nothing
