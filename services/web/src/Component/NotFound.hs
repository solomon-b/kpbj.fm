-- | Not found page content for routing-level 404 responses.
--
-- This template is used by the 404 middleware to render a full HTML page
-- when Servant's router doesn't match any route. It reuses the same
-- design tokens as handler-level not-found errors in "App.Handler.Error".
module Component.NotFound
  ( notFoundPage,
  )
where

--------------------------------------------------------------------------------

import App.CustomContext (StreamConfig)
import App.Handler.Error (notFoundContent)
import Component.Flash (FlashMessage)
import Component.Frame qualified as Frame
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId)
import Lucid qualified

--------------------------------------------------------------------------------

-- | 404 page wrapped in the full site frame, with the flash message of the
-- request.
--
-- It has no user context (logged-out view). Used by 'Middleware.NotFound' to
-- replace Servant's plain-text 404 responses.
notFoundPage :: Maybe GoogleAnalyticsId -> StreamConfig -> Maybe FlashMessage -> Lucid.Html ()
notFoundPage mGoogleAnalyticsId streamConfig mFlash =
  Frame.template mGoogleAnalyticsId streamConfig Nothing mFlash [] $
    notFoundContent "Page"
