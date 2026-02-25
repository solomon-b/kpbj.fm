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

import App.Handler.Error (notFoundContent)
import Component.Frame qualified as Frame
import App.CustomContext (StreamConfig)
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId)
import Lucid qualified

--------------------------------------------------------------------------------

-- | Pre-rendered 404 page wrapped in the full site frame.
--
-- Rendered once at startup with no user context (logged-out view).
-- Used by 'Middleware.NotFound' to replace Servant's plain-text 404 responses.
notFoundPage :: Maybe GoogleAnalyticsId -> StreamConfig -> Lucid.Html ()
notFoundPage mGoogleAnalyticsId streamConfig =
  Frame.template mGoogleAnalyticsId streamConfig Nothing $
    notFoundContent "Page"
