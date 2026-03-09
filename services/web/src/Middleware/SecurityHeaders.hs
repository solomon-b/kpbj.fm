-- | WAI middleware that adds security headers to all responses.
--
-- Covers the headers checked by Mozilla Observatory:
-- Strict-Transport-Security, Content-Security-Policy,
-- X-Content-Type-Options, X-Frame-Options, Referrer-Policy,
-- and Permissions-Policy.
module Middleware.SecurityHeaders
  ( securityHeadersMiddleware,
  )
where

--------------------------------------------------------------------------------

import Network.HTTP.Types.Header qualified as Header
import Network.Wai qualified as Wai

--------------------------------------------------------------------------------

-- | Middleware that adds security headers to every response.
securityHeadersMiddleware :: Wai.Middleware
securityHeadersMiddleware app req respond =
  app req $ \response ->
    respond $ Wai.mapResponseHeaders (securityHeaders <>) response

-- | Security headers added to all responses.
securityHeaders :: [Header.Header]
securityHeaders =
  [ ("Strict-Transport-Security", "max-age=63072000; includeSubDomains; preload"),
    ("X-Content-Type-Options", "nosniff"),
    ("X-Frame-Options", "DENY"),
    ("Referrer-Policy", "strict-origin-when-cross-origin"),
    ("Permissions-Policy", "camera=(), microphone=(), geolocation=()"),
    ( "Content-Security-Policy",
      "default-src 'none'; \
      \script-src 'self' 'unsafe-inline' 'unsafe-eval' https://www.googletagmanager.com https://cdn.tailwindcss.com https://cdnjs.cloudflare.com https://www.paypal.com; \
      \style-src 'self' 'unsafe-inline' https://cdnjs.cloudflare.com; \
      \img-src 'self' https://*.digitaloceanspaces.com https://www.paypalobjects.com data: blob:; \
      \font-src 'self' https://cdnjs.cloudflare.com; \
      \connect-src 'self' blob: https://uploads.kpbj.fm https://uploads.staging.kpbj.fm https://www.google-analytics.com https://www.googletagmanager.com https://www.paypal.com; \
      \media-src 'self' blob: https://stream.kpbj.fm https://stream.staging.kpbj.fm https://*.digitaloceanspaces.com; \
      \frame-src 'self' https://www.paypal.com; \
      \form-action 'self' https://docs.google.com; \
      \base-uri 'self'; \
      \upgrade-insecure-requests"
    )
  ]
