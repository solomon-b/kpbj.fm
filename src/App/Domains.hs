-- | Centralized domain configuration for the KPBJ application.
--
-- This module provides all domain-related configuration derived from the
-- environment. All domain strings are defined here to ensure consistency
-- across cookies, CORS, uploads, and other cross-origin features.
--
-- == Domain Structure
--
-- - Development: localhost
-- - Staging: staging.kpbj.fm (with uploads.staging.kpbj.fm for uploads)
-- - Production: kpbj.fm (with www.kpbj.fm main site, uploads.kpbj.fm for uploads)
module App.Domains
  ( -- * Base Domain
    baseDomain,

    -- * Cookie Configuration
    cookieName,
    cookieDomain,

    -- * Upload Configuration
    uploadsDomain,
    uploadsBaseUrl,
    audioUploadUrl,

    -- * Allowed Origins (for CORS)
    allowedOrigins,
  )
where

--------------------------------------------------------------------------------

import App.Config (Environment (..))
import Data.Text (Text)
import Domain.Types.Origin (Origin)

--------------------------------------------------------------------------------
-- Base Domain

-- | Get the base domain for the environment (without scheme or subdomains).
--
-- - Development: empty (no domain, uses localhost)
-- - Staging: "staging.kpbj.fm"
-- - Production: "kpbj.fm"
baseDomain :: Environment -> Text
baseDomain = \case
  Development -> ""
  Staging -> "staging.kpbj.fm"
  Production -> "kpbj.fm"

--------------------------------------------------------------------------------
-- Cookie Configuration

-- | Get the session cookie name for the environment.
--
-- Different environments use different cookie names to prevent collisions
-- when cookies from production (Domain=.kpbj.fm) would otherwise be sent
-- to staging (staging.kpbj.fm is a subdomain of kpbj.fm).
--
-- - Development: "session-id"
-- - Staging: "session-id-staging"
-- - Production: "session-id-production"
cookieName :: Environment -> Text
cookieName = \case
  Development -> "session-id"
  Staging -> "session-id-staging"
  Production -> "session-id-production"

-- | Get the cookie Domain attribute for cross-subdomain authentication.
--
-- The leading dot allows the cookie to be shared across all subdomains:
-- - Development: empty (no Domain attribute, restricted to exact host)
-- - Staging: ".staging.kpbj.fm" (covers staging.kpbj.fm, uploads.staging.kpbj.fm)
-- - Production: ".kpbj.fm" (covers www.kpbj.fm, uploads.kpbj.fm)
cookieDomain :: Environment -> Text
cookieDomain = \case
  Development -> ""
  env -> "." <> baseDomain env

--------------------------------------------------------------------------------
-- Upload Configuration

-- | Get the uploads subdomain for the environment.
--
-- - Development: empty (relative URLs)
-- - Staging: "uploads.staging.kpbj.fm"
-- - Production: "uploads.kpbj.fm"
uploadsDomain :: Environment -> Text
uploadsDomain = \case
  Development -> ""
  env -> "uploads." <> baseDomain env

-- | Get the base URL for uploads (scheme + domain).
--
-- - Development: empty (use relative URLs)
-- - Staging: "https://uploads.staging.kpbj.fm"
-- - Production: "https://uploads.kpbj.fm"
uploadsBaseUrl :: Environment -> Text
uploadsBaseUrl = \case
  Development -> ""
  env -> "https://" <> uploadsDomain env

-- | Get the full URL for audio uploads.
--
-- - Development: "/api/uploads/audio" (relative)
-- - Staging: "https://uploads.staging.kpbj.fm/api/uploads/audio"
-- - Production: "https://uploads.kpbj.fm/api/uploads/audio"
audioUploadUrl :: Environment -> Text
audioUploadUrl env = uploadsBaseUrl env <> "/api/uploads/audio"

--------------------------------------------------------------------------------
-- CORS Configuration

-- | Get the list of allowed origins for CORS in the given environment.
--
-- These are the origins that are permitted to make cross-origin requests
-- with credentials.
allowedOrigins :: Environment -> [Origin]
allowedOrigins = \case
  Development ->
    [ "http://localhost:4000"
    ]
  Staging ->
    [ "https://staging.kpbj.fm",
      "https://www.staging.kpbj.fm",
      "https://uploads.staging.kpbj.fm"
    ]
  Production ->
    [ "https://kpbj.fm",
      "https://www.kpbj.fm",
      "https://uploads.kpbj.fm"
    ]
