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

    -- * Site Base URL
    siteBaseUrl,

    -- * Cookie Configuration
    cookieDomainMaybe,

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
-- Site Base URL

-- | Get the base URL for the site (scheme + domain).
--
-- Used for constructing absolute URLs when needed (e.g., for external services).
--
-- - Development: "http://localhost:4000"
-- - Staging: "https://staging.kpbj.fm"
-- - Production: "https://kpbj.fm"
siteBaseUrl :: Environment -> Text
siteBaseUrl = \case
  Development -> "http://localhost:4000"
  env -> "https://" <> baseDomain env

--------------------------------------------------------------------------------
-- Cookie Configuration

-- | Get the cookie Domain attribute as Maybe (for 'App.Auth.mkCookieSession').
--
-- This returns 'Nothing' for Development (no Domain attribute) and
-- @Just domain@ for other environments.
cookieDomainMaybe :: Environment -> Maybe Text
cookieDomainMaybe = \case
  Development -> Nothing
  env -> Just $ "." <> baseDomain env

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
