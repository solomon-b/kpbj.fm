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
    cookieDomain,
  )
where

--------------------------------------------------------------------------------

import App.Config (Environment (..))
import Data.Text (Text)

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
